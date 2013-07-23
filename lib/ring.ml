(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2012 Citrix Systems, Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Printf

type buf = Cstruct.t

let sub t off len = Cstruct.sub t off len

let length t = Cstruct.len t

external memory_barrier: unit -> unit = "caml_memory_barrier" "noalloc"

module Rpc = struct

let rec pow2 = function
  | 0 -> 1
  | n -> 2 * (pow2 (n - 1))

(*
  struct sring {
    RING_IDX req_prod, req_event;
    RING_IDX rsp_prod, rsp_event;
    uint8_t  netfront_smartpoll_active;
    uint8_t  pad[47];
  };
*)

cstruct ring_hdr {
  uint32_t req_prod;
  uint32_t req_event;
  uint32_t rsp_prod;
  uint32_t rsp_event;
  uint64_t stuff
} as little_endian

let initialise ring =
  for i = 0 to Cstruct.len ring - 1 do
    Cstruct.set_uint8 ring i 0
  done;
  (* initialise the *_event fields to 1, and the rest to 0 *)
  set_ring_hdr_req_prod ring 0l;
  set_ring_hdr_req_event ring 1l;
  set_ring_hdr_rsp_prod ring 0l;
  set_ring_hdr_rsp_event ring 1l;
  set_ring_hdr_stuff ring 0L

type sring = {
  buf: Cstruct.t;         (* Overall I/O buffer *)
  header_size: int; (* Header of shared ring variables, in bits *)
  idx_size: int;    (* Size in bits of an index slot *)
  nr_ents: int;     (* Number of index entries *)
  name: string;     (* For pretty printing only *)
}

let of_buf ~buf ~idx_size ~name =
  let header_size = 4+4+4+4+48 in (* header bytes size of struct sring *)
  (* Round down to the nearest power of 2, so we can mask indices easily *)
  let round_down_to_nearest_2 x =
    int_of_float (2. ** (floor ( (log (float x)) /. (log 2.)))) in
  (* Free space in shared ring after header is accounted for *)
  let free_bytes = length buf - header_size in
  let nr_ents = round_down_to_nearest_2 (free_bytes / idx_size) in
  { name; buf; idx_size; nr_ents; header_size }

let to_summary_string t =
  Printf.sprintf "ring %s header_size = %d; index slot size = %d; number of entries = %d" t.name t.header_size t.idx_size t.nr_ents

let sring_rsp_prod sring = Int32.to_int (get_ring_hdr_rsp_prod sring.buf)
let sring_req_prod sring = Int32.to_int (get_ring_hdr_req_prod sring.buf)
let sring_req_event sring =
	memory_barrier ();
	Int32.to_int (get_ring_hdr_req_event sring.buf)
let sring_rsp_event sring =
	memory_barrier ();
	Int32.to_int (get_ring_hdr_rsp_event sring.buf)

let sring_push_requests sring req_prod =
	memory_barrier (); (* ensure requests are seen before the index is updated *)
	set_ring_hdr_req_prod sring.buf (Int32.of_int req_prod)

let sring_push_responses sring rsp_prod =
	memory_barrier (); (* ensure requests are seen before the index is updated *)
	set_ring_hdr_rsp_prod sring.buf (Int32.of_int rsp_prod)

let sring_set_rsp_event sring rsp_cons =
	set_ring_hdr_rsp_event sring.buf (Int32.of_int rsp_cons);
	memory_barrier ()

let sring_set_req_event sring req_cons =
	set_ring_hdr_req_event sring.buf (Int32.of_int req_cons);
	memory_barrier ()

let nr_ents sring = sring.nr_ents

let slot sring idx =
  (* TODO should precalculate these and store in the sring? this is fast-path *)
  let idx = idx land (sring.nr_ents - 1) in
  let off = sring.header_size + (idx * sring.idx_size) in
  sub sring.buf off sring.idx_size

module Front = struct

  type ('a,'b) t = {
    mutable req_prod_pvt: int;
    mutable rsp_cons: int;
    sring: sring;
  }

  let init ~sring =
    let req_prod_pvt = 0 in
    let rsp_cons = 0 in
    (* The frontend has to initialise the ring contents *)
    initialise sring.buf;
    { req_prod_pvt; rsp_cons; sring }

  let slot t idx = slot t.sring idx
  let nr_ents t = t.sring.nr_ents

  let get_free_requests t =
    t.sring.nr_ents - (t.req_prod_pvt - t.rsp_cons)

  let is_ring_full t =
    get_free_requests t = 0

  let has_unconsumed_responses t =
    ((sring_rsp_prod t.sring) - t.rsp_cons) > 0

  let push_requests t =
    sring_push_requests t.sring t.req_prod_pvt

  let push_requests_and_check_notify t =
    let old_idx = sring_req_prod t.sring in
    let new_idx = t.req_prod_pvt in
    push_requests t;
    (new_idx - (sring_req_event t.sring)) < (new_idx - old_idx)

  let check_for_responses t =
    if has_unconsumed_responses t then
      true
    else begin
      sring_set_rsp_event t.sring (t.rsp_cons + 1);
      has_unconsumed_responses t
    end 

  let next_req_id t =
    let s = t.req_prod_pvt in
    t.req_prod_pvt <- t.req_prod_pvt + 1;
    s

  let rec ack_responses t fn =
    let rsp_prod = sring_rsp_prod t.sring in
    while t.rsp_cons != rsp_prod do
      let slot_id = t.rsp_cons in
      let slot = slot t slot_id in
      fn slot;
      t.rsp_cons <- t.rsp_cons + 1;
    done;
    if check_for_responses t then ack_responses t fn

end

module Back = struct

  type ('a,'b) t = {
    mutable rsp_prod_pvt: int;
    mutable req_cons: int;
    sring: sring;
  }

  let init ~sring =
    let rsp_prod_pvt = 0 in
    let req_cons = 0 in
    { rsp_prod_pvt; req_cons; sring }

  let slot t idx = slot t.sring idx

  let nr_ents t = t.sring.nr_ents
 
  let has_unconsumed_requests t =
    let req = (sring_req_prod t.sring) - t.req_cons in
    let rsp = t.sring.nr_ents - (t.req_cons - t.rsp_prod_pvt) in
    if req < rsp then (req > 0) else (rsp > 0)
 
  let push_responses t =
    sring_push_responses t.sring t.rsp_prod_pvt 

  let push_responses_and_check_notify t =
    let old_idx = sring_rsp_prod t.sring in
    let new_idx = t.rsp_prod_pvt in
    push_responses t;
    (new_idx - (sring_rsp_event t.sring)) < (new_idx - old_idx)

  let check_for_requests t =
    if has_unconsumed_requests t then
      true
    else begin
      sring_set_req_event t.sring (t.req_cons + 1);
      has_unconsumed_requests t
    end

  let next_res_id t =
    let s = t.rsp_prod_pvt in
    t.rsp_prod_pvt <- t.rsp_prod_pvt + 1;
    s

  let next_slot t =
      slot t (next_res_id t)

  let final_check_for_requests t =
	  has_unconsumed_requests t ||
		  begin
			  sring_set_req_event t.sring (t.req_cons + 1);
			  has_unconsumed_requests t
		  end

  let more_to_do t =
	  if t.rsp_prod_pvt = t.req_cons then
		  final_check_for_requests t
	  else
		  has_unconsumed_requests t

  let to_string t =
	  let req_prod = sring_req_prod t.sring in
	  let rsp_prod = sring_rsp_prod t.sring in
	  let req_event = sring_req_event t.sring in
	  let rsp_event = sring_rsp_event t.sring in
	  Printf.sprintf "{ req_prod=%d rsp_prod=%d req_event=%d rsp_event=%d rsp_prod_pvt=%d req_cons=%d }" req_prod rsp_prod req_event rsp_event t.rsp_prod_pvt t.req_cons

  let rec ack_requests t fn =
    let req_prod = sring_req_prod t.sring in
    while t.req_cons != req_prod do
      let slot_id = t.req_cons in
      let slot = slot t slot_id in
      t.req_cons <- t.req_cons + 1;
      fn slot;
    done;
    if check_for_requests t then ack_requests t fn
end
end

module type RW = sig
	(** A bi-directional pipe where 'input' and 'output' are from
	    the frontend's (i.e. the guest's) point of view *)
	val get_ring_input: Cstruct.t -> Cstruct.t
	val get_ring_input_cons: Cstruct.t -> int32
	val get_ring_input_prod: Cstruct.t -> int32
	val set_ring_input_cons: Cstruct.t -> int32 -> unit
	val set_ring_input_prod: Cstruct.t -> int32 -> unit

	val get_ring_output: Cstruct.t -> Cstruct.t
	val get_ring_output_cons: Cstruct.t -> int32
	val get_ring_output_prod: Cstruct.t -> int32
	val set_ring_output_cons: Cstruct.t -> int32 -> unit
	val set_ring_output_prod: Cstruct.t -> int32 -> unit
end

module Reverse(RW: RW) = struct
	let get_ring_input = RW.get_ring_output
	let get_ring_input_cons = RW.get_ring_output_cons
	let get_ring_input_prod = RW.get_ring_output_prod
	let set_ring_input_cons = RW.set_ring_output_cons
	let set_ring_input_prod = RW.set_ring_output_prod

	let get_ring_output = RW.get_ring_input
	let get_ring_output_cons = RW.get_ring_input_cons
	let get_ring_output_prod = RW.get_ring_input_prod
	let set_ring_output_cons = RW.set_ring_input_cons
	let set_ring_output_prod = RW.set_ring_input_prod
end

module Pipe(RW: RW) = struct
	let unsafe_write t buf ofs len =
		let output = RW.get_ring_output t in
		let output_length = length output in
		(* Remember: the producer and consumer indices can be >> output_length *)
		let cons = Int32.to_int (RW.get_ring_output_cons t) in
		let prod = Int32.to_int (RW.get_ring_output_prod t) in
		memory_barrier ();
		(* 0 <= cons', prod' <= output_length *)
		let cons' =
			let x = cons mod output_length in
			if x < 0 then x + output_length else x
		and prod' =
			let x = prod mod output_length in
			if x < 0 then x + output_length else x in
		let free_space =
			if prod - cons >= output_length
			then 0
			else
				if prod' >= cons'
				then output_length - prod' (* in this write, fill to the end *)
				else cons' - prod' in
		let can_write = min len free_space in
		Cstruct.blit_from_string buf ofs output prod' can_write;
		memory_barrier ();
		RW.set_ring_output_prod t (Int32.of_int (prod + can_write));
		can_write

	let unsafe_read t buf ofs len =
		let input = RW.get_ring_input t in
		let input_length = length input in
		let cons = Int32.to_int (RW.get_ring_input_cons t) in
		let prod = Int32.to_int (RW.get_ring_input_prod t) in
		memory_barrier ();
		let cons' =
			let x = cons mod input_length in
			if x < 0 then x + input_length else x
		and prod' =
			let x = prod mod input_length in
			if x < 0 then x + input_length else x in
		let data_available =
			if prod = cons
			then 0
			else
				if prod' > cons'
				then prod' - cons'
				else input_length - cons' in (* read up to the last byte in the ring *)
		let can_read = min len data_available in
		Cstruct.blit_to_string input cons' buf ofs can_read;
		memory_barrier (); (* XXX: not a write_memory_barrier? *)
		RW.set_ring_input_cons t (Int32.of_int (cons + can_read));
		can_read

	let rec repeat f from buf ofs len =
		let n = f from buf ofs len in
		if n < len && n > 0
		then n + (repeat f from buf (ofs + n) (len - n))
		else n

	let unsafe_read = repeat unsafe_read
	let unsafe_write = repeat unsafe_write
end

module type Bidirectional_byte_stream = sig
	val init: Cstruct.t -> unit
	val to_debug_map: Cstruct.t -> (string * string) list

	module Front : sig
		val unsafe_write: Cstruct.t -> string -> int -> int -> int
		val unsafe_read: Cstruct.t -> string -> int -> int -> int
	end
	module Back : sig
		val unsafe_write: Cstruct.t -> string -> int -> int -> int
		val unsafe_read: Cstruct.t -> string -> int -> int -> int
	end
end

let zero t =
	for i = 0 to Cstruct.len t - 1 do
		Cstruct.set_char t i '\000'
	done
