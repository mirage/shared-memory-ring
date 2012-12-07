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

type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let sub t off len = Bigarray.Array1.sub t off len

let length t = Bigarray.Array1.dim t

external memory_barrier: unit -> unit = "caml_memory_barrier" "noalloc"
external write_memory_barrier: unit -> unit = "caml_write_memory_barrier" "noalloc"

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
  (* initialise the *_event fields to 1, and the rest to 0 *)
  set_ring_hdr_req_prod ring 0l;
  set_ring_hdr_req_event ring 1l;
  set_ring_hdr_rsp_prod ring 0l;
  set_ring_hdr_rsp_event ring 1l;
  set_ring_hdr_stuff ring 0L

type sring = {
  buf: buf;         (* Overall I/O buffer *)
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

external sring_rsp_prod: sring -> int = "caml_sring_rsp_prod" "noalloc"
external sring_req_prod: sring -> int = "caml_sring_req_prod" "noalloc"
external sring_req_event: sring -> int = "caml_sring_req_event" "noalloc"
external sring_rsp_event: sring -> int = "caml_sring_rsp_event" "noalloc"
external sring_push_requests: sring -> int -> unit = "caml_sring_push_requests" "noalloc"
external sring_push_responses: sring -> int -> unit = "caml_sring_push_responses" "noalloc"
external sring_set_rsp_event: sring -> int -> unit = "caml_sring_set_rsp_event" "noalloc"
external sring_set_req_event: sring -> int -> unit = "caml_sring_set_req_event" "noalloc"

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
	val get_ring_input: buf -> buf
	val get_ring_input_cons: buf -> int32
	val get_ring_input_prod: buf -> int32
	val set_ring_input_cons: buf -> int32 -> unit
	val set_ring_input_prod: buf -> int32 -> unit

	val get_ring_output: buf -> buf
	val get_ring_output_cons: buf -> int32
	val get_ring_output_prod: buf -> int32
	val set_ring_output_cons: buf -> int32 -> unit
	val set_ring_output_prod: buf -> int32 -> unit
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
	let unsafe_write t buf len =
		let output = RW.get_ring_output t in
		let cons = Int32.to_int (RW.get_ring_output_cons t) in
		let prod = ref (Int32.to_int (RW.get_ring_output_prod t)) in
		memory_barrier ();
		let sent = ref 0 in
		while !sent < len && (!prod - cons < (length output)) do
			Bigarray.Array1.unsafe_set output (!prod mod (length output)) buf.[!sent];
			incr prod;
			incr sent;
		done;
		write_memory_barrier ();
		RW.set_ring_output_prod t (Int32.of_int !prod);
		!sent

	let unsafe_read t buf len =
		let input = RW.get_ring_input t in
		let cons = ref (Int32.to_int (RW.get_ring_input_cons t)) in
		let prod = Int32.to_int (RW.get_ring_input_prod t) in
		let pos = ref 0 in
		memory_barrier ();
		while (!pos < len && !cons < prod) do
			buf.[!pos] <- Bigarray.Array1.unsafe_get input (!cons mod (length input));
			incr pos;
			incr cons;
		done;
		memory_barrier (); (* XXX: not a write_memory_barrier? *)
		RW.set_ring_input_cons t (Int32.of_int !cons);
		!pos
end

module type Bidirectional_byte_stream = sig
	type t
	val of_buf: buf -> t

	module Front : sig
		val unsafe_write: t -> string -> int -> int
		val unsafe_read: t -> string -> int -> int
	end
	module Back : sig
		val unsafe_write: t -> string -> int -> int
		val unsafe_read: t -> string -> int -> int		
	end
end

module Xenstore = struct
	type t = buf
	let of_buf t = t
	module Layout = struct
		(* memory layout from the frontend's point of view *)
		cstruct ring {
			uint8_t output[1024];
			uint8_t input[1024];
			uint32_t output_cons;
			uint32_t output_prod;
			uint32_t input_cons;
			uint32_t input_prod
		} as little_endian
	end
	module Front = Pipe(Layout)
	module Back = Pipe(Reverse(Layout))
end

module Console = struct
	type t = buf
	let of_buf t = t
	module Layout = struct
		(* memory layout from the frontend's point of view *)
		cstruct ring {
			uint8_t input[1024];
			uint8_t output[2048];
			uint32_t input_cons;
			uint32_t input_prod;
			uint32_t output_cons;
			uint32_t output_prod
		} as little_endian
	end
	module Front = Pipe(Layout)
	module Back = Pipe(Reverse(Layout))
end

(* Raw ring handling section *)
(* TODO both of these can be combined into one set of bindings now *)
module C_Console = struct
    type t = buf
    let of_buf t = t
    external zero: t -> unit = "caml_console_ring_init"
    external unsafe_write: t -> string -> int -> int = "caml_console_ring_write"
    external unsafe_read: t -> string -> int -> int = "caml_console_ring_read"
	module Back = struct
		external unsafe_write : t -> string -> int -> int = "caml_console_back_ring_write"
		external unsafe_read : t -> string -> int -> int = "caml_console_back_ring_read"
	end
end

module C_Xenstore = struct
    type t = buf
    let of_buf t = t
    external zero: t -> unit = "caml_xenstore_ring_init"
    external unsafe_write: t -> string -> int -> int = "caml_xenstore_ring_write"
    external unsafe_read: t -> string -> int -> int = "caml_xenstore_ring_read"
	module Back = struct
		external unsafe_write : t -> string -> int -> int = "caml_xenstore_back_ring_write"
		external unsafe_read : t -> string -> int -> int = "caml_xenstore_back_ring_read"
	end
end

