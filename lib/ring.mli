(*
 * Copyright (c) 2011 Anil Madhavapeddy <anil@recoil.org>
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

(** Shared ring handling to communicate with other Xen domains. *)

type buf = Cstruct.t

module Rpc : sig

  type sring
  (** Abstract type for a shared ring. *)

  val of_buf : buf:Cstruct.t -> idx_size:int -> name:string -> sring
  (** [of_buf ~buf ~idx_size ~name] is an [sring] constructed from
      [buf], of maximum request/response size [idx_size]. [name] is used for
      pretty-printing. [buf] should be a Cstruct.t comprising pre-allocated
      contiguous I/O pages. *)

  val to_summary_string : sring -> string
  (** [to_summary_string ring] is a printable single-line summary of the
      ring. *)

  (** The front-end of the shared ring, which reads requests and reads
      responses from the remote domain. *)
  module Front : sig

    type ('a,'b) t
    (** Type of a frontend. 'a is the response type, and 'b is the
        request id type (e.g. int or int64). *)

    val init : sring:sring -> ('a,'b) t
    (** [init ~sring] is an initialized frontend attached to shared ring
        [sring]. *)

    val slot : ('a,'b) t -> int -> Cstruct.t
    (** [slot frontend idx] retrieves the request/response slot at [idx]
        as an Cstruct.t. [idx] should be less than [nr_ents]. *)

    val nr_ents : ('a,'b) t -> int
    (** [nr_ents frontend] is the number of slots in the underlying
        shared ring. *)

    val get_free_requests : ('a,'b) t -> int
    (** [get_free_requests frontend] is the number of free request slots
        remaining in [frontend]. *)

    val next_req_id: ('a,'b) t -> int
    (** [next_req_id frontend] advances the ring request producer and
        returns the latest slot id. *)

    val ack_responses : ('a,'b) t -> (Cstruct.t -> unit) -> unit
    (** [ack_response frontend f] reads all the outstanding responses
        from the remote domain, calling [f] on them, and updating the
        response consumer pointer after each individual slot has been
        processed.

        This is the low-level function which is only used if some sort
        of batching of requests is being performed, and normally you
        should use the flow-controlled [poll] that will ack the
        responses and wake up any sleeping threads that were waiting for
        that particular response. *)

    val push_requests : ('a,'b) t -> unit
    (** [push_requests frontend] updates the shared request producer. *)

    val push_requests_and_check_notify : ('a,'b) t -> bool
    (** [push_requests_and_check_notify frontend] updates the shared
        request producer, and returns [true] if an event notification is
        required to wake up the remote domain. *)

    val to_string : ('a, 'b) t -> string
    (** [to_string t] pretty-prints ring metadata *)
  end

  (** The back-end of the shared ring, which reads requests and writes
      responses to the remote domain. *)
  module Back : sig

    type ('a,'b) t
    (** Type of a backend. 'a is the response type, and 'b is the
        request id type (e.g. int or int64). *)

    val init : sring:sring -> ('a,'b) t
    (** [init ~sring] is an initialized backend attached to shared ring
        [sring]. *)

    val slot : ('a,'b) t -> int -> Cstruct.t
    (** [slot backend idx] retrieves the request/response slot at [idx]
        as an Cstruct.t. [idx] should be less than [nr_ents]. *)

    val nr_ents : ('a,'b) t -> int
    (** [nr_ents backend] is the number of slots in the underlying
        shared ring. *)

    val next_res_id: ('a,'b) t -> int
    (** [next_res_id backend] advances the response producer and return the
        latest slot id. *)

    val push_responses : ('a,'b) t -> unit
    (** [push_responses backend] updates the shared response producer. *)

    val push_responses_and_check_notify : ('a,'b) t -> bool
    (** [push_responses_and_check_notify backend] updates the shared
        response producer, and returns [true] if an event notification
        is required to wake up the remote domain. *)

    val more_to_do : ('a, 'b) t -> bool
    (** [more_to_do backend] returns [true] if there are outstanding
        requests on the ring which we should immediately process without
        waiting for an event notification. *)

    val ack_requests : ('a, 'b) t -> (Cstruct.t -> unit) -> unit
    (** [ack_requests t fn] applies [fn slot] to each [slot] containing
        a new request. *)

    val to_string : ('a, 'b) t -> string
    (** [to_string backend] pretty-prints the ring metadata. *)
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

module Reverse: functor(RW: RW) -> RW

module Pipe: functor(RW: RW) -> sig
  val write_prepare: Cstruct.t -> int32 * Cstruct.t
  (** [write_prepare ring] returns [(seq,buffer)] where [buffer]
      is the next available contiguous free space area, and [seq]
      is the sequence number of the first byte of [buffer] *)

  val write_commit: Cstruct.t -> int32 -> unit
  (** [write_commit ring seq] signals that data up to [seq] should
      be exposed to the other end. *)

  val read_prepare: Cstruct.t -> int32 * Cstruct.t
  (** [read_prepare ring] returns [(seq,buffer]) where [buffer]
      is the next available contiguous chunk of readable data,
      and [seq] is the sequence number of the first byte of [buffer] *)

  val read_commit: Cstruct.t -> int32 -> unit
  (** [read_commit ring seq] signals that data up to [seq] has
      been processed and may be overwritten with fresh data. *)

  val unsafe_write: Cstruct.t -> string -> int -> int -> int
  val unsafe_read: Cstruct.t -> string -> int -> int -> int
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

val zero: Cstruct.t -> unit
(** [zero c] sets every byte of the [c] Cstruct to zero. *)

val unsafe_load_uint32: Cstruct.t -> int -> int

val unsafe_save_uint32: Cstruct.t -> int -> int -> unit
