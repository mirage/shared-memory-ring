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

(** Shared ring handling to communicate with other Xen domains *)

type buf = Cstruct.t

module Rpc : sig

(** Abstract type for a shared ring *)
type sring

(** Given a buf [buf] comprising pre-allocated contiguous
    I/O pages, return an [sring] where the maximum size of each
    request/response is {[idx_size]}.
    @param buf pre-allocated contiguous I/O pages
    @param idx_size maximum size of each slot, in bytes
    @param name Name of the shared ring, for pretty-printing
    @return shared ring value
  *)
val of_buf : buf:buf -> idx_size:int -> name:string -> sring

(** The front-end of the shared ring, which issues requests and reads
    responses from the remote domain. 
  *)
module Front : sig

  (** 'a is the response type, and 'b is the request id type (e.g. int or int64) *)
  type ('a,'b) t

  (** Given a shared ring, initialise it for this front-end module
    * @param sring Shared ring to attach this front end to
    * @return front end ring value
    *)
  val init : sring:sring -> ('a,'b) t

  (** Retrieve the request/response slot at the specified index as
    * an buf.
    * @param idx Index to retrieve, should be less than nr_ents
    *)
  val slot : ('a,'b) t -> int -> buf

  (** Retrieve number of slots in the shared ring *)
  val nr_ents : ('a,'b) t -> int

  (** Retrieve the number of free request slots remaining *)
  val get_free_requests : ('a,'b) t -> int

  (** Advance the request producer and return the latest slot id *)
  val next_req_id: ('a,'b) t -> int

  (** Read all the outstanding responses from the remote domain,
    * calling {[fn]} on them, and updating the response
    * consumer pointer after each individual slot has been processed.
    *
    * This is the low-level function which is only used if some
    * sort of batching of requests is being performed, and normally
    * you should use the flow-controlled {[poll]} that will ack
    * the responses and wake up any sleeping threads that were
    * waiting for that particular response.
    *)
  val ack_responses : ('a,'b) t -> (buf -> unit) -> unit

  (** Update the shared request producer *)
  val push_requests : ('a,'b) t -> unit

  (** Update the shared request producer, and also check to see
      if an event notification is required to wake up the remote
      domain.
      @return true if an event channel notification is required
    *)
  val push_requests_and_check_notify : ('a,'b) t -> bool
end

module Back : sig
  (** 'a is the response type, and 'b is the request id type (e.g. int or int64) *)
  type ('a,'b) t

  (** Given a shared ring, initialise it for this backend module
    * @param sring Shared ring to attach this backend to
    * @return backend ring value
    *)
  val init : sring:sring -> ('a,'b) t

  (** Retrieve the request/response slot at the specified index as
    * a Io_page.
    * @param idx Index to retrieve, should be less than nr_ents
    *)
  val slot : ('a,'b) t -> int -> buf

  (** Retrieve number of slots in the shared ring *)
  val nr_ents : ('a,'b) t -> int

  (** Advance the response producer and return the latest slot id *)
  val next_res_id: ('a,'b) t -> int

  (** Update the shared response producer *)
  val push_responses : ('a,'b) t -> unit

  (** Update the shared response producer, and also check to see
      if an event notification is required to wake up the remote
      domain.
      @return true if an event channel notification is required
    *)
  val push_responses_and_check_notify : ('a,'b) t -> bool

  (** returns true if there are outstanding requests on the ring
      which we should immediately process without waiting for an
      event notification. *)
  val more_to_do : ('a, 'b) t -> bool

  (** [ack_requests t fn] applies [fn slot] to each [slot] containing
      a new request *)
  val ack_requests : ('a, 'b) t -> (buf -> unit) -> unit

  (** pretty-print ring metadata *)
  val to_string : ('a, 'b) t -> string
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

module Reverse: functor(RW: RW) -> RW

module Pipe: functor(RW: RW) -> sig
	val unsafe_write: buf -> buf -> int
	val unsafe_read: buf -> buf -> int
end

module type Bidirectional_byte_stream = sig
	type t
	val of_buf: buf -> t
	val to_debug_string: t -> string
	module Front : sig
		val unsafe_write: t -> buf -> int
		val unsafe_read: t -> buf -> int
	end
	module Back : sig
		val unsafe_write: t -> buf -> int
		val unsafe_read: t -> buf -> int
	end
end

module Console : Bidirectional_byte_stream
module Xenstore : Bidirectional_byte_stream
