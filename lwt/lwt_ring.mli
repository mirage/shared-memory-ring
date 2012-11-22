(*
 * Copyright (c) 2011 Anil Madhavapeddy <anil@recoil.org>
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

(** Lwt interface to shared memory ring *)

open Ring

(** The client front-end connection to the shared ring *)
module Client : sig

  (** 'a is the response type, and 'b is the request id type (e.g. int or int64) *)
  type ('a,'b) t

  (** Given a shared ring, initialise an lwt client
    * @param ring Shared ring frontend to attach to
    * @return stateful ring client
    *)
  val init : ('a, 'b) Ring.Front.t -> ('a,'b) t

  (** Push an asynchronous request to the slot and call [freefn] when a response comes in *)
  val push_request_async : ('a,'b) t -> (buf -> 'b) -> (unit -> unit) -> unit Lwt.t 

  (** Given a function {[fn]} which writes to a slot and returns
      the request id, this will wait for a free request slot,
      write the request, and return with the response when it
      is available.
      @param fn Function that writes to a request slot and returns the request id
      @return Thread which returns the response value to the input request
    *)
  val push_request_and_wait : ('a,'b) t -> (buf -> 'b) -> 'a Lwt.t

  (** Poll the ring for responses, and wake up any threads that are
      sleeping (as a result of calling {[push_request_and_wait]}).
	  This can be called regularly, or triggered via some external event
      such as an event channel signal.
    *)
  val poll : ('a,'b) t -> (buf -> ('b * 'a)) -> unit
end

(** The server back-end connection to the shared ring *)
module Server : sig

  (** 'a is the response type, and 'b is the request id type (e.g. int or int64) *)
  type ('a,'b) t

  (** Given a shared ring, initialise an lwt server
    * @param ring Shared ring frontend to attach to
    * @return stateful ring server
    *)
  val init : ('a, 'b) Ring.Back.t -> ('a,'b) t

  (** [push_response t fn] finds a free slot and applies it to [fn],
      signalling the client that a response is ready. *)
  val push_response : ('a, 'b) t -> (buf -> unit) -> unit

end
