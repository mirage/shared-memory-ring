(*
 * Copyright (c) 2013 Citrix Systems, Inc
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
open Lwt

let create name =
    lwt fd = Lwt_unix.openfile name [ Unix.O_CREAT; Unix.O_TRUNC ] 0o0644 in
    Lwt_unix.close fd

let openmem name =
    lwt fd = Lwt_unix.openfile name [ Unix.O_RDWR ] 0o0 in
    try_lwt
        return (Bigarray.Genarray.map_file (Lwt_unix.unix_file_descr fd) Bigarray.char Bigarray.c_layout true [| 4096 |])
    finally
        Lwt_unix.close fd

(*
module EVENTCHN = sig
    type t

    type handle

    val init: unit -> handle
    val unbind: handle -> t -> unit
    val notify: handle -> t -> unit
    val to_int: t -> int
    val alloc_unbound_port: handle -> int -> t
    val bind_interdomain: handle -> int -> int -> t 

end
*)

module Eventchn = struct

    type t = int option * Lwt_unix.file_descr

    let to_int = function
      | None, _ -> -1
      | Some x, _ -> x

    type handle = {
        mutable ports: t list;
    }

    let init () = {
        ports = []
    }

    let unbind h t =
        h.ports <- List.filter (fun t' -> t' <> t) h.ports

    let notify _ (_, fd) =
        let th = Lwt_unix.write fd "!" 0 1 in
        ()

    let path_of_port = Printf.sprintf "eventchn.%d"

    let free_port =
        let counter = ref 0 in
        fun () ->
            let result = !counter in
            incr counter;
            result

    let bind_interdomain h domid port =
        let path = path_of_port port in
        let sock = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        lwt () = Lwt_unix.connect sock (Unix.ADDR_UNIX path) in
        h.ports <- (None, sock) :: h.ports;
        return (None, sock)

    let bind_unbound_port h domid =
        let port = free_port () in
        let path = path_of_port port in
        let sock = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        Lwt_unix.bind sock (Unix.ADDR_UNIX path);
        Lwt_unix.listen sock 5;
        lwt (fd, _) = Lwt_unix.accept sock in
        h.ports <- (Some port, fd) :: h.ports;
        return (Some port, fd)

    let pending h =
        let wait t =
            let buf = String.create 1 in
            lwt _ = Lwt_unix.read (snd t) buf 0 1 in
            return t in
        Lwt.pick (List.map wait h.ports)
end

