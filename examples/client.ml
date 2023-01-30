(*
 * Copyright (C) Citrix Systems Inc.
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

let ( |> ) a b = b a
let id x = x
let alloc_page () = Bigarray.Array1.create Bigarray.char Bigarray.c_layout 4096

let one_request_response () =
  let page = alloc_page () in
  let sring =
    Ring.Rpc.of_buf ~buf:(Cstruct.of_bigarray page) ~idx_size:1 ~name:"test"
  in
  let front = Ring.Rpc.Front.init ~sring in
  let back = Ring.Rpc.Back.init ~sring in

  Printf.fprintf stdout "%s\n%!" (Ring.Rpc.Back.to_string back);

  let client = Lwt_ring.Front.init string_of_int front in
  let server = Lwt_ring.Back.init string_of_int back in

  let id = 0 in
  let must_notify = ref false in
  let request_th =
    Lwt_ring.Front.push_request_and_wait client
      (fun () -> must_notify := true)
      (fun _ -> id)
  in
  assert !must_notify;
  Printf.fprintf stdout "%s\n%!" (Ring.Rpc.Back.to_string back);

  let finished = ref false in
  Ring.Rpc.Back.ack_requests back (fun _ -> finished := true);

  assert !finished;
  Lwt_ring.Back.push_response server (fun () -> ()) (fun _ -> ());

  Printf.fprintf stdout "%s\n%!" (Ring.Rpc.Back.to_string back);

  let replied = ref false in
  Lwt_ring.Front.poll client (fun _ ->
      replied := true;
      (id, ()));

  assert !replied;

  let p = Lwt.choose [ Lwt_unix.sleep 5.; request_th ] in
  assert (not @@ Lwt.is_sleeping request_th);
  p

let () = Lwt_main.run (one_request_response ())
