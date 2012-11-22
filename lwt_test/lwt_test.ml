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

open Lwt
open OUnit

let ( |> ) a b = b a
let id x = x

let alloc_page () =
	Bigarray.Array1.create Bigarray.char Bigarray.c_layout 4096

let one_request_response () =
	let page = alloc_page () in
	let sring = Ring.of_buf ~buf:page ~idx_size:1 ~name:"test" in
	let front = Ring.Front.init sring in
	let back = Ring.Back.init sring in

	Printf.fprintf stdout "%s\n%!" (Ring.Back.to_string back);
	assert_equal ~msg:"more_to_do" ~printer:string_of_bool false (Ring.Back.more_to_do back);

	let client = Lwt_ring.Client.init front in
	let request_th = Lwt_ring.Client.push_request_and_wait client (fun _ -> ()) in
	Printf.fprintf stdout "%s\n%!" (Ring.Back.to_string back);
	assert_equal ~msg:"more_to_do" ~printer:string_of_bool true (Ring.Back.more_to_do back);

	let finished = ref false in
	Ring.Back.ack_requests back (fun _ -> finished := true);
	assert_equal ~msg:"ack_requests" ~printer:string_of_bool (!finished) true;

	assert_equal ~msg:"more_to_do" ~printer:string_of_bool false (Ring.Back.more_to_do back);
	assert_equal ~msg:"is_sleeping" ~printer:string_of_bool false (Lwt.is_sleeping request_th);
	()


let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test shared memory ring code";

  let suite = "ring" >:::
    [
		"one_request_response" >:: one_request_response
    ] in
  run_test_tt ~verbose:!verbose suite
