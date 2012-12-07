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

open OUnit

let ( |> ) a b = b a
let id x = x

let alloc_page () =
	Bigarray.Array1.create Bigarray.char Bigarray.c_layout 4096
let length t = Bigarray.Array1.dim t

let compare_bufs a b =
	assert_equal ~printer:string_of_int (length a) (length b);
	for i = 0 to length a - 1 do
		let x = Bigarray.Array1.unsafe_get a i in
		let y = Bigarray.Array1.unsafe_get b i in
		assert_equal ~printer:(fun c -> Printf.sprintf "%02x" (int_of_char c)) x y
	done

let with_xenstores f =
	let b1 = alloc_page () in
	let b2 = alloc_page () in
	let a = Ring.Xenstore.of_buf b1 in
	let b = Ring.C_Xenstore.of_buf b2 in
	f b1 b2 a b

let xenstore_init () =
	with_xenstores
		(fun b1 b2 _ _ ->
			compare_bufs b1 b2
		)

let xenstore_hello () =
	let msg = "hello" in
	let buf = String.make 16 '\000' in
	with_xenstores
		(fun b1 b2 a b ->
			let x = Ring.Xenstore.Front.unsafe_write a msg (String.length msg) in
			let y = Ring.C_Xenstore.unsafe_write b msg (String.length msg) in
			assert_equal ~printer:string_of_int x y;
			compare_bufs b1 b2;
			let x = Ring.Xenstore.Back.unsafe_read a buf (String.length buf) in
			assert_equal ~printer:string_of_int x (String.length msg);
			assert_equal (String.sub buf 0 x) msg;
			let x = Ring.C_Xenstore.Back.unsafe_read b buf (String.length buf) in
			assert_equal ~printer:string_of_int x (String.length msg);
			assert_equal (String.sub buf 0 x) msg;
			()
		)

let with_consoles f =
	let b1 = alloc_page () in
	let b2 = alloc_page () in
	let a = Ring.Console.of_buf b1 in
	let b = Ring.C_Console.of_buf b2 in
	f b1 b2 a b

let console_init () =
	with_consoles
		(fun b1 b2 _ _ ->
			compare_bufs b1 b2
		)

let console_hello () =
	let msg = "hello" in
	let buf = String.make 16 '\000' in
	with_consoles
		(fun b1 b2 a b ->
			let x = Ring.Console.Front.unsafe_write a msg (String.length msg) in
			let y = Ring.C_Console.unsafe_write b msg (String.length msg) in
			assert_equal ~printer:string_of_int x y;
			compare_bufs b1 b2;
			let x = Ring.Console.Back.unsafe_read a buf (String.length buf) in
			assert_equal ~printer:string_of_int x (String.length msg);
			assert_equal (String.sub buf 0 x) msg;
			let x = Ring.C_Console.Back.unsafe_read b buf (String.length buf) in
			assert_equal ~printer:string_of_int x (String.length msg);
			assert_equal (String.sub buf 0 x) msg;
			()
		)

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Test shared memory ring code";

  let suite = "ring" >:::
    [
		"xenstore_init" >:: xenstore_init;
		"xenstore_hello" >:: xenstore_hello;
		"console_init" >:: console_init;
		"console_hello" >:: console_hello;
    ] in
  run_test_tt ~verbose:!verbose suite
