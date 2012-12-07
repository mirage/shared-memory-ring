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

let block =
	let buf = Bigarray.Array1.create Bigarray.char Bigarray.c_layout (15 * 1024 * 1024) in
	let counter = ref 0l in
	for i = 0 to Bigarray.Array1.dim buf / 4 - 1 do
		Cstruct.LE.set_uint32 buf (i * 4) !counter;
		counter := Int32.add 1l !counter;
	done;
	buf

let bigarray_to_string a =
	let s = String.make (Bigarray.Array1.dim a) '\000' in
	for i = 0 to Bigarray.Array1.dim a - 1 do
		s.[0] <- Bigarray.Array1.unsafe_get a i
	done;
	s

let throughput_test ~use_ocaml ~write_chunk_size ~read_chunk_size () =
	with_consoles
		(fun b1 b2 a b ->
			let read_chunk = String.make read_chunk_size '\000' in
			let block = bigarray_to_string block in
			let producer = ref 0 in
			let consumed = ref 0 in
			let length = String.length block in
			let start = Unix.gettimeofday () in
			while !producer < length do
				let remaining = length - !producer in
				let can_write = min write_chunk_size remaining in
				let chunk = String.sub block !producer can_write in
				let written =
					if use_ocaml
					then Ring.Console.Front.unsafe_write a chunk can_write
					else Ring.C_Console.unsafe_write b chunk can_write in
				producer := !producer + written;
				let read =
					if use_ocaml
						then Ring.Console.Back.unsafe_read a read_chunk read_chunk_size
					else Ring.C_Console.Back.unsafe_read b read_chunk read_chunk_size in
				consumed := !consumed + read;
				assert ((written <> 0) || (read <> 0))
			done;
			let duration = Unix.gettimeofday () -. start in
			assert_equal ~msg:"transferred" ~printer:string_of_int length !consumed;
			Printf.fprintf stderr "%s read(%d) write(%d): %.2f MiB/sec\n"
				(if use_ocaml then "OCaml" else "C")
				read_chunk_size write_chunk_size
				(float_of_int !consumed /. duration /. (1024. *. 1024.))
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
		"ocaml throughput_test" >:: throughput_test ~use_ocaml:true ~read_chunk_size:1024 ~write_chunk_size:1024;
		"C throughput_test" >:: throughput_test ~use_ocaml:false ~read_chunk_size:1024 ~write_chunk_size:1024;
    ] in
  run_test_tt ~verbose:!verbose suite
