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

let bigarray_to_string a =
	let s = String.make (Bigarray.Array1.dim a) '\000' in
	for i = 0 to Bigarray.Array1.dim a - 1 do
		s.[0] <- Bigarray.Array1.unsafe_get a i
	done;
	s

let string_to_bigarray s =
	let a = Bigarray.Array1.create Bigarray.char Bigarray.c_layout (String.length s) in
	for i = 0 to String.length s - 1 do
		Bigarray.Array1.unsafe_set a i s.[i]
	done;
	a

let with_xenstores f =
	let b1 = alloc_page () in
	let b2 = alloc_page () in
	let a = Xenstore_ring.Ring.of_buf b1 in
	let b = Old_ring.C_Xenstore.of_buf b2 in
	f b1 b2 a b

let xenstore_init () =
	with_xenstores
		(fun b1 b2 _ _ ->
			compare_bufs b1 b2
		)

let xenstore_hello () =
	let msg = "hello" in
	let msg' = string_to_bigarray msg in
	let buf = String.make 16 '\000' in
	let buf' = string_to_bigarray buf in
	with_xenstores
		(fun b1 b2 a b ->
			let x = Xenstore_ring.Ring.Front.unsafe_write a msg' in
			let y = Old_ring.C_Xenstore.unsafe_write b msg (String.length msg) in
			assert_equal ~printer:string_of_int x y;
			compare_bufs b1 b2;
			let x = Xenstore_ring.Ring.Back.unsafe_read a buf' in
			assert_equal ~printer:string_of_int x (String.length msg);
			compare_bufs (Bigarray.Array1.sub buf' 0 x) msg';
			let x = Old_ring.C_Xenstore.Back.unsafe_read b buf (String.length buf) in
			assert_equal ~printer:string_of_int x (String.length msg);
			assert_equal (String.sub buf 0 x) msg;
			()
		)

let with_consoles f =
	let b1 = alloc_page () in
	let b2 = alloc_page () in
	let a = Console_ring.Ring.of_buf b1 in
	let b = Old_ring.C_Console.of_buf b2 in
	f b1 b2 a b

let console_init () =
	with_consoles
		(fun b1 b2 _ _ ->
			compare_bufs b1 b2
		)

let console_hello () =
	let msg = "hello" in
	let msg' = string_to_bigarray msg in
	let buf = String.make 16 '\000' in
	let buf' = string_to_bigarray buf in
	with_consoles
		(fun b1 b2 a b ->
			let x = Console_ring.Ring.Front.unsafe_write a msg' in
			let y = Old_ring.C_Console.unsafe_write b msg (String.length msg) in
			assert_equal ~printer:string_of_int x y;
			compare_bufs b1 b2;
			let x = Console_ring.Ring.Back.unsafe_read a buf' in
			assert_equal ~printer:string_of_int x (String.length msg);
			compare_bufs (Bigarray.Array1.sub buf' 0 x) msg';
			let x = Old_ring.C_Console.Back.unsafe_read b buf (String.length buf) in
			assert_equal ~printer:string_of_int x (String.length msg);
			assert_equal (String.sub buf 0 x) msg;
			()
		)

let block' =
	let buf = Bigarray.Array1.create Bigarray.char Bigarray.c_layout (15 * 1024 * 1024) in
	let counter = ref 0l in
	for i = 0 to Bigarray.Array1.dim buf / 4 - 1 do
		Cstruct.LE.set_uint32 buf (i * 4) !counter;
		counter := Int32.add 1l !counter;
	done;
	buf

let throughput_test ~use_ocaml ~write_chunk_size ~read_chunk_size ~verify () =
	with_consoles
		(fun b1 b2 a b ->
			let read_chunk = String.make read_chunk_size '\000' in
			let block = bigarray_to_string block' in
			let output' = Bigarray.Array1.create Bigarray.char Bigarray.c_layout (Bigarray.Array1.dim block') in
			let output = String.make (String.length block) '\000' in
			let producer = ref 0 in
			let consumed = ref 0 in
			let length = String.length block in
			let start = Unix.gettimeofday () in
			let end_of_transfer = ref false in
			while not(!end_of_transfer) do
				let remaining = length - !producer in
				let can_write = min write_chunk_size remaining in
				let written =
					if use_ocaml
					then Console_ring.Ring.Front.unsafe_write a (Bigarray.Array1.sub block' !producer can_write)
					else Old_ring.C_Console.unsafe_write b (String.sub block !producer can_write) can_write in
				producer := !producer + written;
				let read =
					if use_ocaml
					then Console_ring.Ring.Back.unsafe_read a (Bigarray.Array1.sub output' !consumed (length - !consumed))
					else begin
						let n = Old_ring.C_Console.Back.unsafe_read b read_chunk read_chunk_size in
						begin
							try
								String.blit read_chunk 0 output !consumed n
							with e ->
								Printf.fprintf stderr "String.blit consumed=%d n=%d\n%!" !consumed n;
								Printf.fprintf stderr "%s\n%!" (Console_ring.Ring.(to_debug_string (of_buf b2)));
								raise e
						end;
						n
					end in
				(* verify *)
				if verify then begin
					if use_ocaml
					then compare_bufs (Bigarray.Array1.sub block' !consumed read) (Bigarray.Array1.sub output' !consumed read)
					else assert_equal (String.sub block !consumed read) (String.sub read_chunk 0 read)
				end;
				consumed := !consumed + read;
				end_of_transfer := (written = 0) && (read = 0)
			done;
			let duration = Unix.gettimeofday () -. start in
			assert_equal ~msg:"transferred" ~printer:string_of_int length !consumed;
			assert_equal ~msg:"transferred" ~printer:string_of_int length !producer;
			if not verify
			then Printf.fprintf stderr "%s read(%d) write(%d): %.2f MiB/sec\n"
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
		"ocaml throughput_test" >:: throughput_test ~use_ocaml:true ~read_chunk_size:1024 ~write_chunk_size:1024 ~verify:false;
		"C throughput_test" >:: throughput_test ~use_ocaml:false ~read_chunk_size:1024 ~write_chunk_size:1024 ~verify:false;
		"ocaml correctness_test" >:: throughput_test ~use_ocaml:true ~read_chunk_size:1024 ~write_chunk_size:1024 ~verify:true;
		"C correctness_test" >:: throughput_test ~use_ocaml:false ~read_chunk_size:1024 ~write_chunk_size:1024 ~verify:true;
		"ocaml correctness_test" >:: throughput_test ~use_ocaml:true ~read_chunk_size:1023 ~write_chunk_size:1024 ~verify:true;
		"C correctness_test" >:: throughput_test ~use_ocaml:false ~read_chunk_size:1023 ~write_chunk_size:1024 ~verify:true;
		"ocaml correctness_test" >:: throughput_test ~use_ocaml:true ~read_chunk_size:1024 ~write_chunk_size:1023 ~verify:true;
		"C correctness_test" >:: throughput_test ~use_ocaml:false ~read_chunk_size:1024 ~write_chunk_size:1023 ~verify:true;
    ] in
  run_test_tt ~verbose:!verbose suite
