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

open Ring

module Ring = struct
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
	let to_debug_string t =
		Printf.sprintf "input_cons = %ld prod = %ld; output cons = %ld prod = %ld"
			(Layout.get_ring_input_cons t) (Layout.get_ring_input_prod t)
			(Layout.get_ring_output_cons t) (Layout.get_ring_output_prod t)
			
	module Front = Pipe(Layout)
	module Back = Pipe(Reverse(Layout))
end
