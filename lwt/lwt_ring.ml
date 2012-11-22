(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
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
open Printf

module Client = struct

  type ('a, 'b) t = {
    ring: ('a, 'b) Ring.Front.t;
    wakers: ('b, 'a Lwt.u) Hashtbl.t; (* id * wakener *)
    waiters: unit Lwt.u Lwt_sequence.t;
  }

  let init ring =
    let wakers = Hashtbl.create 7 in
    let waiters = Lwt_sequence.create () in
    { ring; wakers; waiters }

  let wait_for_free_slot t =
    if Ring.Front.get_free_requests t.ring > 0 then
      return ()
    else begin
      let th, u = Lwt.task () in
      let node = Lwt_sequence.add_r u t.waiters in
      Lwt.on_cancel th (fun _ -> Lwt_sequence.remove node);
      th
    end 

  let poll t respfn =
    Ring.Front.ack_responses t.ring (fun slot ->
      let id, resp = respfn slot in
      try
         let u = Hashtbl.find t.wakers id in
         Hashtbl.remove t.wakers id;
         Lwt.wakeup u resp
       with Not_found ->
         printf "RX: ack id wakener not found\n%!"
    );
    (* Check for any sleepers waiting for free space *)
    match Lwt_sequence.take_opt_l t.waiters with
    |None -> ()
    |Some u -> Lwt.wakeup u ()

  let rec push_request_and_wait t reqfn =
    if Ring.Front.get_free_requests t.ring > 0 then begin
      let slot_id = Ring.Front.next_req_id t.ring in
      let slot = Ring.Front.slot t.ring slot_id in
      let th,u = Lwt.task () in
      let id = reqfn slot in
      if Ring.Front.push_requests_and_check_notify t.ring
      then printf "TX: need to signal event channel\n%!";
      Lwt.on_cancel th (fun _ -> Hashtbl.remove t.wakers id);
      Hashtbl.add t.wakers id u;
      th
    end else begin
      let th,u = Lwt.task () in
      let node = Lwt_sequence.add_r u t.waiters in
      Lwt.on_cancel th (fun _ -> Lwt_sequence.remove node);
      th >>
      push_request_and_wait t reqfn
    end

   let push_request_async t reqfn freefn =
     lwt () = wait_for_free_slot t in
     let slot_id = Ring.Front.next_req_id t.ring in
     let slot = Ring.Front.slot t.ring slot_id in
     let th,u = Lwt.task () in
     let id = reqfn slot in
     if Ring.Front.push_requests_and_check_notify t.ring
     then printf "TX: need to signal event channel\n%!";
     Lwt.on_cancel th (fun _ -> Hashtbl.remove t.wakers id);
     Hashtbl.add t.wakers id u;
     let _ = th >> return (freefn ()) in
     return ()

end

module Server = struct
  type ('a, 'b) t = {
    ring: ('a, 'b) Ring.Back.t;
  }

  let init ring =
    { ring }

  let push_response t rspfn =
	  let slot_id = Ring.Back.next_res_id t.ring in
	  let slot = Ring.Back.slot t.ring slot_id in
	  rspfn slot;
	  if Ring.Back.push_responses_and_check_notify t.ring
	  then printf "TX: need to signal event channel\n%!"

  let poll t fn =
    Ring.Back.ack_requests t.ring fn
end
