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

exception Shutdown

module Front = struct

  (* If there aren't free slots on the ring then add a 'waiter'
     to the sequence of 'waiters' *)
  type waiter = {
    nr_slots_needed: int;
    th: int Lwt.t;
    u: int Lwt.u;
  }

  type ('a, 'b) t = {
    ring: ('a, 'b) Ring.Rpc.Front.t;
    wakers: ('b, 'a Lwt.u) Hashtbl.t; (* id * wakener *)
    waiters: waiter Lwt_sequence.t; (* threads queueing for free slot(s) *)
    string_of_id: 'b -> string;
  }

  let init string_of_id ring =
    let wakers = Hashtbl.create 7 in
    let waiters = Lwt_sequence.create () in
    { ring; wakers; waiters; string_of_id }

  let rec maybe_wake_next_thread t =
    let nr_slots_free = Ring.Rpc.Front.get_free_requests t.ring in
    match Lwt_sequence.take_opt_l t.waiters with
    | Some x when x.nr_slots_needed <= nr_slots_free ->
      (* ensure slots are allocated in the queue order *)
      let next_slot_id = Ring.Rpc.Front.allocate_slots t.ring x.nr_slots_needed in
      Lwt.wakeup_later x.u next_slot_id
    | Some x ->
      (* unfortunately there aren't enough free slots for the
         first thread in the queue, so we have to put it back. *)
      let is_first = Lwt_sequence.is_empty t.waiters in
      let node = Lwt_sequence.add_l x t.waiters in
      add_cancel_callback t is_first node x
    | None -> ()

  and add_cancel_callback t is_first node x =
      Lwt.on_cancel x.th (fun _ ->
        Lwt_sequence.remove node;
        (* If I was first in the queue then the thread waiting next
           may be waiting for fewer slots and so may be immediately
           runnable. *)
        if is_first then maybe_wake_next_thread t
      )

  let get_free_slots t nr_slots_needed =
    (* We want to make sure requests are emitted in order, so
       we take care not to let a short (eg 1 slot) write race
       ahead of a blocked long (eg 2 slot) write. *)
    let is_first = Lwt_sequence.is_empty t.waiters in
    if is_first && Ring.Rpc.Front.get_free_requests t.ring >= nr_slots_needed then
      let next_free_slot = Ring.Rpc.Front.allocate_slots t.ring nr_slots_needed in
      return next_free_slot
    else begin
      let th, u = Lwt.task () in
      let x = { nr_slots_needed; th; u } in
      let node = Lwt_sequence.add_r x t.waiters in
      add_cancel_callback t is_first node x;
      (* wait for enough slots to be freed *)
      th
    end 

  let poll t respfn =
    Ring.Rpc.Front.ack_responses t.ring (fun slot ->
      let id, resp = respfn slot in
      try
         let u = Hashtbl.find t.wakers id in
         Hashtbl.remove t.wakers id;
         Lwt.wakeup u resp
       with Not_found ->
         printf "RX: ack (id = %s) wakener not found\n" (t.string_of_id id);
         printf "    valid ids = [ %s ]\n%!" (String.concat "; " (List.map t.string_of_id (Hashtbl.fold (fun k _ acc -> k :: acc) t.wakers [])));
    );
    (* Check for any sleepers waiting for free space *)
    maybe_wake_next_thread t

  (* Every active slot corresponds to a (i) place to write to; and a
     (ii) registered request id. *)
  let prepare t slot_id request_id =
    let slot = Ring.Rpc.Front.slot t.ring slot_id in
    let th, u = Lwt.task () in
    Lwt.on_cancel th (fun _ -> Hashtbl.remove t.wakers request_id);
    Hashtbl.add t.wakers request_id u;
    (slot, th)

  let write t id =
    lwt slot_id = get_free_slots t 1 in
    return (prepare t slot_id id)

  let writev t request_ids =
    let len = List.length ids in
    lwt first_slot_id = get_free_slots t len in
    let _, rev_slots = List.fold_left (fun (next_slot_id, acc) request_id ->
      next_slot_id + 1, prepare t next_slot_id request_id :: acc
    ) (first_slot_id, []) request_ids in
    return (List.rev rev_slots)

  let push t notifyfn =
    if Ring.Rpc.Front.push_requests_and_check_notify t.ring
    then notifyfn ()

  (*
  let push_request_and_wait t notifyfn reqfn =
    lwt th = write t reqfn in
    push t notifyfn;
    th
  *)
  (*
   let push_request_async t notifyfn reqfn freefn =
     lwt th = write t reqfn in
     push t notifyfn;
     let _ = freefn th in
     return ()
  *)
   let shutdown t =
     Hashtbl.iter (fun id th ->
       Lwt.wakeup_exn th Shutdown
     ) t.wakers;
     (* Check for any sleepers waiting for free space *)
     let rec loop () =
       match Lwt_sequence.take_opt_l t.waiters with
       | None -> ()
       | Some u -> Lwt.wakeup_exn u Shutdown; loop ()
     in loop ()

end

module Back = struct
  type ('a, 'b) t = {
    ring: ('a, 'b) Ring.Rpc.Back.t;
    string_of_id: 'b -> string;
  }

  let init string_of_id ring =
    { ring; string_of_id }

  let push_response t notifyfn rspfn =
	  let slot_id = Ring.Rpc.Back.next_res_id t.ring in
	  let slot = Ring.Rpc.Back.slot t.ring slot_id in
	  rspfn slot;
	  if Ring.Rpc.Back.push_responses_and_check_notify t.ring
	  then notifyfn ()

  let poll t fn =
    Ring.Rpc.Back.ack_requests t.ring fn
end
