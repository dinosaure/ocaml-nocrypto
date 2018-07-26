open Core
open Async
open Nocrypto

let chunk = 32
and period =
  (* Synchronous_time_source.(alarm_precision (wall_clock ())) *)
  Core.Time_ns.Span.of_int_sec 30
and device = Nocrypto_entropy_unix.sys_rng

let mvar_map ~name t f =
  Mvar.take t >>= fun v ->
  Monitor.try_with ~name (fun () -> f v) >>= function
  | Ok v -> Mvar.put t v
  | Error exn -> raise exn

let start_queue = Queue.create ()

type t =
  { fd : Fd.t
  ; g : Rng.g }

let background ~period f =
  let live = ref false in

  fun () ->
    Synchronous_time_source.(run_at_intervals (wall_clock ()))
      period @@ fun () ->
    if (not !live)
    then begin
      live := true
    ; don't_wait_for (f () >>| fun _ -> live := false)
    end

let attach ~period ?(device = device) g =
  Unix.openfile device ~mode:[ `Rdonly ] >>= fun fd ->
  let reader = Reader.create fd in
  let buf = Bytes.create chunk in
  let seed () =
    let rec go acc =
      if acc = chunk
      then return ()
      else Reader.read ~pos:acc ~len:(chunk - acc) reader buf >>= function
        | `Ok n -> go (acc + n)
        | `Eof -> go acc in
    go 0 >>| fun () ->
    Rng.reseed ~g (Cstruct.of_bytes buf) in
  Queue.enqueue start_queue (background ~period seed);
  return { fd; g; }

let stop t =
  Queue.clear start_queue;
  Monitor.try_with ~name:"close entropy device"
    (fun () -> Fd.close t.fd) >>= function
  | Ok () | Error (Unix.Unix_error (Unix.EBADF, _, _)) -> return ()
  | Error exn -> raise exn

let active = let t = Mvar.create () in Mvar.set t None; t

let some x = Some x

let () = Async.Scheduler.Expert.set_on_start_of_cycle
    (fun () -> Queue.iter start_queue ~f:(fun f -> f ()))
(* XXX(dinosaure): really not sure. *)

let initialize () =
  Nocrypto_entropy_unix.initialize ();
  let g = !Rng.generator in
  mvar_map ~name:"entropy async initialization" active @@ function
  | Some t when phys_equal t.g g -> return (Some t)
  | Some t -> stop t >>= fun () -> attach ~period g >>| some
  | None -> attach ~period g >>| some
