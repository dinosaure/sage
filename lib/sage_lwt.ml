open Sage

module Lwt_scheduler = Make (struct type 'a t = 'a Lwt.t end)

let lwt =
  let open Lwt_scheduler in
  let open Lwt.Infix in
  { bind= (fun x f -> inj ((prj x) >>= fun x -> prj (f x)))
  ; return= (fun x -> inj (Lwt.return x)) }

module type SYSCALL = sig
  type t
  type path

  val open_ro : path -> t Lwt.t
  val open_wo : path -> t Lwt.t
  val read : t -> bytes -> off:int -> len:int -> int Lwt.t
  val write : t -> bytes -> off:int -> len:int -> int Lwt.t
  val close : t -> unit Lwt.t
end

let run
  : type fd path p q a.
    (module SYSCALL with type t = fd and type path = path) ->
    (p, fd) state -> (path, p, q, a) t -> ((q, fd) state * a) Lwt.t
  = fun (module Syscall) s m ->
    let inj = Lwt_scheduler.inj in

    let op : type c. c capabilities -> path -> (fd, Lwt_scheduler.t) io = function
      | Sage.RD_ONLY -> fun path -> inj (Syscall.open_ro path)
      | Sage.WR_ONLY -> fun path -> inj (Syscall.open_wo path) in
    let rd fd buf ~off ~len = inj (Syscall.read fd buf ~off ~len) in
    let wr fd buf ~off ~len = inj (Syscall.write fd buf ~off ~len) in
    let close fd = inj (Syscall.close fd) in

    let v = Lwt_scheduler.prj (run lwt { op; rd; wr; close; } s m) in v
