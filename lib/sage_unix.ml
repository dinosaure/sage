open Sage

module Unix_scheduler = Make (struct type 'a t = 'a end)
type unix = Unix_scheduler.t

let unix =
  let open Unix_scheduler in
  { bind= (fun x f -> let x = prj x in f x)
  ; return= (fun x -> inj x) }

type fd = Unix.file_descr

let unix_syscall ~perm =
  let return = unix.return in

  let op
    : type c. c capabilities -> string -> (fd, Unix_scheduler.t) io
    = function
      | Sage.RD_ONLY -> fun path -> return (Unix.openfile path [ Unix.O_RDONLY ] perm)
      | Sage.WR_ONLY -> fun path -> return (Unix.openfile path [ Unix.O_WRONLY ] perm) in
  let rd
    : fd -> bytes -> off:int -> len:int -> (int, Unix_scheduler.t) io
    = fun fd buf ~off ~len -> return (Unix.read fd buf off len) in
  let wr
    : fd -> bytes -> off:int -> len:int -> (int, Unix_scheduler.t) io
    = fun fd buf ~off ~len -> return (Unix.write fd buf off len) in
  let close fd = return (Unix.close fd) in
  let ln fd =
    let st = Unix.LargeFile.fstat fd in
    return st.Unix.LargeFile.st_size in

  { op; rd; wr; ln; close; }

let inj = Unix_scheduler.inj
let prj = Unix_scheduler.prj

let run
  : type p q a. perm:Unix.file_perm -> (p, fd) state -> (string, p, q, a, unix) t -> (q, fd) state * a
  = fun ~perm s m ->
    let v = Unix_scheduler.prj (run unix (unix_syscall ~perm) s m) in v
