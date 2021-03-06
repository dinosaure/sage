open Sage

module Caml_scheduler = Make (struct type 'a t = 'a end)
type caml = Caml_scheduler.t

let caml =
  let open Caml_scheduler in
  { bind= (fun x f -> let x = prj x in f x)
  ; return= (fun x -> inj x) }

type fd =
  | RD_ONLY of in_channel
  | WR_ONLY of out_channel

let caml_syscall =
  let return = caml.return in

  let op
    : type c. c capabilities -> string -> (fd, Caml_scheduler.t) io
    = function
    | Sage.RD_ONLY -> fun path -> return (RD_ONLY (open_in path))
    | Sage.WR_ONLY -> fun path -> return (WR_ONLY (open_out path)) in

  let rd
    : fd -> bytes -> off:int -> len:int -> (int, Caml_scheduler.t) io
    = function
    | RD_ONLY ic -> fun buf ~off ~len -> return (input ic buf off len)
    | WR_ONLY _ -> assert false in

  let wr = function
    | WR_ONLY oc -> fun buf ~off ~len -> output oc buf off len ; return len
    | RD_ONLY _ -> assert false in

  let close = function
    | RD_ONLY ic -> return (close_in ic)
    | WR_ONLY oc -> return (close_out oc) in

  let ln = function
    | RD_ONLY ic -> return (Int64.of_int (in_channel_length ic))
    | WR_ONLY oc -> return (Int64.of_int (out_channel_length oc)) in

  { op; rd; wr; ln; close; }

let inj = Caml_scheduler.inj
let prj = Caml_scheduler.prj

let run
  : type p q a. (p, fd) state -> (string, p, q, a, caml) t -> (q, fd) state * a
  = fun s m ->
    let v = Caml_scheduler.prj (run caml caml_syscall s m) in v
