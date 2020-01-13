module type FUNCTOR = sig type 'a t end
module Refl = struct type ('a, 'b) t = Refl : ('a, 'a) t end

type ('a, 's) io

module Make (T : FUNCTOR) : sig
  type 'a s = 'a T.t
  type t

  external inj : 'a s -> ('a, t) io = "%identity"
  external prj : ('a, t) io -> 'a s = "%identity"
end = struct
  type 'a s = 'a T.t
  type t

  external inj : 'a -> 'b = "%identity"
  external prj : 'a -> 'b = "%identity"
end

type 's scheduler =
  { bind : 'a 'b. ('a, 's) io -> ('a -> ('b, 's) io) -> ('b, 's) io
  ; return : 'a. 'a -> ('a, 's) io }

type 'a capabilities =
  | RD_ONLY : < rd : unit; > capabilities
  | WR_ONLY : < wr : unit; > capabilities

let ro = RD_ONLY
let wo = WR_ONLY

type 'a opened = | constraint 'a = < .. >
type closed = |

type ('path, 'p, 'q, 'a, 's) t =
  | Return : 'a -> ('path, 'p, 'p, 'a, 's) t
  | Map    : ('path, 'p, 'q, 'a, 's) t * ('a -> 'b) -> ('path, 'p, 'q, 'b, 's) t
  | Both   : ('path, 'p, 'q, 'a, 's) t * ('path, 'q, 'r, 'b, 's) t -> ('path, 'p, 'r, 'a * 'b, 's) t
  | Bind   : ('path, 'p, 'q, 'a, 's) t * ('a -> ('path, 'q, 'r, 'b, 's) t) -> ('path, 'p, 'r, 'b, 's) t
  | Open   : 'c capabilities * 'path -> ('path, closed, 'c opened, unit, 's) t
  | Read   : bytes * int * int -> ('path, < rd: unit; .. > opened, < rd: unit; .. > opened, int, 's) t
  | Write  : bytes * int * int -> ('path, < wr: unit; .. > opened, < wr: unit; .. > opened, int, 's) t
  | Length : ('path, 'a opened, 'a opened, int64, 's) t
  | Close  : ('path, 'a opened, closed, unit, 's) t
  | V      : ('a, 's) io -> ('path, 'p, 'p, 'a, 's) t

let return x = Return x
let map m f = Map (m, f)
let both m n = Both (m, n)
let bind m f = Bind (m, f)
let open_file capabilities ~path = Open (capabilities, path)
let read buf ~off ~len = Read (buf, off, len)
let write buf ~off ~len = Write (buf, off, len)
let length = Length
let close = Close
let v fiber = V fiber

type ('c, 'fd) state =
  | Opened : 'fd -> ('c opened, 'fd) state
  | Closed : (closed, 'fd) state

let closed = Closed

let is_closed
  : type s. (s, 'fd) state -> (s, closed) Refl.t option
  = function
    | Closed -> Some Refl.Refl
    | _ -> None

type ('path, 'fd, 's) syscall =
  { op : 'a. 'a capabilities -> 'path -> ('fd, 's) io
  ; rd : 'fd -> bytes -> off:int -> len:int -> (int, 's) io
  ; wr : 'fd -> bytes -> off:int -> len:int -> (int, 's) io
  ; ln : 'fd -> (int64, 's) io
  ; close : 'fd -> (unit, 's) io }

let run
  : type a s p q path fd. s scheduler -> (path, fd, s) syscall -> (p, fd) state -> (path, p, q, a, s) t -> ((q, fd) state * a, s) io
  = fun { bind; return; } syscall s m ->
    let ( >>= ) = bind in

    let rec go
    : type a p q. (p, fd) state -> (path, p, q, a, s) t -> ((q, fd) state * a, s) io
    = fun s m -> match m, s with
      | Return x, _ -> return (s, x)
      | Map (m, f), _ ->
        go s m >>= fun (s, x) -> return (s, (f x))
      | Both (m, n), _ ->
        go s m >>= fun (s, x) ->
        go s n >>= fun (s, y) ->
        return (s, (x, y))
      | Bind (m, f), _ ->
        go s m >>= fun (s, x) -> go s (f x)
      | Open (c, path), Closed ->
        syscall.op c path >>= fun fd -> return (Opened fd, ())
      | Read (buf, off, len), Opened fd ->
        syscall.rd fd buf ~off ~len >>= fun n -> return (Opened fd, n)
      | Write (buf, off, len), Opened fd ->
        syscall.wr fd buf ~off ~len >>= fun n -> return (Opened fd, n)
      | Length, Opened fd ->
        syscall.ln fd >>= fun len -> return (Opened fd, len)
      | Close, Opened fd ->
        syscall.close fd >>= fun () -> return (Closed, ())
      | V v, s ->
        v >>= fun x -> return (s, x) in
    go s m >>= function (s, v) -> return (s, v)

let ( let+ ) = map
let ( let* ) = bind
let ( and+ ) = both
let ( and* ) = both
