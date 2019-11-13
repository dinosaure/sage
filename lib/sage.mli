module type FUNCTOR = sig type 'a t end
module Refl : sig type ('a, 'b) t = Refl : ('a, 'a) t end

type ('a, 's) io

module Make (T : FUNCTOR) : sig
  type 'a s = 'a T.t
  type t

  external inj : 'a s -> ('a, t) io = "%identity"
  external prj : ('a, t) io -> 'a s = "%identity"
end

type 's scheduler =
  { bind : 'a 'b. ('a, 's) io -> ('a -> ('b, 's) io) -> ('b, 's) io
  ; return : 'a. 'a -> ('a, 's) io }

type 'a capabilities =
  | RD_ONLY : < rd : unit; > capabilities
  | WR_ONLY : < wr : unit; > capabilities
  | RDWR : < rd: unit; wr: unit; > capabilities

val ro : < rd : unit; > capabilities
val wo : < wr : unit; > capabilities
val rdwr : < rd : unit; wr : unit; > capabilities

type 'a opened constraint 'a = < .. >
type closed

type ('path, 'p, 'q, 'a) t

val return : 'a -> ('path, 'p, 'p, 'a) t
val map : ('path, 'p, 'q, 'a) t -> ('a -> 'b) -> ('path, 'p, 'q, 'b) t
val both : ('path, 'p, 'q, 'a) t -> ('path, 'q, 'r, 'b) t -> ('path, 'p, 'r, 'a * 'b) t
val bind : ('path, 'p, 'q, 'a) t -> ('a -> ('path, 'q, 'r, 'b) t) -> ('path, 'p, 'r, 'b) t
val open_path : 'c capabilities -> path:'path -> ('path, closed, 'c opened, unit) t
val read : bytes -> off:int -> len:int -> ('path, < rd: unit; .. > opened, < rd : unit; .. > opened, int) t
val write : bytes -> off:int -> len:int -> ('path, < wr : unit; .. > opened, < wr : unit; .. > opened, int) t
val close : ('path, 'a opened, closed, unit) t

type ('c, 'fd) state

val closed : (closed, _) state
val is_closed : ('s, 'fd) state -> ('s, closed) Refl.t option

type ('path, 'fd, 's) syscall =
  { op : 'a. 'a capabilities -> 'path -> ('fd, 's) io
  ; rd : 'fd -> bytes -> off:int -> len:int -> (int, 's) io
  ; wr : 'fd -> bytes -> off:int -> len:int -> (int, 's) io
  ; close : 'fd -> (unit, 's) io }

val run : 's scheduler -> ('path, 'fd, 's) syscall -> ('p, 'fd) state -> ('path, 'p, 'q, 'a) t -> (('q, 'fd) state * 'a, 's) io

val ( let+ ) : ('path, 'p, 'q, 'a) t -> ('a -> 'b) -> ('path, 'p, 'q, 'b) t
val ( let* ) : ('path, 'p, 'q, 'a) t -> ('a -> ('path, 'q, 'r, 'b) t) -> ('path, 'p, 'r, 'b) t
val ( and+ ) : ('path, 'p, 'q, 'a) t -> ('path, 'q, 'r, 'b) t -> ('path, 'p, 'r, 'a * 'b) t
val ( and* ) : ('path, 'p, 'q, 'a) t -> ('path, 'q, 'r, 'b) t -> ('path, 'p, 'r, 'a * 'b) t
