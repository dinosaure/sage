type lwt

module type SYSCALL = sig
  type t
  type path

  val open_ro : path -> t Lwt.t
  val open_wo : path -> t Lwt.t
  val length : t -> int64 Lwt.t
  val read : t -> bytes -> off:int -> len:int -> int Lwt.t
  val write : t -> bytes -> off:int -> len:int -> int Lwt.t
  val close : t -> unit Lwt.t
end

val lwt : lwt Sage.scheduler
val inj : 'a Lwt.t -> ('a, lwt) Sage.io
val prj : ('a, lwt) Sage.io -> 'a Lwt.t 

val run
  : (module SYSCALL with type t = 'fd and type path = 'path) ->
    ('p, 'fd) Sage.state -> ('path, 'p, 'q, 'a, lwt) Sage.t -> (('q, 'fd) Sage.state * 'a) Lwt.t
