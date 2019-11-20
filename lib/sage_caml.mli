type fd
type caml

val caml : caml Sage.scheduler
val inj : 'a -> ('a, caml) Sage.io
val prj : ('a, caml) Sage.io -> 'a

val run : ('p, fd) Sage.state -> (string, 'p, 'q, 'a) Sage.t -> ('q, fd) Sage.state * 'a
