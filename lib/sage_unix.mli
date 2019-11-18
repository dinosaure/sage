type fd = Unix.file_descr
type unix

val inj : 'a -> ('a, unix) Sage.io
val prj : ('a, unix) Sage.io -> 'a

val run : perm:Unix.file_perm -> ('p, fd) Sage.state -> (string, 'p, 'q, 'a) Sage.t -> ('q, fd) Sage.state * 'a
