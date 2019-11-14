type fd = Unix.file_descr

val run : perm:Unix.file_perm -> ('p, fd) Sage.state -> (string, 'p, 'q, 'a) Sage.t -> ('q, fd) Sage.state * 'a
