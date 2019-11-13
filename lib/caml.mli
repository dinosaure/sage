type fd

val run : ('p, fd) Sage.state -> (string, 'p, 'q, 'a) Sage.t -> ('q, fd) Sage.state * 'a
