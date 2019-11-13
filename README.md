### Safe and Agnostic monad to handle file

The idea of this package is to provide a safe (check if we are allowed to read/write on a file)
and agnostic (user can specify _syscall_) monad to handle I/O on a _file_.

#### How to use it?

The library provides a wrapper of the `Caml` world, let the user to craft the monad and provides
a `run` function:

```ocaml
open Sage

let my_computation =
  let* () = open_path ro ~path:"file" in
  let buf = Bytes.create 0x100 in
  let* len = read buf ~off:0 ~len:0x100 in
  Fmt.pr "%s%!" (Bytes.sub_string buf 0 len) ;
  close

let () =
  let s, () = Caml.run closed my_computation in
  match is_closed s with
  | Some Refl.Refl -> () (* properly closed *)
  | None -> assert false
```
