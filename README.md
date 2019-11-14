### Safe and Agnostic monad to handle file

The idea of this package is to provide a safe (check if we are allowed to
read/write on a file) and agnostic (user can specify _syscall_) monad to handle
I/O on a _file_. All of that are abstracted and the core library is completely
agnostic to the system as other MirageOS library.

At the end, the distribution provides specialization on the _Caml_ world (which
uses `in_channel`/`out_channel` type), on a UNIX system (with UNIX
_file-descriptor_) and a LWT specialization.

#### How to use it?

This library wants to protect the user about `open` and `close` actions which
create and delete ressource (in our case, a _file-descriptor_). By this way, we
ensure that a `read` syscall can be done only in a special context where the
ressource is available (where the file is opened).

But let's play a bit with `sage`:

```ocaml
# let m0 = let buf = Bytes.create 0x1000 in Sage.read buf ~off:0 ~len:0x1000 ;;
val m0 : (_, < rd : unit; .. > Sage.opened, < rd : unit; .. > Sage.opened, int) Sage.t
```

The type `t` has 4 types:
- type of the path (usually a `string` but you can use `Fpath.t`)
- type of the pre-condition:
  in this case, we expect that the file is already opened
- type of the post-condition:
  a _read_ does not change the state of the file
- type of the returned value

At the end, this computation must need an opened `state` and to get it, you need
to use `Sage.open_path`:

```ocaml
# let m1 = Sage.open_path Sage.ro "file.txt" ;;
val m1 : (string, Sage.closed, < rd : unit > Sage.opened, unit) Sage.t
```

With this computation, the pre-condition expects a closed file. `path` is
specialized with a `string` (with `"file.txt"`) and the post-condition delivers
an opened file with the read capability (eg. `ro`). So we are able to bind them
then:

```ocaml
# let ( >>= ) = Sage.bind ;;
# let m2 = m1 >>= fun () -> m0 ;;
val m2 : (string, Sage.closed, < rd : unit; .. > Sage.opened, int) Sage.t
```

However, it's not possible to bind a _write_ action with `m1` because we opened
the file in read-only:

```ocaml
# let _ = s >>= fun () -> Sage.write Bytes.empty ~off:0 ~len:0 ;;
Error: this expression has type (_, < wr : unit; .. > Sage.opened, _, _) Sage.t
       but expression was expected of type (_, < rd : unit > Sage.opened, _, _) Sage.t
```

Another impossible case is when you closed the file and you want to read/write then:

```ocaml
# let _ = Sage.close >>= fun () -> m0 ;;
Error: this expression has type (_, < rd : unit > Sage.opened, _, _) Sage.t
       but an expression was expected of type (_, Sage.closed, _, _) Sage.t
```

Of course, the only initial state available for the end-user is the closed state:

```ocaml
# Sage.closed ;;
- : (Sage.closed, 'a) Sage.state
```

And the only way to update the state from `closed` to `opened` is to use the
`open_path` action. By this way, we ensure that any `read`/`write` can be done
only after an `open_path` action. Finally, at the end, we probably wants to
ensure that the ressource is properly closed.

After a `run` of your process, you can prove that with:

```ocaml
let () =
  let s, () = run Sage.closed m in
  match Sage.is_closed s with
  | Some Sage.Refl.Refl -> ()
  | None -> failwith "Ressource leaks!"
```
