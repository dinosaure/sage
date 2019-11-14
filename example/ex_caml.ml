open Sage

let m path =
  let buf = Bytes.create 13 in

  let* () = open_path ro ~path in
  let* len = read buf ~off:0 ~len:13 in

  Fmt.pr "%s" (Bytes.sub_string buf 0 len) ; close

let () =
  let s, () = Sage_caml.run closed (m Sys.argv.(1)) in
  match is_closed s with
  | Some Refl -> ()
  | None -> assert false
