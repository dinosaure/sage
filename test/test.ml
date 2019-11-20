let test01 =
  Alcotest.test_case "test01" `Quick @@ fun () ->
  let m =
    let open Sage in
    let* () = open_file ro ~path:"test01.txt" in
    let* len = length in
    let buf = Bytes.create (Int64.to_int len) in
    let* res = read buf ~off:0 ~len:(Int64.to_int len) in
    Alcotest.(check int) "len" (Int64.to_int len) res ;
    Alcotest.(check string) "buf" (Bytes.unsafe_to_string buf) "Hello World!" ;
    close in
  let s, () = Sage_caml.run Sage.closed m in
  match Sage.is_closed s with
  | Some Sage.Refl.Refl -> ()
  | None -> Alcotest.fail "fd leaks"

let () =
  Alcotest.run "sage"
    [ "caml", [ test01 ] ]
