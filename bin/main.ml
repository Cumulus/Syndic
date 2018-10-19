open Syndic

let () =
  Printexc.record_backtrace true ;
  try
    let lst, _ = W3C.parse (Xmlm.make_input (`Channel stdin)) in
    List.iter
      (fun (_, err) -> Printf.printf "E: %s\n%!" err)
      (List.map Syndic.W3C.to_error lst)
  with
  | W3C.Error.Error ((l, c), err) -> Printf.printf "[%d;%d]: %s\n%!" l c err
  | _ -> Printexc.print_backtrace stderr
