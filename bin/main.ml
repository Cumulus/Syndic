open Syndic

let () =

  Printexc.record_backtrace true;

  try let _ = Atom.parse (Xmlm.make_input (`Channel stdin)) in ()
  with
    | Atom.Error.Error (pos, err) ->
        print_endline err
    | _ -> Printexc.print_backtrace stderr
