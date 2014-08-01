open Syndic

let () =

  Printexc.record_backtrace true;

  try let _ = Atom.analyze (Xmlm.make_input (`Channel stdin)) in ()
  with
    | Atom.Error.Expected (a, b) ->
        print_endline (Atom.Error.string_of_expectation (a, b))
    | _ -> Printexc.print_backtrace stderr
