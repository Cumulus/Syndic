let () =
  try let _ = Atom.analyze (Xmlm.make_input (`Channel stdin)) in ()
  with
    | Atom.Error.Expected (a, b) -> print_endline (Atom.Error.string_of_expectation (a, b))
    | Atom.Error.Malformed_URL e -> print_endline ("Malformed URL: " ^ e)
    | _ -> print_endline "Unknown error"
