let () =
  try let _ = Rss1.analyze (Xmlm.make_input (`Channel stdin))
  in ()
  with
    | Rss1.Expected (a, b) -> print_endline (Rss1.string_of_expectation (a, b))
    | Rss1.Malformed_URL e -> print_endline ("Malformed URL: " ^ e)
    | _ -> print_endline "Unknown error"
