let () =
  let version, homepage, output =
    match Sys.argv with
    | [|_; "--version"; version; "--homepage"; homepage; "-o"; output|] ->
        (version, homepage, output)
    | _ ->
        invalid_arg
          "%s --version ${VERSION} --homepage ${HOMEPAGE} -o <output>"
  in
  let oc = open_out output in
  Printf.fprintf oc "let version = \"%s\" and homepage = Uri.of_string \"%s\""
    version homepage ;
  close_out oc
