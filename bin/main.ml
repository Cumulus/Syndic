open Syndic

let () =

  Printexc.record_backtrace true;

  try let rss2 =
        Syndic.Rss2.relax
          Rss2.Relax.({ channel with
                        description = ignore;
                        cloud = { cloud with
                                  registerProcedure = ignore;
                                  protocol          = ignore;
                                  domain            = ignore;
                                  port              = ignore;
                                  path              = ignore;
                                  uri =
                                    (fun ~pos _ _ _ -> (None : Uri.t option)) } })
          (Xmlm.make_input (`Channel stdin)) in
      ()
  with
    | Rss2.Error.Error ((l, c), err) ->
      Printf.printf "[%d;%d]: %s\n%!"
        l c err
    | _ -> Printexc.print_backtrace stderr
