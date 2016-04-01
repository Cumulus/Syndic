open Ocamlbuild_plugin

let subst vars s =
  let buf = Buffer.create (String.length s) in
  let start = ref 0 in
  let last = ref 0 in
  let len = String.length s in
  while (!last < len - 4) do
    if not (s.[!last] = '%' && s.[!last + 1] = '%')
    then incr last
    else
      begin
        let start_subst = !last in
        let last_id = ref (!last + 2) in
        let stop = ref false in
        while (!last_id < len - 1 && not !stop) do
          if not (s.[!last_id] = '%' && s.[!last_id + 1] = '%') then begin
            if s.[!last_id] <> ' ' then (incr last_id) else
              (stop := true; last := !last_id)
          end else begin
            let id_start = start_subst + 2 in
            let id = String.sub s (id_start) (!last_id - id_start) in
            try
              let subst = List.assoc id vars in
              Buffer.add_substring buf s !start (start_subst - !start);
              Buffer.add_string buf subst;
              stop := true;
              start := !last_id + 2;
              last := !last_id + 2;
            with Not_found ->
              stop := true;
              last := !last_id
          end;
        done;
      end
  done;
  Buffer.add_substring buf s !start (len - !start);
  Buffer.contents buf;;

let subst_rule file args =
  rule file ~dep:(file ^ ".in") ~prod:file
    (fun env build ->
       Printf.eprintf "we have subst rule with %s\n" file;
       let ifile = env (file ^ ".in") in
       let ic = open_in ifile in
       let ilen = in_channel_length ic in
       let content = String.make ilen ' ' in

       really_input ic content 0 ilen;

       let res = subst args content in
       Echo( [ res ], file));;

(* OASIS_START *)
(* OASIS_STOP *)

let env_filename = Pathname.basename BaseEnvLight.default_filename
let env          = BaseEnvLight.load ~filename:env_filename ~allow_empty:true ()

let has_ptime    = bool_of_string (BaseEnvLight.var_get "ptime" env)
let has_calendar = bool_of_string (BaseEnvLight.var_get "calendar" env)
let version      = (BaseEnvLight.var_get "pkg_version" env)

let link_rule source dest =
  rule (Printf.sprintf "%s -> %s" source dest)
    ~dep:source
    ~prod:dest
    (fun env _ -> Cmd (S [A "ln"; A "-f"; P (env source); P (env dest)]))

let () = dispatch
    (function
     | After_hygiene ->
       if has_ptime
       then begin
         flag ["ocaml"; "ocamldep"]
           (S [A "-package"; A "ptime"]);
         flag ["ocaml"; "compile"]
           (S [A "-package"; A "ptime"]);
         flag ["ocaml"; "link"]
           (S [A "-package"; A "ptime"]);


         link_rule
           "lib/ptime/syndic_date.ml"
           "lib/syndic_date.ml";
       end;

       if has_calendar
       then begin
         flag ["ocaml"; "ocamldep"]
           (S [A "-package"; A "calendar"]);
         flag ["ocaml"; "compile"]
           (S [A "-package"; A "calendar"]);
         flag ["ocaml"; "link"]
           (S [A "-package"; A "calendar"]);

         link_rule
           "lib/calendar/syndic_date.ml"
           "lib/syndic_date.ml";
       end;

       dispatch_default After_hygiene
     | hook -> dispatch_default hook)
