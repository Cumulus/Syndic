let () = Printexc.record_backtrace true

open Lwt
module CLU = Cohttp_lwt_unix
module CLB = Cohttp_lwt.Body

type result =
  | Ok
  | SyndicError of (Xmlm.pos * string)
  | W3CError of (Xmlm.pos * string) list
  | AnotherError of exn

exception Is_not_a_file

type src = [`Data of string | `Uri of Uri.t]
type fmt = [`Atom | `Rss1 | `Rss2 | `Opml1]

let timer =
  let already_used = ref false in
  fun () ->
    if !already_used then (
      Lwt_unix.sleep 1.0
      >>= fun () ->
      already_used := true ;
      Lwt.return () )
    else (
      already_used := true ;
      Lwt.return () )

let get : src -> Xmlm.source Lwt.t = function
  | `Uri src ->
      timer ()
      >>= fun () ->
      CLU.Client.get src
      >>= fun (_response, body) ->
      CLB.to_string body >>= fun data -> Lwt.return (`String (0, data))
  | `Data data -> Lwt.return (`String (0, data))

let parse ?xmlbase = function
  | `Rss1 -> fun src -> `Rss1 (Syndic.Rss1.parse ?xmlbase src)
  | `Rss2 -> fun src -> `Rss2 (Syndic.Rss2.parse ?xmlbase src)
  | `Atom -> fun src -> `Atom (Syndic.Atom.parse ?xmlbase src)
  | `Opml1 -> fun src -> `Opml1 (Syndic.Opml1.parse ?xmlbase src)

let string_of_src = function
  | `Uri uri -> Printf.sprintf "'%s'" (Uri.to_string uri)
  | `Data data ->
      let buffer = Buffer.create 16 in
      Buffer.add_string buffer (String.sub data 0 16) ;
      Buffer.add_string buffer "…" ;
      Buffer.contents buffer

let string_of_fmt = function
  | `Rss1 -> "RSS 1.0"
  | `Rss2 -> "RSS 2.0"
  | `Atom -> "Atom"
  | `Opml1 -> "OPML 1.0"

let tests : ([> src] * [< fmt] * result) list =
  [ (`Uri (Uri.of_string "http://16andcounting.libsyn.com/rss"), `Rss2, Ok)
  ; (`Uri (Uri.of_string "http://ocaml.org/feed.xml"), `Atom, Ok)
  ; (`Uri (Uri.of_string "http://korben.info/feed"), `Rss2, Ok)
  ; (`Uri (Uri.of_string "http://linuxfr.org/journaux.atom"), `Atom, Ok)
  ; (`Uri (Uri.of_string "http://www.reddit.com/r/ocaml/.rss"), `Atom, Ok)
  ; ( `Data
        "<?xml version='1.0' encoding='utf-8'?> <feed \
         xmlns='http://www.w3.org/2015/Atom'> <title></title></feed>"
    , `Atom
    , W3CError [] ) ]

let () =
  Printexc.register_printer (function
    | Syndic.Rss1.Error.Error _ as exn ->
        Some (Syndic.Rss1.Error.to_string exn)
    | Syndic.Rss2.Error.Error _ as exn ->
        Some (Syndic.Rss2.Error.to_string exn)
    | Syndic.Atom.Error.Error _ as exn ->
        Some (Syndic.Atom.Error.to_string exn)
    | Syndic.Opml1.Error.Error _ as exn ->
        Some (Syndic.Opml1.Error.to_string exn)
    | _ -> None )

module Printf = struct
  include Printf

  let add_list ?(sep = "") add_data ch lst =
    let rec aux = function
      | [] -> ()
      | [x] -> add_data ch x
      | x :: r ->
          Printf.fprintf ch "%a%s" add_data x sep ;
          aux r
    in
    aux lst
end

let state, switch =
  let e = ref false in
  ((fun () -> !e), fun () -> e := true)

let sp = Printf.sprintf
let red fmt = sp ("\027[31m" ^^ fmt ^^ "\027[m")
let green fmt = sp ("\027[32m" ^^ fmt ^^ "\027[m")
let yellow fmt = sp ("\027[33m" ^^ fmt ^^ "\027[m")
let blue fmt = sp ("\027[36m" ^^ fmt ^^ "\027[m")
let gray fmt = sp ("\027[37m" ^^ fmt ^^ "\027[m")
let red_s = red "%s"
let green_s = green "%s"
let yellow_s = yellow "%s"
let blue_s = blue "%s"
let gray_s = gray "%s"

let with_process_in cmd f =
  let ic = Unix.open_process_in cmd in
  try
    let r = f ic in
    ignore (Unix.close_process_in ic) ;
    r
  with exn ->
    ignore (Unix.close_process_in ic) ;
    raise exn

let columns =
  try with_process_in "tput cols" (fun ic -> int_of_string (input_line ic))
  with _ -> (
    try
      with_process_in "stty size" (fun ic ->
          match Stringext.split (input_line ic) ~on:' ' with
          | [_; v] -> int_of_string v
          | _ -> failwith "stty" )
    with _ -> ( try int_of_string (Sys.getenv "COLUMNS") with _ -> 80 ) )

let line oc ?color c =
  let line =
    match color with
    | Some `Blue -> blue_s (String.make columns c)
    | Some `Yellow -> yellow_s (String.make columns c)
    | None -> String.make columns c
  in
  Printf.fprintf oc "%s\n%!" line

let left s n =
  let n = n - String.length s in
  if n <= 0 then s else s ^ String.make n ' '

let left_columns = 20
let print s = Printf.printf "%s%!" s

let print_info (src, fmt) =
  print (sp "%s %s" (blue_s (string_of_fmt fmt)) (string_of_src src))

let error (src, fmt) str =
  print (left (red "[ERROR]") left_columns) ;
  print_info (src, fmt) ;
  Printf.printf str

let expected (src, fmt) str =
  print (left (green "[EXPECTED]") left_columns) ;
  print_info (src, fmt) ;
  Printf.printf str

let result_equal a b =
  match (a, b) with
  | SyndicError (_, err), SyndicError (_, err') -> err = err'
  | W3CError _, W3CError _ -> true
  | _, _ -> false

let print_result p (r, e) =
  let add_error _ (pos, err) =
    print (left (gray_s "W3C:") left_columns) ;
    print (Syndic.W3C.Error.Error (pos, err) |> Syndic.W3C.Error.to_string)
  in
  match r with
  | SyndicError (_pos, err) when result_equal r e ->
      (expected p "\nsyndic: %s") err
  | SyndicError (_pos, err) -> (error p "\nsyndic: %s") err
  | W3CError errors when result_equal r e ->
      (expected p "\n%a") (Printf.add_list ~sep:"\n" add_error) errors
  | W3CError errors ->
      (error p "\n%a") (Printf.add_list ~sep:"\n" add_error) errors
  | AnotherError exn -> (error p "exn: %s") (Printexc.to_string exn)
  | Ok ->
      print (left (green "[OK]") left_columns) ;
      print_info p

let newline () = print "\n"
let reset () = print "\r"
let nbsp_entity = function "nbsp" -> Some " " | _ -> None

let make_test (src, fmt, result) =
  print (left (yellow "...") left_columns) ;
  print_info (src, fmt) ;
  get src
  >>= fun xmlm_source ->
  Lwt.catch
    (fun () ->
      let _ = parse fmt (Xmlm.make_input ~entity:nbsp_entity xmlm_source) in
      Lwt.return Ok )
    (function
      | Syndic.Rss1.Error.Error (pos, err)
       |Syndic.Rss2.Error.Error (pos, err)
       |Syndic.Atom.Error.Error (pos, err)
       |Syndic.Opml1.Error.Error (pos, err) -> (
          get (`Uri (Syndic.W3C.url src))
          >>= fun xmlm_source ->
          Lwt.return
            (snd
               (Syndic.W3C.parse
                  (Xmlm.make_input ~entity:nbsp_entity xmlm_source)))
          >>= function
          | [] -> Lwt.return (SyndicError (pos, err))
          | errors ->
              Lwt.return (W3CError (List.map Syndic.W3C.to_error errors)) )
      | exn -> Lwt.return (AnotherError exn))
  >>= fun r ->
  match r with
  | SyndicError (_pos, _err) ->
      reset () ;
      print_result (src, fmt) (r, result) ;
      newline () ;
      switch () ;
      Lwt.return ()
  | W3CError _errors ->
      reset () ;
      print_result (src, fmt) (r, result) ;
      newline () ;
      Lwt.return ()
  | AnotherError _exn ->
      reset () ;
      print_result (src, fmt) (r, result) ;
      newline () ;
      switch () ;
      Lwt.return ()
  | Ok ->
      reset () ;
      print_result (src, fmt) (r, result) ;
      newline () ;
      Lwt.return ()

let () =
  Lwt_main.run (Lwt_list.map_s make_test tests >>= fun _ -> Lwt.return ()) ;
  if state () then exit 1 else exit 0
