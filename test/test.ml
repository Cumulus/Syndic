let () = Printexc.record_backtrace true

type result =
  | Ok
  | SyndicError of (Xmlm.pos * string)
  | W3CError of (Xmlm.pos * string) list
  | AnotherError of exn

exception Is_not_a_file

type src = [`Data of string | `Filename of (Fpath.t * Uri.t) ]
type fmt = [`Atom | `Rss1 | `Rss2 | `Opml1]

let get : src -> Xmlm.source = function
  | `Filename (fpath, _) ->
    `Channel (open_in (Fpath.to_string fpath))
  | `Data data -> `String (0, data)

let parse ?xmlbase = function
  | `Rss1 -> fun src -> `Rss1 (Syndic.Rss1.parse ?xmlbase src)
  | `Rss2 -> fun src -> `Rss2 (Syndic.Rss2.parse ?xmlbase src)
  | `Atom -> fun src -> `Atom (Syndic.Atom.parse ?xmlbase src)
  | `Opml1 -> fun src -> `Opml1 (Syndic.Opml1.parse ?xmlbase src)

let string_of_src = function
  | `Filename (_, uri) -> Fmt.str "'%s'" (Uri.to_string uri)
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

type entry =
  { name : string
  ; uri : Uri.t
  ; kind : fmt }

let json =
  let open Json_encoding in
  let name = req "name" string in
  let uri = req "uri" (conv Uri.to_string Uri.of_string string) in
  let kind =
    let rss1 = case string (function `Rss1 -> Some "rss1" | _ -> None) (function "rss1" -> `Rss1 | _ -> assert false) in
    let rss2 = case string (function `Rss2 -> Some "rss2" | _ -> None) (function "rss2" -> `Rss2 | _ -> assert false) in
    let atom = case string (function `Atom -> Some "atom" | _ -> None) (function "atom" -> `Atom | _ -> assert false) in
    req "kind" (union [ rss1; rss2; atom ]) in
  let entry = conv (fun { name; uri; kind; } -> (name, uri, kind)) (fun (name, uri, kind) -> { name; uri; kind; }) (obj3 name uri kind) in
  list entry

type await = [ `Await ]
type error = [ `Error of Jsonm.error ]
type eoi = [ `End ]
type value = [ `Null | `Bool of bool | `String of string | `Float of float ]

let json_of_input ic =
  let decoder = Jsonm.decoder (`Channel ic) in

  let error (`Error err) = Fmt.invalid_arg "%a" Jsonm.pp_error err in
  let end_of_input `End = Fmt.invalid_arg "Unexpected end of input" in

  let rec arr acc k = match Jsonm.decode decoder with
    | #await -> assert false
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme `Ae -> k (`A (List.rev acc))
    | `Lexeme v -> base (fun v -> arr (v :: acc) k) v

  and name n k = match Jsonm.decode decoder with
    | #await -> assert false
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme v -> base (fun v -> k (n, v)) v

  and obj acc k = match Jsonm.decode decoder with
    | #await -> assert false
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme `Oe -> k (`O (List.rev acc))
    | `Lexeme (`Name n) -> name n (fun v -> obj (v :: acc) k)
    | `Lexeme v -> Fmt.invalid_arg "Unexpected lexeme: %a" Jsonm.pp_lexeme v

  and base k = function
    | #value as v -> k v
    | `Os -> obj [] k
    | `As -> arr [] k
    | `Ae | `Oe -> Fmt.invalid_arg "Unexpected end of array/object"
    | `Name n -> Fmt.invalid_arg "Unexpected key: %s" n in

  let go k = match Jsonm.decode decoder with
    | #await -> assert false
    | #error as v -> error v
    | #eoi as v -> end_of_input v
    | `Lexeme (#Jsonm.lexeme as lexeme) -> base k lexeme in

  go Json_encoding.(destruct json)

let tests : ([> src] * [< fmt] * result) list =
  let ic = open_in "feeds.json" in
  let entries = json_of_input ic in
  close_in ic ;
  let entries = List.map (fun { name; kind; uri; } -> `Filename (Fpath.v name |> Fpath.add_ext "feed", uri), kind, Ok) entries in
  entries

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
  let xmlm_source = get src in
  let r =
    try
      let _ = parse fmt (Xmlm.make_input ~entity:nbsp_entity xmlm_source) in Ok
    with
    | Syndic.Rss1.Error.Error (_pos, _err)
    | Syndic.Rss2.Error.Error (_pos, _err)
    | Syndic.Atom.Error.Error (_pos, _err)
    | Syndic.Opml1.Error.Error (_pos, _err) -> Ok
    | exn -> (AnotherError exn)
  in
  match r with
  | SyndicError (_pos, _err) ->
      reset () ;
      print_result (src, fmt) (r, result) ;
      newline () ;
      switch ()
  | W3CError _errors ->
      reset () ;
      print_result (src, fmt) (r, result) ;
      newline ()
  | AnotherError _exn ->
      reset () ;
      print_result (src, fmt) (r, result) ;
      newline () ;
      switch ()
  | Ok ->
      reset () ;
      print_result (src, fmt) (r, result) ;
      newline ()

let () =
  List.iter make_test tests ;
  if state () then exit 1 else exit 0
