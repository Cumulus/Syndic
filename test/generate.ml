#use "topfind" ;;

#require "ocplib-json-typed" ;;
#require "fmt" ;;
#require "jsonm" ;;
#require "fpath" ;;

type entry =
  { name : string
  ; uri : Uri.t
  ; kind : kind }
and kind = Rss1 | Rss2 | Atom

let json =
  let open Json_encoding in
  let name = req "name" string in
  let uri = req "uri" (conv Uri.to_string Uri.of_string string) in
  let kind =
    let rss1 = case string (function Rss1 -> Some "rss1" | _ -> None) (function "rss1" -> Rss1 | _ -> assert false) in
    let rss2 = case string (function Rss2 -> Some "rss2" | _ -> None) (function "rss2" -> Rss2 | _ -> assert false) in
    let atom = case string (function Atom -> Some "atom" | _ -> None) (function "atom" -> Atom | _ -> assert false) in
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

let flat_json json : Jsonm.lexeme list =
  let rec arr acc k = function
    | [] -> k (List.rev (`Ae :: acc))
    | (#value as x) :: r -> arr (x :: acc) k r
    | `A l :: r -> arr [ `As ] (fun l -> arr (List.rev_append l acc) k r) l
    | `O l :: r -> obj [ `Os ] (fun l -> arr (List.rev_append l acc) k r) l

  and obj acc k = function
    | [] -> k (List.rev (`Oe :: acc))
    | (n, x) :: r -> base (fun v -> obj (List.rev_append v (`Name n :: acc)) k r) x

  and base k = function
    | `A l -> arr [ `As ] k l
    | `O l -> obj [ `Os ] k l
    | #value as x -> k [ x ] in

  base (fun l -> l) json

external identity : 'a -> 'a = "%identity"

let pp_json ppf map =
  let json = Json_encoding.(construct json map) in
  let raw = Bytes.create 0x800 in
  let encoder = Jsonm.encoder `Manual in
  let rec write k = function
    | `Ok -> k ()
    | `Partial ->
      Fmt.string ppf (Bytes.sub_string raw 0 (Jsonm.Manual.dst_rem encoder)) ;
      Jsonm.Manual.dst encoder raw 0 (Bytes.length raw) ;
      write k (Jsonm.encode encoder `Await) in
  let rec go k = function
    | [] -> write k (Jsonm.encode encoder `End)
    | lexeme :: r -> write (fun () -> go k r) (Jsonm.encode encoder (`Lexeme lexeme)) in
  let lexemes = flat_json json in
  go identity lexemes

let pp_entry ppf entry =
  Fmt.pf ppf "(rule (targets %s.feed) \
              (mode fallback) \
              (deps (:gen downloader.ml)) \
              (action (run %%{ocaml} %%{gen} --uri %s --output %%{targets})))"
    entry.name (Uri.to_string entry.uri)

let pp_test ppf entries =
  Fmt.pf ppf "(alias (name runtest) \
              (package syndic) \
              (deps (:test test.exe) (:feeds %a) feeds.json) \
              (action (run %%{test} --color=always)))@\n"
    Fmt.(list ~sep:(const string " ") (using (fun { name; _ } -> name ^ ".feed") string)) entries

let man () = Fmt.epr "%s --json <json> --output <dune.inc>\n%!" Sys.argv.(0)

let success = 0
let failure = 1

let compare_entry a b = String.compare a.name b.name

let () =
  let json, output =
    try match Sys.argv with
      | [| _; "--json"; json; "--output"; output |] ->
        if Sys.file_exists json
        then Fpath.v json, Fpath.v output
        else ( Fmt.epr "%s does not exist.\n%!" json; exit failure )
      | _ -> man () ; exit failure
    with _ -> man () ; exit failure in
  let ic = open_in (Fpath.to_string json) in
  let data = List.sort compare_entry (json_of_input ic) in
  let oc = open_out (Fpath.to_string output) in
  let ppf = Format.formatter_of_out_channel oc in
  Fmt.pf ppf "%a@\n@\n%!" Fmt.(list ~sep:(any "@\n") pp_entry) data ;
  Fmt.pf ppf "%a@\n" pp_test data ;
  close_out oc ;
  exit success
