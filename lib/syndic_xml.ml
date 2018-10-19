type dtd = string option

module Error = Syndic_error

type pos = Xmlm.pos
type tag = Xmlm.tag
type t = Node of pos * tag * t list | Data of pos * string

let resolve ~xmlbase uri =
  match xmlbase with None -> uri | Some b -> Uri.resolve "" b uri

(* Specialized version of the Xmlm.make_input one. *)
let input_of_channel fh =
  (* Xmlm.make_input does not raise any exception. *)
  Xmlm.make_input (`Channel fh)

let of_xmlm input =
  let el tag datas = Node (Xmlm.pos input, tag, datas) in
  let data data = Data (Xmlm.pos input, data) in
  try Xmlm.input_doc_tree ~el ~data input with Xmlm.Error (pos, e) ->
    raise (Error.Error (pos, Xmlm.error_message e))

let get_position = function Node (pos, _, _) -> pos | Data (pos, _) -> pos

let rec t_to_xmlm t output =
  match t with
  | Data (_pos, d) -> (
    try Xmlm.output output (`Data d) with Xmlm.Error (pos, e) ->
      raise (Error.Error (pos, Xmlm.error_message e)) )
  | Node (_pos, tag, t_sub) -> (
      Xmlm.output output (`El_start tag) ;
      List.iter (fun t -> t_to_xmlm t output) t_sub ;
      try Xmlm.output output `El_end with Xmlm.Error (pos, e) ->
        raise (Error.Error (pos, Xmlm.error_message e)) )

(* Specialized version of the Xmlm one. *)
let make_output ?ns_prefix dest =
  (* Xmlm.make_output does not raise any exception. *)
  Xmlm.make_output dest ~decl:true ?ns_prefix

let to_xmlm ?dtd t output =
  ( try Xmlm.output output (`Dtd dtd) with Xmlm.Error (pos, e) ->
      raise (Error.Error (pos, Xmlm.error_message e)) ) ;
  t_to_xmlm t output

let to_buffer ?ns_prefix t b =
  let output = Xmlm.make_output ~decl:false (`Buffer b) ?ns_prefix in
  to_xmlm t output

let to_string ?ns_prefix t =
  let b = Buffer.create 4096 in
  to_buffer ?ns_prefix t b ; Buffer.contents b
