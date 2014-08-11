type dtd = string option

type t =
  | Node of Xmlm.pos * Xmlm.tag * t list
  | Data of Xmlm.pos * string              (* FIXME: I prefer Data *)

let of_xmlm input =
  let el tag datas = Node (Xmlm.pos input, tag, datas) in
  let data data = Data (Xmlm.pos input, data) in
  Xmlm.input_doc_tree ~el ~data input

let get_position = function
  | Node (pos, _, _) -> pos
  | Data (pos, _) -> pos

let rec t_to_xmlm t output =
  match t with
  | Data (pos, d) -> Xmlm.output output (`Data d)
  | Node (pos, tag, t_sub) ->
     Xmlm.output output (`El_start tag);
     List.iter (fun t -> t_to_xmlm t output) t_sub;
     Xmlm.output output (`El_end)

let to_xmlm ?dtd t output =
  Xmlm.output output (`Dtd dtd);
  t_to_xmlm t output

let to_buffer ?ns_prefix t b =
  let output = Xmlm.make_output ~decl:false (`Buffer b) ?ns_prefix in
  to_xmlm t output

let to_string ?ns_prefix t =
  let b = Buffer.create 4096 in
  to_buffer ?ns_prefix t b;
  Buffer.contents b
