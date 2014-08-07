type dtd = string option

type t =
  | Node of Xmlm.tag * t list
  | Leaf of string              (* FIXME: I prefer Data *)

let of_xmlm input =
  let el tag datas = Node (tag, datas) in
  let data data = Leaf data in
  Xmlm.input_doc_tree ~el ~data input

type signal_or_error = [Xmlm.signal | `Error of Xmlm.error]

let xmlm_input i =
  try (Xmlm.input i :> signal_or_error)
  with Xmlm.Error(p, (`Unexpected_eoi as e)) -> `Error e

let rec parse_html acc i =
  match xmlm_input i with
  | `Dtd _ -> parse_html acc i (* ignore *)
  | `El_start tag -> let content = get_tag [] i in
                    parse_html (Node(tag, content) :: acc) i
  | `El_end -> parse_html acc i (* nothing to close, ignore *)
  | `Data d -> parse_html (Leaf d :: acc) i
  | `Error _ -> List.rev acc (* error or EOI *)
and get_tag acc i =
  match xmlm_input i with
  | `Dtd _ -> get_tag acc i (* ignore *)
  | `El_start tag -> let content = get_tag [] i in
                    get_tag (Node(tag, content) :: acc) i
  | `El_end -> List.rev acc
  | `Data d -> get_tag (Leaf d :: acc) i
  | `Error _ -> List.rev acc (* error or EOI *)

let preserve_entity s = Some("&" ^ s ^ ";")

let of_html ?(enc=`UTF_8) ?(entity=preserve_entity) html =
  let i = Xmlm.make_input (`String(0, html)) ~entity ~enc:(Some enc) in
  parse_html [] i



let rec to_xmlm t output =
  match t with
  | Leaf d -> Xmlm.output output (`Data d)
  | Node(tag, t_sub) ->
     Xmlm.output output (`El_start tag);
     List.iter (fun t -> to_xmlm t output) t_sub;
     Xmlm.output output (`El_end)

let to_buffer t b =
  let output = Xmlm.make_output ~decl:false (`Buffer b) in
  to_xmlm t output

let to_string t =
  let b = Buffer.create 4096 in
  to_buffer t b;
  Buffer.contents b
