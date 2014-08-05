type t =
  | Node of Xmlm.tag * t list
  | Leaf of string              (* FIXME: I prefer Data *)

let of_xmlm input =
  let el tag datas = Node (tag, datas) in
  let data data = Leaf data in
  Xmlm.input_doc_tree ~el ~data input

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
