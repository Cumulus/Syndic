(* XML *)

module XML = struct
  include Syndic_xml

  type node = pos * tag * t list

  let xmlbase_tag = (Xmlm.ns_xml, "base")

  let xmlbase_of_attr ~xmlbase attr =
    try
      let new_base = List.assoc xmlbase_tag attr in
      Some (Syndic_xml.resolve ~xmlbase (Uri.of_string new_base))
    with Not_found -> xmlbase

  let generate_catcher ?(namespaces = [""]) ?(attr_producer = [])
      ?(data_producer = []) ?leaf_producer maker =
    let in_namespaces ((prefix, _), _) = List.mem prefix namespaces in
    let get_attr_name (((_prefix, name), _) : Xmlm.attribute) = name in
    let get_attr_value ((_, value) : Xmlm.attribute) = value in
    let get_tag_name (((_prefix, name), _) : tag) = name in
    let get_attrs ((_, attrs) : tag) = attrs in
    let get_producer name map =
      try Some (List.assoc name map) with _ -> None
    in
    let rec catch_attr ~xmlbase acc pos = function
      | attr :: r -> (
        match get_producer (get_attr_name attr) attr_producer with
        | Some f when in_namespaces attr ->
            let acc = f ~xmlbase (get_attr_value attr) :: acc in
            catch_attr ~xmlbase acc pos r
        | _ -> catch_attr ~xmlbase acc pos r )
      | [] -> acc
    in
    let rec catch_datas ~xmlbase acc = function
      | Node (pos, tag, datas) :: r -> (
        match get_producer (get_tag_name tag) data_producer with
        | Some f when in_namespaces tag ->
            let acc = f ~xmlbase (pos, tag, datas) :: acc in
            catch_datas ~xmlbase acc r
        | _ -> catch_datas ~xmlbase acc r )
      | Data (pos, str) :: r -> (
        match leaf_producer with
        | Some f -> catch_datas ~xmlbase (f ~xmlbase pos str :: acc) r
        | None -> catch_datas ~xmlbase acc r )
      | [] -> acc
    in
    let generate ~xmlbase ((pos, tag, datas) : node) =
      (* The spec says that "The base URI for a URI reference appearing in any
         other attribute value, including default attribute values, is the base
         URI of the element bearing the attribute" so get xml:base first. *)
      let xmlbase = xmlbase_of_attr ~xmlbase (get_attrs tag) in
      let acc = catch_attr ~xmlbase [] pos (get_attrs tag) in
      maker ~pos (catch_datas ~xmlbase acc datas)
    in
    generate

  let dummy_of_xml ~ctor =
    let leaf_producer ~xmlbase _pos data = ctor ~xmlbase data in
    let head ~pos:_ = function [] -> ctor ~xmlbase:None "" | x :: _ -> x in
    generate_catcher ~leaf_producer head
end

(* Util *)

module Util = struct
  let find f l = try Some (List.find f l) with Not_found -> None

  exception Found of XML.t

  let recursive_find f root =
    let rec aux = function
      | [] -> None
      | x :: _ when f x -> raise (Found x)
      | XML.Node (_, _, x) :: r -> (
          aux x
          |> function
          | Some x -> raise (Found x) (* assert false ? *) | None -> aux r )
      | XML.Data _ :: r -> aux r
    in
    try aux [root] with Found x -> Some x | _ -> None

  let rec filter_map l f =
    match l with
    | [] -> []
    | x :: tl -> (
      match f x with None -> filter_map tl f | Some x -> x :: filter_map tl f )

  let rec take l n =
    match l with
    | [] -> []
    | e :: tl -> if n > 0 then e :: take tl (n - 1) else []

  let tag_is (((_prefix, name), _attrs) : XML.tag) = ( = ) name
  let attr_is (((_prefix, name), _value) : Xmlm.attribute) = ( = ) name
  let datas_has_leaf = List.exists (function XML.Data _ -> true | _ -> false)

  let get_leaf l =
    match find (function XML.Data _ -> true | _ -> false) l with
    | Some (XML.Data (_, s)) -> s
    | _ -> raise Not_found

  let get_attrs ((_, attrs) : XML.tag) = attrs
  let get_value ((_, value) : Xmlm.attribute) = value
  let get_attr_name (((_prefix, name), _) : Xmlm.attribute) = name
  let get_tag_name (((_prefix, name), _) : XML.tag) = name
  let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

  let only_whitespace s =
    let r = ref true in
    let i = ref 0 and len = String.length s in
    while !r && !i < len do
      r := is_space s.[!i] ;
      incr i
    done ;
    !r

  (* Output feeds to XML *)

  let add_attr name v_opt attr =
    match v_opt with None | Some "" -> attr | Some v -> (name, v) :: attr

  let add_attr_uri name v_opt attr =
    match v_opt with None -> attr | Some v -> (name, Uri.to_string v) :: attr

  let tag name = (("", name), [])
  let dummy_pos = (0, 0)

  (* Do smarter positions make sense? *)

  let node_data tag content =
    XML.Node (dummy_pos, tag, [XML.Data (dummy_pos, content)])

  let node_uri tag uri = node_data tag (Uri.to_string uri)

  let add_node_data tag c nodes =
    match c with
    | None -> nodes
    | Some content -> node_data tag content :: nodes

  let add_node_uri tag c nodes =
    match c with
    | None -> nodes
    | Some uri -> node_data tag (Uri.to_string uri) :: nodes

  (* Add to [nodes] those coming from mapping [f] on [els] *)
  let add_nodes_rev_map f els nodes =
    List.fold_left (fun nodes el -> f el :: nodes) nodes els

  let add_node_option f op nodes =
    match op with None -> nodes | Some v -> f v :: nodes
end
