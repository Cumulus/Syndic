(* XML *)

module XML = struct
  include Syndic_xml

  exception Ignore_namespace

  let generate_catcher
      ?(namespaces=[""])
      ?(attr_producer=[])
      ?(data_producer=[])
      ?leaf_producer maker =
    let in_namespaces ((prefix, _), _) = List.mem prefix namespaces in
    let get_attr_name (((prefix, name), _) : Xmlm.attribute) = name in
    let get_attr_value ((_, value) : Xmlm.attribute) = value in
    let get_tag_name (((prefix, name), _) : Xmlm.tag) = name in
    let get_attrs ((_, attrs) : Xmlm.tag) = attrs in
    let get_producer name map =
      try Some (List.assoc name map)
      with _ -> None
    in
    let rec catch_attr acc pos = function
      | attr :: r -> begin
          match get_producer (get_attr_name attr) attr_producer with
          | Some f when in_namespaces attr ->
              catch_attr ((f acc pos (get_attr_value attr)) :: acc) pos r
          | _ -> catch_attr acc pos r end
      | [] -> acc
    in
    let rec catch_datas acc = function
      | Node (pos, tag, datas) :: r ->
        begin match get_producer (get_tag_name tag) data_producer with
          | Some f when in_namespaces tag ->
              catch_datas ((f acc (pos, tag, datas)) :: acc) r
          | _ -> catch_datas acc r end
      | Data (pos, str) :: r ->
        begin match leaf_producer with
          | Some f -> catch_datas ((f acc pos str) :: acc) r
          | None -> catch_datas acc r end
      | [] -> acc
    in
    let generate (pos, tag, datas) =
      maker (catch_attr (catch_datas [] datas) pos (get_attrs tag))
    in generate

  let dummy_of_xml ~ctor =
    let leaf_producer ctx pos data = ctor data in
    generate_catcher ~leaf_producer (function [] -> (ctor "") | x :: r -> x)
end

(* Util *)

module Util = struct
  let find f l = try Some (List.find f l) with Not_found -> None

  let rec filter_map l f =
    match l with
    | [] -> []
    | x :: tl -> match f x with
                | None -> filter_map tl f
                | Some x -> x :: filter_map tl f

  let tag_is (((prefix, name), attrs) : Xmlm.tag) = (=) name
  let attr_is (((prefix, name), value) : Xmlm.attribute) = (=) name
  let datas_has_leaf = List.exists (function | XML.Data _ -> true | _ -> false)
  let get_leaf l  = match find (function XML.Data _ -> true | _ -> false) l with
    | Some (XML.Data (_, s)) -> s
    | _ -> raise Not_found
  let get_attrs ((_, attrs) : Xmlm.tag) = attrs
  let get_value ((_, value) : Xmlm.attribute) = value
  let get_attr_name (((prefix, name), _) : Xmlm.attribute) = name
  let get_tag_name (((prefix, name), _) : Xmlm.tag) = name

  let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

  let only_whitespace s =
    let r = ref true in
    let i = ref 0 and len = String.length s in
    while !r && !i < len do r := is_space s.[!i]; incr i done;
    !r


  (* Output feeds to XML *)

  let add_attr name v_opt attr =
    match v_opt with
    | None -> attr
    | Some v -> (name, v) :: attr

  let add_attr_uri name v_opt attr =
    match v_opt with
    | None -> attr
    | Some v -> (name, Uri.to_string v) :: attr

  let tag name = (("", name), [])

  let dummy_pos = (0,0) (* Do smarter positions make sense? *)

  let node_data tag content =
    XML.Node(dummy_pos, tag, [XML.Data(dummy_pos, content)])

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
    match op with
    | None -> nodes
    | Some v -> f v :: nodes

end
