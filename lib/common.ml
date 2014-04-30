(* XML *)

module XML = struct
  type tree =
  | Node of Xmlm.tag * tree list
  | Leaf of string

  let generate_catcher
    ?(attr_producer=[])
    ?(data_producer=[])
    ?leaf_producer maker =
    let get_attr_name (((prefix, name), _) : Xmlm.attribute) = name in
    let get_tag_name (((prefix, name), _) : Xmlm.tag) = name in
    let get_attrs ((_, attrs) : Xmlm.tag) = attrs in
    let get_producer name map =
      try Some (List.assoc name map)
      with _ -> None
    in
    let rec catch_attr acc = function
      | attr :: r -> begin match get_producer (get_attr_name attr) attr_producer with
        | Some f -> catch_attr ((f acc attr) :: acc) r
        | None -> catch_attr acc r end
      | [] -> acc
    in
    let rec catch_datas acc = function
      | (Node (tag, datas)) :: r ->
        begin match get_producer (get_tag_name tag) data_producer with
        | Some f -> catch_datas ((f acc (tag, datas)) :: acc) r
        | None -> catch_datas acc r end
      | (Leaf str) :: r ->
        begin match leaf_producer with
        | Some f -> catch_datas ((f acc str) :: acc) r
        | None -> catch_datas acc r end
      | [] -> acc
    in
    let generate (tag, datas) =
      maker (catch_attr (catch_datas [] datas) (get_attrs tag))
    in generate
end

(* Exception *)

module Error = struct
  type expected_type =
    | Attr of string
    | Tag of string
    | Data
    | Root

  exception Expected of expected_type * expected_type
  exception ExpectedLeaf

  let string_of_expectation (a, b) =
    let string_of_expected_type = function
      | Attr a -> a ^ "="
      | Tag a -> "<" ^ a ^ ">"
      | Data -> "data"
      | Root -> "root"
    in let buffer = Buffer.create 16 in
    Buffer.add_string buffer "Expected ";
    Buffer.add_string buffer (string_of_expected_type a);
    Buffer.add_string buffer " in ";
    Buffer.add_string buffer (string_of_expected_type b);
    Buffer.contents buffer

  let raise_expectation data in_data = raise (Expected (data, in_data))

  exception Malformed_URL of string
end

(* Util *)

module Util = struct
  type opts_neturl = {
    schemes: (string, Neturl.url_syntax) Hashtbl.t;
    base_syntax: Neturl.url_syntax;
    accept_8bits: bool;
    enable_fragment: bool;
  }

  let find f l = try Some (List.find f l) with Not_found -> None

  let url_of_string opts_neturl str =
    try Neturl.parse_url
      ~schemes:opts_neturl.schemes
      ~base_syntax:opts_neturl.base_syntax
      ~accept_8bits:opts_neturl.accept_8bits
      ~enable_fragment:opts_neturl.enable_fragment
      str
    with Neturl.Malformed_URL -> raise (Error.Malformed_URL str)

  let tag_is (((prefix, name), attrs) : Xmlm.tag) = (=) name
  let attr_is (((prefix, name), value) : Xmlm.attribute) = (=) name
  let datas_has_leaf = List.exists (function | XML.Leaf _ -> true | _ -> false)
  let get_leaf l  = match find (function XML.Leaf _ -> true | _ -> false) l with
    | Some (XML.Leaf s) -> s
    | _ -> raise Error.ExpectedLeaf
  let get_attrs ((_, attrs) : Xmlm.tag) = attrs
  let get_value ((_, value) : Xmlm.attribute) = value
  let get_attr_name (((prefix, name), _) : Xmlm.attribute) = name
  let get_tag_name (((prefix, name), _) : Xmlm.tag) = name

  let make_opts_neturl
    ?(schemes = Neturl.common_url_syntax)
    ?(base_syntax = Hashtbl.find Neturl.common_url_syntax "http")
    ?(accept_8bits = true)
    ?(enable_fragment = true) () =
  {
    schemes;
    base_syntax;
    accept_8bits;
    enable_fragment;
  }
end

