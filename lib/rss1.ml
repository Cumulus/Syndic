type tree =
  | Node of Xmlm.tag * tree list
  | Leaf of string

type channel = {
  about: Uri.t; (* must be uniq *)
  title: string;
  link: Uri.t;
  description: string;
  image: Uri.t option;
  items: Uri.t list;
  textinput: Uri.t option;
}

type image = {
  about: Uri.t;
  title: string;
  url: Uri.t;
  link: Uri.t;
}

type item = {
  about: Uri.t;
  title: string;
  link: Uri.t;
  description: string option;
}

type rdf = {
  channel: channel;
  image: image option;
  item: item list;
}

(* same Abstraction *)

type opts_neturl = {
  schemes: (string, Neturl.url_syntax) Hashtbl.t;
  base_syntax: Neturl.url_syntax;
  accept_8bits: bool;
  enable_fragment: bool;
}

(* same Exception *)

type expected_type =
  | EAttr of string
  | ETag of string
  | EData

exception Expected of expected_type * expected_type
exception ExpectedLeaf

let string_of_expectation (a, b) =
  let string_of_expected_type = function
    | EAttr a -> a ^ "="
    | ETag a -> "<" ^ a ^ ">"
    | EData -> "data"
  in let buffer = Buffer.create 16 in
  Buffer.add_string buffer "Expected ";
  Buffer.add_string buffer (string_of_expected_type a);
  Buffer.add_string buffer " in ";
  Buffer.add_string buffer (string_of_expected_type b);
  Buffer.contents buffer

exception Malformed_URL of string

let raise_expectation data in_data = raise (Expected (data, in_data))

(* same Util *)

let find f l = try Some (List.find f l) with Not_found -> None

let url_of_string opts_neturl str =
  try Neturl.parse_url
    ~schemes:opts_neturl.schemes
    ~base_syntax:opts_neturl.base_syntax
    ~accept_8bits:opts_neturl.accept_8bits
    ~enable_fragment:opts_neturl.enable_fragment
    str
  with Neturl.Malformed_URL -> raise (Malformed_URL str)

let tag_is (((prefix, name), attrs) : Xmlm.tag) = (=) name
let attr_is (((prefix, name), value) : Xmlm.attribute) = (=) name
let datas_has_leaf = List.exists (function | Leaf _ -> true | _ -> false)
let get_leaf l  = match find (function Leaf _ -> true | _ -> false) l with
  | Some (Leaf s) -> s
  | _ -> raise ExpectedLeaf
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

let generate_catcher
  ?(attr_producer=[])
  ?(data_producer=[])
  ?leaf_producer maker =
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

(* same <title> *)

let make_title (l : [< `TitleData of string] list) =
  let title = match find (fun (`TitleData _) -> true) l with
    | Some (`TitleData d) -> d
    | _ -> raise_expectation EData (ETag "title")
  in title

let title_of_xml =
  let leaf_producer ctx data = `TitleData data in
  generate_catcher ~leaf_producer make_title

let make_description (l : [< `DescriptionData of string] list) =
  let description = match find (function `DescriptionData _ -> true) l with
    | Some (`DescriptionData s) -> s
    | _ -> raise_expectation EData (ETag "description")
  in description

let description_of_xml =
  let leaf_producer ctx data = `DescriptionData data in
  generate_catcher ~leaf_producer make_description

let make_image (l : [< `ImageData of Uri.t] list) =
  let image = match find (function `ImageData _ -> true) l with
    | Some (`ImageData u) -> u
    | _ -> raise_expectation EData (ETag "image")
  in image

let image_of_xml =
  let attr_producer = [
    ("resource", (fun ctx attr -> `ImageData (Uri.of_string (get_value attr))));
  ] in
  generate_catcher ~attr_producer make_image

let make_link (l : [< `LinkData of Uri.t] list) =
  let link = match find (function `LinkData _ -> true) l with
    | Some (`LinkData u) -> u
    | _ -> raise_expectation EData (ETag "link")
  in link

let link_of_xml =
  let leaf_producer ctx data = `LinkData (Uri.of_string data) in
  generate_catcher ~leaf_producer make_link

let make_url (l : [< `URLData of Uri.t] list) =
  let url = match find (function `URLData _ -> true) l with
    | Some (`URLData u) -> u
    | _ -> raise_expectation EData (ETag "url")
  in url

let url_of_xml =
  let leaf_producer ctx data = `URLData (Uri.of_string data) in
  generate_catcher ~leaf_producer make_url


let make_li (l : [< `LiRessource of Uri.t] list) =
  let url = match find (function `LiRessource _ -> true) l with
    | Some (`LiRessource u) -> u
    | _ -> raise_expectation (EAttr "ressource") (ETag "li")
  in url

let li_of_xml =
  let attr_producer = [
    ("resource", (fun ctx attr -> `LiRessource (Uri.of_string (get_value attr))));
  ] in
  generate_catcher ~attr_producer make_li

let make_seq (l : [< `SeqLi of Uri.t] list) =
  let li = List.map (function `SeqLi u -> u) l
  in li

let seq_of_xml =
  let data_producer = [
    ("li", (fun ctx a -> `SeqLi (li_of_xml a)));
  ] in
  generate_catcher ~data_producer make_seq

let make_items (l : [< `ItemsSeq of Uri.t list] list) =
  let li = match find (function `ItemsSeq _ -> true) l with
    | Some (`ItemsSeq l) -> l
    | _ -> raise_expectation (ETag "rdf:Seq") (ETag "items") (* Empty list or error ? *)
  in li

let items_of_xml =
  let data_producer = [
    ("Seq", (fun ctx a -> `ItemsSeq (seq_of_xml a)));
  ] in
  generate_catcher ~data_producer make_items

let make_textinput (l : [< `TextInputResource of Uri.t] list) =
  let url = match find (function `TextInputResource _ -> true) l with
    | Some (`TextInputResource u) -> u
    | _ -> raise_expectation (EAttr "resource") (ETag "textinput")
  in url

let textinput_of_xml =
  let attr_producer = [
    ("resource", (fun ctx attr -> `TextInputResource (Uri.of_string (get_value attr))));
  ] in
  generate_catcher ~attr_producer make_textinput

let make_channel (l : [< `ChannelTitle of string | `ChannelLink of Uri.t | `ChannelDescription of string | `ChannelImage of Uri.t | `ChannelItems of Uri.t list | `ChannelTextInput of Uri.t | `ChannelAbout of Uri.t ] list) =
  let about = match find (function `ChannelAbout _ -> true | _ -> false) l with
    | Some (`ChannelAbout u) -> u
    | _ -> raise_expectation (EAttr "about") (ETag "channel")
  in let title = match find (function `ChannelTitle _ -> true | _ -> false) l with
    | Some (`ChannelTitle s) -> s
    | _ -> raise_expectation (ETag "title") (ETag "channel")
  in let link = match find (function `ChannelLink _ -> true | _ -> false) l with
    | Some (`ChannelLink u) -> u
    | _ -> raise_expectation (ETag "link") (ETag "channel")
  in let description = match find (function `ChannelDescription _ -> true | _ -> false) l with
    | Some (`ChannelDescription s) -> s
    | _ -> raise_expectation (ETag "description") (ETag "channel")
  in let image = match find (function `ChannelImage _ -> true | _ -> false) l with
    | Some (`ChannelImage i) -> Some i
    | _ -> None
  in let items = match find (function `ChannelItems _ -> true | _ -> false) l with
    | Some (`ChannelItems l) -> l
    | _ -> raise_expectation (ETag "items") (ETag "channel")
  in let textinput = match find (function `ChannelTextInput _ -> true | _ -> false) l with
    | Some (`ChannelTextInput u) -> Some u
    | _ -> None
  in ({ about; title; link; description; image; items; textinput } : channel)

let channel_of_xml =
  let data_producer = [
    ("title", (fun ctx a -> `ChannelTitle (title_of_xml a)));
    ("link", (fun ctx a -> `ChannelLink (link_of_xml a)));
    ("description", (fun ctx a -> `ChannelDescription (description_of_xml a)));
    ("image", (fun ctx a -> `ChannelImage (image_of_xml a)));
    ("items", (fun ctx a -> `ChannelItems (items_of_xml a)));
    ("textinput", (fun ctx a -> `ChannelTextInput (textinput_of_xml a)));
  ] in
  let attr_producer = [
    ("about", (fun ctx attr -> `ChannelAbout (Uri.of_string (get_value attr))));
  ] in
  generate_catcher ~attr_producer ~data_producer make_channel

let make_image' (l : [< `ImageTitle of string | `ImageLink of Uri.t | `ImageURL of Uri.t | `ImageAbout of Uri.t] list) =
  let title = match find (function `ImageTitle _ -> true | _ -> false) l with
    | Some (`ImageTitle t) -> t
    | _ -> raise_expectation (ETag "title") (ETag "image")
  in let link = match find (function `ImageLink _ -> true | _ -> false) l with
    | Some (`ImageLink u) -> u
    | _ -> raise_expectation (ETag "link") (ETag "image")
  in let url = match find (function `ImageURL _ -> true | _ -> false) l with
    | Some (`ImageURL u) -> u
    | _ -> raise_expectation (ETag "url") (ETag "image")
  in let about = match find (function `ImageAbout _ -> true | _ -> false) l with
    | Some (`ImageAbout a) -> a
    | _ -> raise_expectation (EAttr "about") (ETag "image")
  in ({ about; title; url; link; } : image)

let image_of_xml' =
  let data_producer = [
    ("title", (fun ctx a -> `ImageTitle (title_of_xml a)));
    ("link" , (fun ctx a -> `ImageLink (link_of_xml a)));
    ("url", (fun ctx a -> `ImageURL (url_of_xml a)));
  ] in
  let attr_producer = [
    ("about", (fun ctx attr -> `ImageAbout (Uri.of_string (get_value attr))));
  ] in
  generate_catcher ~attr_producer ~data_producer make_image'

let make_item (l : [< `ItemTitle of string | `ItemLink of Uri.t | `ItemDescription of string | `ItemAbout of Uri.t] list) =
  let title = match find (function `ItemTitle _ -> true | _ -> false) l with
    | Some (`ItemTitle t) -> t
    | _ -> raise_expectation (ETag "title") (ETag "item")
  in let link = match find (function `ItemLink _ -> true | _ -> false) l with
    | Some (`ItemLink u) -> u
    | _ -> raise_expectation (ETag "link") (ETag "item")
  in let description = match find (function `ItemDescription _ -> true | _ -> false) l with
    | Some (`ItemDescription d) -> Some d
    | _ -> None
  in let about = match find (function `ItemAbout _ -> true | _ -> false) l with
    | Some (`ItemAbout u) -> u
    | _ -> raise_expectation (EAttr "about") (ETag "item")
  in ({ about; title; link; description; } : item)

let item_of_xml =
  let data_producer = [
    ("title", (fun ctx a -> `ItemTitle (title_of_xml a)));
    ("link", (fun ctx a -> `ItemLink (link_of_xml a)));
    ("description", (fun ctx a -> `ItemDescription (description_of_xml a)));
  ] in
  let attr_producer = [
    ("about", (fun ctx attr -> `ItemAbout (Uri.of_string (get_value attr))));
  ] in
  generate_catcher ~attr_producer ~data_producer make_item

let make_rdf (l : [< `RDFChannel of channel | `RDFImage of image | `RDFItem of item] list) =
  let channel = match find (function `RDFChannel _ -> true | _ -> false) l with
    | Some (`RDFChannel channel) -> channel
    | _ -> raise_expectation (ETag "channel") (ETag "RDF")
  in let image = match find (function `RDFImage _ -> true | _ -> false) l with
    | Some (`RDFImage image) -> Some image
    | _ -> None
  in let item = List.fold_left (fun acc -> function `RDFItem x -> x :: acc | _ -> acc) [] l
  in ({ channel; image; item; } : rdf)

let rdf_of_xml =
  let data_producer = [
    ("channel", (fun ctx a -> `RDFChannel (channel_of_xml a)));
    ("image", (fun ctx a -> `RDFImage (image_of_xml' a)));
    ("item", (fun ctx a -> `RDFItem (item_of_xml a)));
  ] in
  generate_catcher ~data_producer make_rdf

let analyze input =
  let el tag datas = Node (tag, datas) in
  let data data = Leaf data in
  let (_, tree) = Xmlm.input_doc_tree ~el ~data input in
  let aux = function
    | Node (tag, datas) when tag_is tag "RDF" -> rdf_of_xml (tag, datas)
    | _ -> raise_expectation (ETag "RDF") (ETag "[root]")
  in aux tree
