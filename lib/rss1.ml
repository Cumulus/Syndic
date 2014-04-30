open Common.XML
open Common.Util

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

type textinput = {
  about: Uri.t;
  title: string;
  description: string;
  name: string;
  link: Uri.t;
}

type rdf = {
  channel: channel;
  image: image option;
  item: item list;
  textinput: textinput option;
}

module Error = struct
  include Common.Error
end

(* same <title> *)

let make_title (l : [< `TitleData of string] list) =
  let title = match find (fun (`TitleData _) -> true) l with
    | Some (`TitleData d) -> d
    | _ -> Error.raise_expectation Error.Data (Error.Tag "title")
  in title

let title_of_xml =
  let leaf_producer ctx data = `TitleData data in
  generate_catcher ~leaf_producer make_title

let make_name (l : [< `NameData of string] list) =
  let name = match find (fun (`NameData _) -> true) l with
    | Some (`NameData d) -> d
    | _ -> Error.raise_expectation Error.Data (Error.Tag "name")
  in name

let name_of_xml =
  let leaf_producer ctx data = `NameData data in
  generate_catcher ~leaf_producer make_name

let make_description (l : [< `DescriptionData of string] list) =
  let description = match find (function `DescriptionData _ -> true) l with
    | Some (`DescriptionData s) -> s
    | _ -> Error.raise_expectation Error.Data (Error.Tag "description")
  in description

let description_of_xml =
  let leaf_producer ctx data = `DescriptionData data in
  generate_catcher ~leaf_producer make_description

let make_image (l : [< `ImageData of Uri.t] list) =
  let image = match find (function `ImageData _ -> true) l with
    | Some (`ImageData u) -> u
    | _ -> Error.raise_expectation Error.Data (Error.Tag "image")
  in image

let image_of_xml =
  let attr_producer = [
    ("resource", (fun ctx attr -> `ImageData (Uri.of_string (get_value attr))));
  ] in
  generate_catcher ~attr_producer make_image

let make_link (l : [< `LinkData of Uri.t] list) =
  let link = match find (function `LinkData _ -> true) l with
    | Some (`LinkData u) -> u
    | _ -> Error.raise_expectation Error.Data (Error.Tag "link")
  in link

let link_of_xml =
  let leaf_producer ctx data = `LinkData (Uri.of_string data) in
  generate_catcher ~leaf_producer make_link

let make_url (l : [< `URLData of Uri.t] list) =
  let url = match find (function `URLData _ -> true) l with
    | Some (`URLData u) -> u
    | _ -> Error.raise_expectation Error.Data (Error.Tag "url")
  in url

let url_of_xml =
  let leaf_producer ctx data = `URLData (Uri.of_string data) in
  generate_catcher ~leaf_producer make_url


let make_li (l : [< `LiRessource of Uri.t] list) =
  let url = match find (function `LiRessource _ -> true) l with
    | Some (`LiRessource u) -> u
    | _ -> Error.raise_expectation (Error.Attr "ressource") (Error.Tag "li")
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
    | _ -> Error.raise_expectation (Error.Tag "rdf:Seq") (Error.Tag "items") (* Empty list or error ? *)
  in li

let items_of_xml =
  let data_producer = [
    ("Seq", (fun ctx a -> `ItemsSeq (seq_of_xml a)));
  ] in
  generate_catcher ~data_producer make_items

let make_textinput (l : [< `TextInputResource of Uri.t] list) =
  let url = match find (function `TextInputResource _ -> true) l with
    | Some (`TextInputResource u) -> u
    | _ -> Error.raise_expectation (Error.Attr "resource") (Error.Tag "textinput")
  in url

let textinput_of_xml =
  let attr_producer = [
    ("resource", (fun ctx attr -> `TextInputResource (Uri.of_string (get_value attr))));
  ] in
  generate_catcher ~attr_producer make_textinput

let make_channel (l : [< `ChannelTitle of string | `ChannelLink of Uri.t | `ChannelDescription of string | `ChannelImage of Uri.t | `ChannelItems of Uri.t list | `ChannelTextInput of Uri.t | `ChannelAbout of Uri.t ] list) =
  let about = match find (function `ChannelAbout _ -> true | _ -> false) l with
    | Some (`ChannelAbout u) -> u
    | _ -> Error.raise_expectation (Error.Attr "about") (Error.Tag "channel")
  in let title = match find (function `ChannelTitle _ -> true | _ -> false) l with
    | Some (`ChannelTitle s) -> s
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "channel")
  in let link = match find (function `ChannelLink _ -> true | _ -> false) l with
    | Some (`ChannelLink u) -> u
    | _ -> Error.raise_expectation (Error.Tag "link") (Error.Tag "channel")
  in let description = match find (function `ChannelDescription _ -> true | _ -> false) l with
    | Some (`ChannelDescription s) -> s
    | _ -> Error.raise_expectation (Error.Tag "description") (Error.Tag "channel")
  in let image = match find (function `ChannelImage _ -> true | _ -> false) l with
    | Some (`ChannelImage i) -> Some i
    | _ -> None
  in let items = match find (function `ChannelItems _ -> true | _ -> false) l with
    | Some (`ChannelItems l) -> l
    | _ -> Error.raise_expectation (Error.Tag "items") (Error.Tag "channel")
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
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "image")
  in let link = match find (function `ImageLink _ -> true | _ -> false) l with
    | Some (`ImageLink u) -> u
    | _ -> Error.raise_expectation (Error.Tag "link") (Error.Tag "image")
  in let url = match find (function `ImageURL _ -> true | _ -> false) l with
    | Some (`ImageURL u) -> u
    | _ -> Error.raise_expectation (Error.Tag "url") (Error.Tag "image")
  in let about = match find (function `ImageAbout _ -> true | _ -> false) l with
    | Some (`ImageAbout a) -> a
    | _ -> Error.raise_expectation (Error.Attr "about") (Error.Tag "image")
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
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "item")
  in let link = match find (function `ItemLink _ -> true | _ -> false) l with
    | Some (`ItemLink u) -> u
    | _ -> Error.raise_expectation (Error.Tag "link") (Error.Tag "item")
  in let description = match find (function `ItemDescription _ -> true | _ -> false) l with
    | Some (`ItemDescription d) -> Some d
    | _ -> None
  in let about = match find (function `ItemAbout _ -> true | _ -> false) l with
    | Some (`ItemAbout u) -> u
    | _ -> Error.raise_expectation (Error.Attr "about") (Error.Tag "item")
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

let make_textinput' (l : [< `TextInputAbout of Uri.t | `TextInputTitle of string | `TextInputDescription of string | `TextInputName of string | `TextInputLink of Uri.t] list) = 
  let title = match find (function `TextInputTitle _ -> true | _ -> false) l with
    | Some (`TextInputTitle s) -> s
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "textinput")
  in let description = match find (function `TextInputDescription _ -> true | _ -> false) l with
    | Some (`TextInputDescription s) -> s
    | _ -> Error.raise_expectation (Error.Tag "description") (Error.Tag "textinput")
  in let name = match find (function `TextInputName _ -> true | _ -> false) l with
    | Some (`TextInputName n) -> n
    | _ -> Error.raise_expectation (Error.Tag "name") (Error.Tag "textinput")
  in let link = match find (function `TextInputLink _ -> true | _ -> false) l with
    | Some (`TextInputLink u) -> u
    | _ -> Error.raise_expectation (Error.Tag "link") (Error.Tag "textinput")
  in let about = match find (function `TextInputAbout _ -> true | _ -> false) l with
    | Some (`TextInputAbout u) -> u
    | _ -> Error.raise_expectation (Error.Attr "about") (Error.Tag "textinput")
  in ({ about; title; description; name; link; } : textinput)

let textinput_of_xml' =
  let data_producer = [
    ("title", (fun ctx a -> `TextInputTitle (title_of_xml a)));
    ("description", (fun ctx a -> `TextInputDescription (description_of_xml a)));
    ("name", (fun ctx a -> `TextInputName (name_of_xml a)));
    ("link", (fun ctx a -> `TextInputLink (link_of_xml a)));
  ] in
  let attr_producer = [
    ("about", (fun ctx attr -> `TextInputAbout (Uri.of_string (get_value attr))))
  ] in
  generate_catcher ~attr_producer ~data_producer make_textinput'

let make_rdf (l : [< `RDFChannel of channel | `RDFImage of image | `RDFItem of item | `RDFTextInput of textinput] list) =
  let channel = match find (function `RDFChannel _ -> true | _ -> false) l with
    | Some (`RDFChannel channel) -> channel
    | _ -> Error.raise_expectation (Error.Tag "channel") (Error.Tag "RDF")
  in let image = match find (function `RDFImage _ -> true | _ -> false) l with
    | Some (`RDFImage image) -> Some image
    | _ -> None
  in let textinput = match find (function `RDFTextInput _ -> true | _ -> false) l with
    | Some (`RDFTextInput textinput) -> Some textinput
    | _ -> None
  in let item = List.fold_left (fun acc -> function `RDFItem x -> x :: acc | _ -> acc) [] l
  in ({ channel; image; item; textinput } : rdf)

let rdf_of_xml =
  let data_producer = [
    ("channel", (fun ctx a -> `RDFChannel (channel_of_xml a)));
    ("image", (fun ctx a -> `RDFImage (image_of_xml' a)));
    ("item", (fun ctx a -> `RDFItem (item_of_xml a)));
    ("textinput", (fun ctx a -> `RDFTextInput (textinput_of_xml' a)))
  ] in
  generate_catcher ~data_producer make_rdf

let analyze input =
  let el tag datas = Node (tag, datas) in
  let data data = Leaf data in
  let (_, tree) = Xmlm.input_doc_tree ~el ~data input in
  let aux = function
    | Node (tag, datas) when tag_is tag "RDF" -> rdf_of_xml (tag, datas)
    | _ -> Error.raise_expectation (Error.Tag "RDF") Error.Root
  in aux tree
