module XML = Syndic_common.XML
open Syndic_common.Util

module Error = Syndic_error

type title = string
type title' = [ `Data of string ]

let make_title (l : [< title' ] list) =
  let title = match find (fun (`Data _) -> true) l with
    | Some (`Data d) -> d
    | _ -> Error.raise_expectation Error.Data (Error.Tag "title")
  in title

let title_of_xml, title_of_xml' =
  let leaf_producer ctx data = `Data data in
  XML.generate_catcher ~leaf_producer make_title,
  XML.generate_catcher ~leaf_producer (fun x -> x)

type name = string
type name' = [`Data of string]

let make_name (l : [< name' ] list) =
  let name = match find (fun (`Data _) -> true) l with
    | Some (`Data d) -> d
    | _ -> Error.raise_expectation Error.Data (Error.Tag "name")
  in name

let name_of_xml, name_of_xml' =
  let leaf_producer ctx data = `Data data in
  XML.generate_catcher ~leaf_producer make_name,
  XML.generate_catcher ~leaf_producer (fun x -> x)

type description = string
type description' = [ `Data of string ]

let make_description (l : [< description' ] list) =
  let description = match find (function `Data _ -> true) l with
    | Some (`Data s) -> s
    | _ -> Error.raise_expectation Error.Data (Error.Tag "description")
  in description

let description_of_xml, description_of_xml' =
  let leaf_producer ctx data = `Data data in
  XML.generate_catcher ~leaf_producer make_description,
  XML.generate_catcher ~leaf_producer (fun x -> x)

type channel_image = Uri.t
type channel_image' = [ `URI of string ]

let make_channel_image (l : [< channel_image' ] list) =
  let image = match find (function `URI _ -> true) l with
    | Some (`URI u) -> (Uri.of_string u)
    | _ -> Error.raise_expectation (Error.Attr "resource") (Error.Tag "image")
  in image

let channel_image_of_xml, channel_image_of_xml' =
  let attr_producer = [
    ("resource", (fun ctx a -> `URI a));
  ] in
  XML.generate_catcher ~attr_producer make_channel_image,
  XML.generate_catcher ~attr_producer (fun x -> x)

type link = Uri.t
type link' = [ `URI of string ]

let make_link (l : [< link' ] list) =
  let link = match find (function `URI _ -> true) l with
    | Some (`URI u) -> (Uri.of_string u)
    | _ -> Error.raise_expectation Error.Data (Error.Tag "link")
  in link

let link_of_xml, link_of_xml' =
  let leaf_producer ctx data = `URI data in
  XML.generate_catcher ~leaf_producer make_link,
  XML.generate_catcher ~leaf_producer (fun x -> x)

type url = Uri.t
type url' = [ `URI of string ]

let make_url (l : [< url' ] list) =
  let url = match find (function `URI _ -> true) l with
    | Some (`URI u) -> Uri.of_string u
    | _ -> Error.raise_expectation Error.Data (Error.Tag "url")
  in url

let url_of_xml, url_of_xml' =
  let leaf_producer ctx data = `URI data in
  XML.generate_catcher ~leaf_producer make_url,
  XML.generate_catcher ~leaf_producer (fun x -> x)

type li = Uri.t
type li' = [ `URI of string ]

let make_li (l : [< li' ] list) =
  let url = match find (function `URI _ -> true) l with
    | Some (`URI u) -> (Uri.of_string u)
    | _ -> Error.raise_expectation (Error.Attr "ressource") (Error.Tag "li")
  in url

let li_of_xml, li_of_xml' =
  let attr_producer = [
    ("resource", (fun ctx a -> `URI a));
  ] in
  XML.generate_catcher ~attr_producer make_li,
  XML.generate_catcher ~attr_producer (fun x -> x)

type seq = li list
type seq' = [ `Li of li ]

let make_seq (l : [< seq' ] list) =
  let li = List.map (function `Li u -> u) l
  in li

let seq_of_xml =
  let data_producer = [
    ("li", (fun ctx a -> `Li (li_of_xml a)));
  ] in
  XML.generate_catcher ~data_producer make_seq

let seq_of_xml' =
  let data_producer = [
    ("li", (fun ctx a -> `Li (li_of_xml' a)));
  ] in
  XML.generate_catcher ~data_producer (fun x -> x)

type items = seq
type items' = [ `Seq of seq ]

let make_items (l : [< items' ] list) =
  let li = match find (function `Seq _ -> true) l with
    | Some (`Seq l) -> l
    | _ ->
      Error.raise_expectation
        (Error.Tag "rdf:Seq")
        (Error.Tag "items") (* Empty list or error ? *)
  in li

let items_of_xml =
  let data_producer = [
    ("Seq", (fun ctx a -> `Seq (seq_of_xml a)));
  ] in
  XML.generate_catcher ~data_producer make_items

let items_of_xml' =
  let data_producer = [
    ("Seq", (fun ctx a -> `Seq (seq_of_xml' a)));
  ] in
  XML.generate_catcher ~data_producer (fun x -> x)

type channel_textinput = Uri.t
type channel_textinput' = [ `URI of string ]

let make_textinput (l : [< channel_textinput' ] list) =
  let url = match find (function `URI _ -> true) l with
    | Some (`URI u) -> (Uri.of_string u)
    | _ -> Error.raise_expectation
             (Error.Attr "resource")
             (Error.Tag "textinput")
  in url

let channel_textinput_of_xml, channel_textinput_of_xml' =
  let attr_producer = [
    ("resource", (fun ctx a -> `URI a));
  ] in
  XML.generate_catcher ~attr_producer make_textinput,
  XML.generate_catcher ~attr_producer (fun x -> x)

type channel =
  {
    about: Uri.t; (* must be uniq *)
    title: title;
    link: link;
    description: description;
    image: channel_image option;
    items: items;
    textinput: channel_textinput option;
  }

type channel' = [
  | `Title of title
  | `Link of link
  | `Description of description
  | `Image of channel_image
  | `Items of items
  | `TextInput of channel_textinput
  | `About of string
]

let make_channel (l : [< channel' ] list) =
  let about = match find (function `About _ -> true | _ -> false) l with
    | Some (`About u) -> (Uri.of_string u)
    | _ -> Error.raise_expectation (Error.Attr "about") (Error.Tag "channel")
  in let title = match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title s) -> s
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "channel")
  in let link = match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link u) -> u
    | _ -> Error.raise_expectation (Error.Tag "link") (Error.Tag "channel")
  in let description =
    match find (function `Description _ -> true | _ -> false) l with
    | Some (`Description s) -> s
    | _ -> Error.raise_expectation
             (Error.Tag "description")
             (Error.Tag "channel")
  in let image = match find (function `Image _ -> true | _ -> false) l with
    | Some (`Image i) -> Some i
    | _ -> None
  in let items = match find (function `Items _ -> true | _ -> false) l with
    | Some (`Items l) -> l
    | _ -> Error.raise_expectation
             (Error.Tag "items")
             (Error.Tag "channel")
  in let textinput =
    match find (function `TextInput _ -> true | _ -> false) l with
    | Some (`TextInput u) -> Some u
    | _ -> None
  in ({ about; title; link; description; image; items; textinput } : channel)

let channel_of_xml =
  let data_producer = [
    ("title", (fun ctx a -> `Title (title_of_xml a)));
    ("link", (fun ctx a -> `Link (link_of_xml a)));
    ("description", (fun ctx a -> `Description (description_of_xml a)));
    ("image", (fun ctx a -> `Image (channel_image_of_xml a)));
    ("items", (fun ctx a -> `Items (items_of_xml a)));
    ("textinput", (fun ctx a -> `TextInput (channel_textinput_of_xml a)));
  ] in
  let attr_producer = [
    ("about", (fun ctx a -> `About a));
  ] in
  XML.generate_catcher ~attr_producer ~data_producer make_channel

let channel_of_xml' =
  let data_producer = [
    ("title", (fun ctx a -> `Title (title_of_xml' a)));
    ("link", (fun ctx a -> `Link (link_of_xml' a)));
    ("description", (fun ctx a -> `Description (description_of_xml' a)));
    ("image", (fun ctx a -> `Image (channel_image_of_xml' a)));
    ("items", (fun ctx a -> `Items (items_of_xml' a)));
    ("textinput", (fun ctx a -> `TextInput (channel_textinput_of_xml' a)));
  ] in
  let attr_producer = [
    ("about", (fun ctx a -> `About a));
  ] in
  XML.generate_catcher ~attr_producer ~data_producer (fun x -> x)

type image =
  {
    about: Uri.t;
    title: title;
    url: url;
    link: link;
  }

type image' = [
  | `Title of title
  | `Link of link
  | `URL of url
  | `About of string
]

let make_image (l : [< image' ] list) =
  let title = match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title t) -> t
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "image")
  in let link = match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link u) -> u
    | _ -> Error.raise_expectation (Error.Tag "link") (Error.Tag "image")
  in let url = match find (function `URL _ -> true | _ -> false) l with
    | Some (`URL u) -> u
    | _ -> Error.raise_expectation (Error.Tag "url") (Error.Tag "image")
  in let about = match find (function `About _ -> true | _ -> false) l with
    | Some (`About a) -> (Uri.of_string a)
    | _ -> Error.raise_expectation (Error.Attr "about") (Error.Tag "image")
  in ({ about; title; url; link; } : image)

let image_of_xml =
  let data_producer = [
    ("title", (fun ctx a -> `Title (title_of_xml a)));
    ("link" , (fun ctx a -> `Link (link_of_xml a)));
    ("url", (fun ctx a -> `URL (url_of_xml a)));
  ] in
  let attr_producer = [
    ("about", (fun ctx a -> `About a));
  ] in
  XML.generate_catcher ~attr_producer ~data_producer make_image

let image_of_xml' =
  let data_producer = [
    ("title", (fun ctx a -> `Title (title_of_xml' a)));
    ("link" , (fun ctx a -> `Link (link_of_xml' a)));
    ("url", (fun ctx a -> `URL (url_of_xml' a)));
  ] in
  let attr_producer = [
    ("about", (fun ctx a -> `About a));
  ] in
  XML.generate_catcher ~attr_producer ~data_producer (fun x -> x)

type item =
  {
    about: Uri.t;
    title: title;
    link: link;
    description: description option;
  }

type item' = [
  | `Title of title
  | `Link of link
  | `Description of description
  | `About of string
]

let make_item (l : [< item' ] list) =
  let title = match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title t) -> t
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "item")
  in let link = match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link u) -> u
    | _ -> Error.raise_expectation (Error.Tag "link") (Error.Tag "item")
  in let description =
    match find (function `Description _ -> true | _ -> false) l with
    | Some (`Description d) -> Some d
    | _ -> None
  in let about = match find (function `About _ -> true | _ -> false) l with
    | Some (`About u) -> (Uri.of_string u)
    | _ -> Error.raise_expectation (Error.Attr "about") (Error.Tag "item")
  in ({ about; title; link; description; } : item)

let item_of_xml =
  let data_producer = [
    ("title", (fun ctx a -> `Title (title_of_xml a)));
    ("link", (fun ctx a -> `Link (link_of_xml a)));
    ("description", (fun ctx a -> `Description (description_of_xml a)));
  ] in
  let attr_producer = [
    ("about", (fun ctx a -> `About a));
  ] in
  XML.generate_catcher ~attr_producer ~data_producer make_item

let item_of_xml' =
  let data_producer = [
    ("title", (fun ctx a -> `Title (title_of_xml' a)));
    ("link", (fun ctx a -> `Link (link_of_xml' a)));
    ("description", (fun ctx a -> `Description (description_of_xml' a)));
  ] in
  let attr_producer = [
    ("about", (fun ctx a -> `About a));
  ] in
  XML.generate_catcher ~attr_producer ~data_producer (fun x -> x)

type textinput =
  {
    about: Uri.t;
    title: title;
    description: description;
    name: name;
    link: link;
  }

type textinput' = [
  | `About of string
  | `Title of title
  | `Description of description
  | `Name of name
  | `Link of link
]

let make_textinput (l : [< textinput' ] list) =
  let title = match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title s) -> s
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "textinput")
  in let description =
    match find (function `Description _ -> true | _ -> false) l with
    | Some (`Description s) -> s
    | _ -> Error.raise_expectation
             (Error.Tag "description")
             (Error.Tag "textinput")
  in let name = match find (function `Name _ -> true | _ -> false) l with
    | Some (`Name n) -> n
    | _ -> Error.raise_expectation (Error.Tag "name") (Error.Tag "textinput")
  in let link = match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link u) -> u
    | _ -> Error.raise_expectation (Error.Tag "link") (Error.Tag "textinput")
  in let about = match find (function `About _ -> true | _ -> false) l with
    | Some (`About u) -> (Uri.of_string u)
    | _ -> Error.raise_expectation (Error.Attr "about") (Error.Tag "textinput")
  in ({ about; title; description; name; link; } : textinput)

let textinput_of_xml =
  let data_producer = [
    ("title", (fun ctx a -> `Title (title_of_xml a)));
    ("description", (fun ctx a -> `Description (description_of_xml a)));
    ("name", (fun ctx a -> `Name (name_of_xml a)));
    ("link", (fun ctx a -> `Link (link_of_xml a)));
  ] in
  let attr_producer = [
    ("about", (fun ctx a -> `About a))
  ] in
  XML.generate_catcher ~attr_producer ~data_producer make_textinput

let textinput_of_xml' =
  let data_producer = [
    ("title", (fun ctx a -> `Title (title_of_xml' a)));
    ("description", (fun ctx a -> `Description (description_of_xml' a)));
    ("name", (fun ctx a -> `Name (name_of_xml' a)));
    ("link", (fun ctx a -> `Link (link_of_xml' a)));
  ] in
  let attr_producer = [
    ("about", (fun ctx a -> `About a))
  ] in
  XML.generate_catcher ~attr_producer ~data_producer (fun x -> x)

type rdf =
  {
    channel: channel;
    image: image option;
    item: item list;
    textinput: textinput option;
  }

type rdf' = [
  | `Channel of channel
  | `Image of image
  | `Item of item
  | `TextInput of textinput
]

let make_rdf (l : [< rdf' ] list) =
  let channel = match find (function `Channel _ -> true | _ -> false) l with
    | Some (`Channel channel) -> channel
    | _ -> Error.raise_expectation (Error.Tag "channel") (Error.Tag "RDF")
  in let image = match find (function `Image _ -> true | _ -> false) l with
    | Some (`Image image) -> Some image
    | _ -> None
  in let textinput = match find (function `TextInput _ -> true | _ -> false) l with
    | Some (`TextInput textinput) -> Some textinput
    | _ -> None
  in let item = List.fold_left (fun acc -> function `Item x -> x :: acc | _ -> acc) [] l
  in ({ channel; image; item; textinput } : rdf)

let rdf_of_xml =
  let data_producer = [
    ("channel", (fun ctx a -> `Channel (channel_of_xml a)));
    ("image", (fun ctx a -> `Image (image_of_xml a)));
    ("item", (fun ctx a -> `Item (item_of_xml a)));
    ("textinput", (fun ctx a -> `TextInput (textinput_of_xml a)))
  ] in
  XML.generate_catcher ~data_producer make_rdf

let rdf_of_xml' =
  let data_producer = [
    ("channel", (fun ctx a -> `Channel (channel_of_xml' a)));
    ("image", (fun ctx a -> `Image (image_of_xml' a)));
    ("item", (fun ctx a -> `Item (item_of_xml' a)));
    ("textinput", (fun ctx a -> `TextInput (textinput_of_xml' a)))
  ] in
  XML.generate_catcher ~data_producer (fun x -> x)

let analyze input =
  match XML.tree input with
  | XML.Node (tag, datas) when tag_is tag "RDF" -> rdf_of_xml (tag, datas)
  | _ -> Error.raise_expectation (Error.Tag "RDF") Error.Root

let unsafe input =
  match XML.tree input with
  | XML.Node (tag, datas) when tag_is tag "RDF" ->
     `RDF(rdf_of_xml' (tag, datas))
  | _ -> `RDF []
