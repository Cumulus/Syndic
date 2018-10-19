open Syndic_common.XML
open Syndic_common.Util
module XML = Syndic_xml
module Error = Syndic_error

let namespaces =
  ["http://purl.org/rss/1.0/"; "http://www.w3.org/1999/02/22-rdf-syntax-ns#"]

type title = string

let make_title ~pos (l : string list) =
  let title =
    match l with
    | d :: _ -> d
    | [] ->
        raise
          (Error.Error
             (pos, "The content of <title> MUST be a non-empty string"))
  in
  `Title title

let title_of_xml, title_of_xml' =
  let leaf_producer ~xmlbase:_ _pos data = data in
  ( generate_catcher ~namespaces ~leaf_producer make_title
  , generate_catcher ~namespaces ~leaf_producer (fun ~pos:_ x -> `Title x) )

type name = string

let make_name ~pos (l : string list) =
  let name =
    match l with
    | d :: _ -> d
    | [] ->
        raise
          (Error.Error (pos, "The content of <name> MUST be a non-empty string"))
  in
  `Name name

let name_of_xml, name_of_xml' =
  let leaf_producer ~xmlbase:_ _pos data = data in
  ( generate_catcher ~namespaces ~leaf_producer make_name
  , generate_catcher ~namespaces ~leaf_producer (fun ~pos:_ x -> `Name x) )

type description = string

let make_description ~pos (l : string list) =
  let description =
    match l with
    | s :: _ -> s
    | [] ->
        raise
          (Error.Error
             (pos, "The content of <description> MUST be a non-empty string"))
  in
  `Description description

let description_of_xml, description_of_xml' =
  let leaf_producer ~xmlbase:_ _pos data = data in
  ( generate_catcher ~namespaces ~leaf_producer make_description
  , generate_catcher ~namespaces ~leaf_producer (fun ~pos:_ x -> `Description x)
  )

type channel_image = Uri.t
type channel_image' = [`URI of Uri.t option * string]

let make_channel_image ~pos (l : [< channel_image'] list) =
  let image =
    match find (function `URI _ -> true) l with
    | Some (`URI (xmlbase, u)) -> XML.resolve ~xmlbase (Uri.of_string u)
    | _ ->
        raise
          (Error.Error
             (pos, "The content of <image> MUST be a non-empty string"))
  in
  `Image image

let channel_image_of_xml, channel_image_of_xml' =
  let attr_producer = [("resource", fun ~xmlbase a -> `URI (xmlbase, a))] in
  ( generate_catcher ~namespaces ~attr_producer make_channel_image
  , generate_catcher ~namespaces ~attr_producer (fun ~pos:_ x -> `Image x) )

type link = Uri.t
type link' = [`URI of Uri.t option * string]

let make_link ~pos (l : [< link'] list) =
  let link =
    match find (function `URI _ -> true) l with
    | Some (`URI (xmlbase, u)) -> XML.resolve ~xmlbase (Uri.of_string u)
    | _ ->
        raise
          (Error.Error (pos, "The content of <link> MUST be a non-empty string"))
  in
  `Link link

let link_of_xml, link_of_xml' =
  let leaf_producer ~xmlbase _pos data = `URI (xmlbase, data) in
  ( generate_catcher ~namespaces ~leaf_producer make_link
  , generate_catcher ~namespaces ~leaf_producer (fun ~pos:_ x -> `Link x) )

type url = Uri.t
type url' = [`URI of Uri.t option * string]

let make_url ~pos (l : [< url'] list) =
  let url =
    match find (function `URI _ -> true) l with
    | Some (`URI (xmlbase, u)) -> XML.resolve ~xmlbase (Uri.of_string u)
    | _ ->
        raise
          (Error.Error (pos, "The content of <url> MUST be a non-empty string"))
  in
  `URL url

let url_of_xml, url_of_xml' =
  let leaf_producer ~xmlbase _pos data = `URI (xmlbase, data) in
  ( generate_catcher ~namespaces ~leaf_producer make_url
  , generate_catcher ~namespaces ~leaf_producer (fun ~pos:_ x -> `URL x) )

type li = Uri.t
type li' = [`URI of Uri.t option * string]

let make_li ~pos (l : [< li'] list) =
  let url =
    match find (function `URI _ -> true) l with
    | Some (`URI (xmlbase, u)) -> XML.resolve ~xmlbase (Uri.of_string u)
    | _ ->
        raise
          (Error.Error (pos, "Li elements MUST have a 'resource' attribute"))
  in
  `Li url

let li_of_xml, li_of_xml' =
  let attr_producer = [("resource", fun ~xmlbase a -> `URI (xmlbase, a))] in
  ( generate_catcher ~namespaces ~attr_producer make_li
  , generate_catcher ~namespaces ~attr_producer (fun ~pos:_ x -> `Li x) )

type seq = li list
type seq' = [`Li of li]

let make_seq ~pos:_ (l : [< seq'] list) =
  let li = List.map (function `Li u -> u) l in
  `Seq li

let seq_of_xml =
  let data_producer = [("li", li_of_xml)] in
  generate_catcher ~namespaces ~data_producer make_seq

let seq_of_xml' =
  let data_producer = [("li", li_of_xml')] in
  generate_catcher ~namespaces ~data_producer (fun ~pos:_ x -> `Seq x)

type items = seq
type items' = [`Seq of seq]

let make_items ~pos (l : [< items'] list) =
  let li =
    match find (function `Seq _ -> true) l with
    | Some (`Seq l) -> l
    | _ ->
        raise
          (Error.Error
             ( pos
             , "<items> elements MUST contains exactly one <rdf:Seq> element"
             ))
  in
  `Items li

let items_of_xml =
  let data_producer = [("Seq", seq_of_xml)] in
  generate_catcher ~namespaces ~data_producer make_items

let items_of_xml' =
  let data_producer = [("Seq", seq_of_xml')] in
  generate_catcher ~namespaces ~data_producer (fun ~pos:_ x -> `Items x)

type channel_textinput = Uri.t
type channel_textinput' = [`URI of Uri.t option * string]

let make_textinput ~pos (l : [< channel_textinput'] list) =
  let url =
    match find (function `URI _ -> true) l with
    | Some (`URI (xmlbase, u)) -> XML.resolve ~xmlbase (Uri.of_string u)
    | _ ->
        raise
          (Error.Error
             (pos, "Textinput elements MUST have a 'resource' attribute"))
  in
  `TextInput url

let channel_textinput_of_xml, channel_textinput_of_xml' =
  let attr_producer = [("resource", fun ~xmlbase a -> `URI (xmlbase, a))] in
  ( generate_catcher ~namespaces ~attr_producer make_textinput
  , generate_catcher ~namespaces ~attr_producer (fun ~pos:_ x -> `TextInput x)
  )

type channel =
  { about: Uri.t
  ; (* must be uniq *)
    title: title
  ; link: link
  ; description: description
  ; image: channel_image option
  ; items: items
  ; textinput: channel_textinput option }

type channel' =
  [ `Title of title
  | `Link of link
  | `Description of description
  | `Image of channel_image
  | `Items of items
  | `TextInput of channel_textinput
  | `About of Uri.t ]

let make_channel ~pos (l : [< channel'] list) =
  let about =
    match find (function `About _ -> true | _ -> false) l with
    | Some (`About u) -> u
    | _ ->
        raise
          (Error.Error (pos, "Channel elements MUST have a 'about' attribute"))
  in
  let title =
    match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title s) -> s
    | _ ->
        raise
          (Error.Error
             ( pos
             , "<channel> elements MUST contains exactly one <title> element"
             ))
  in
  let link =
    match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link u) -> u
    | _ ->
        raise
          (Error.Error
             ( pos
             , "<channel> elements MUST contains exactly one <link> element" ))
  in
  let description =
    match find (function `Description _ -> true | _ -> false) l with
    | Some (`Description s) -> s
    | _ ->
        raise
          (Error.Error
             ( pos
             , "<channel> elements MUST contains exactly one <description> \
                element" ))
  in
  let image =
    match find (function `Image _ -> true | _ -> false) l with
    | Some (`Image i) -> Some i
    | _ -> None
  in
  let items =
    match find (function `Items _ -> true | _ -> false) l with
    | Some (`Items l) -> l
    | _ ->
        raise
          (Error.Error
             ( pos
             , "<channel> elements MUST contains exactly one <items> element"
             ))
  in
  let textinput =
    match find (function `TextInput _ -> true | _ -> false) l with
    | Some (`TextInput u) -> Some u
    | _ -> None
  in
  `Channel
    ({about; title; link; description; image; items; textinput} : channel)

let about_of_xml ~xmlbase a = `About (XML.resolve ~xmlbase (Uri.of_string a))
let about_of_xml' ~xmlbase a = `About (xmlbase, a)

let channel_of_xml =
  let data_producer =
    [ ("title", title_of_xml); ("link", link_of_xml)
    ; ("description", description_of_xml)
    ; ("image", channel_image_of_xml)
    ; ("items", items_of_xml)
    ; ("textinput", channel_textinput_of_xml) ]
  in
  let attr_producer = [("about", about_of_xml)] in
  generate_catcher ~namespaces ~attr_producer ~data_producer make_channel

let channel_of_xml' =
  let data_producer =
    [ ("title", title_of_xml'); ("link", link_of_xml')
    ; ("description", description_of_xml')
    ; ("image", channel_image_of_xml')
    ; ("items", items_of_xml')
    ; ("textinput", channel_textinput_of_xml') ]
  in
  let attr_producer = [("about", about_of_xml')] in
  generate_catcher ~namespaces ~attr_producer ~data_producer (fun ~pos:_ x ->
      `Channel x )

type image = {about: Uri.t; title: title; url: url; link: link}
type image' = [`Title of title | `Link of link | `URL of url | `About of Uri.t]

let make_image ~pos (l : [< image'] list) =
  let title =
    match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title t) -> t
    | _ ->
        raise
          (Error.Error
             (pos, "<image> elements MUST contains exactly one <title> element"))
  in
  let link =
    match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link u) -> u
    | _ ->
        raise
          (Error.Error
             (pos, "<image> elements MUST contains exactly one <link> element"))
  in
  let url =
    match find (function `URL _ -> true | _ -> false) l with
    | Some (`URL u) -> u
    | _ ->
        raise
          (Error.Error
             (pos, "<image> elements MUST contains exactly one <url> element"))
  in
  let about =
    match find (function `About _ -> true | _ -> false) l with
    | Some (`About a) -> a
    | _ ->
        raise
          (Error.Error (pos, "Image elements MUST have a 'about' attribute"))
  in
  `Image ({about; title; url; link} : image)

let image_of_xml =
  let data_producer =
    [("title", title_of_xml); ("link", link_of_xml); ("url", url_of_xml)]
  in
  let attr_producer = [("about", about_of_xml)] in
  generate_catcher ~namespaces ~attr_producer ~data_producer make_image

let image_of_xml' =
  let data_producer =
    [("title", title_of_xml'); ("link", link_of_xml'); ("url", url_of_xml')]
  in
  let attr_producer = [("about", about_of_xml')] in
  generate_catcher ~namespaces ~attr_producer ~data_producer (fun ~pos:_ x ->
      `Image x )

type item =
  {about: Uri.t; title: title; link: link; description: description option}

type item' =
  [ `Title of title
  | `Link of link
  | `Description of description
  | `About of Uri.t ]

let make_item ~pos (l : [< item'] list) =
  let title =
    match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title t) -> t
    | _ ->
        raise
          (Error.Error
             (pos, "<item> elements MUST contains exactly one <title> element"))
  in
  let link =
    match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link u) -> u
    | _ ->
        raise
          (Error.Error
             (pos, "<item> elements MUST contains exactly one <link> element"))
  in
  let description =
    match find (function `Description _ -> true | _ -> false) l with
    | Some (`Description d) -> Some d
    | _ -> None
  in
  let about =
    match find (function `About _ -> true | _ -> false) l with
    | Some (`About u) -> u
    | _ ->
        raise
          (Error.Error (pos, "Item elements MUST have a 'about' attribute"))
  in
  `Item ({about; title; link; description} : item)

let item_of_xml =
  let data_producer =
    [ ("title", title_of_xml); ("link", link_of_xml)
    ; ("description", description_of_xml) ]
  in
  let attr_producer = [("about", about_of_xml)] in
  generate_catcher ~namespaces ~attr_producer ~data_producer make_item

let item_of_xml' =
  let data_producer =
    [ ("title", title_of_xml'); ("link", link_of_xml')
    ; ("description", description_of_xml') ]
  in
  let attr_producer = [("about", about_of_xml')] in
  generate_catcher ~namespaces ~attr_producer ~data_producer (fun ~pos:_ x ->
      `Item x )

type textinput =
  {about: Uri.t; title: title; description: description; name: name; link: link}

type textinput' =
  [ `About of Uri.t
  | `Title of title
  | `Description of description
  | `Name of name
  | `Link of link ]

let make_textinput ~pos (l : [< textinput'] list) =
  let title =
    match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title s) -> s
    | _ ->
        raise
          (Error.Error
             ( pos
             , "<textinput> elements MUST contains exactly one <title> element"
             ))
  in
  let description =
    match find (function `Description _ -> true | _ -> false) l with
    | Some (`Description s) -> s
    | _ ->
        raise
          (Error.Error
             ( pos
             , "<textinput> elements MUST contains exactly one <description> \
                element" ))
  in
  let name =
    match find (function `Name _ -> true | _ -> false) l with
    | Some (`Name n) -> n
    | _ ->
        raise
          (Error.Error
             ( pos
             , "<textinput> elements MUST contains exactly one <name> element"
             ))
  in
  let link =
    match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link u) -> u
    | _ ->
        raise
          (Error.Error
             ( pos
             , "<textinput> elements MUST contains exactly one <link> element"
             ))
  in
  let about =
    match find (function `About _ -> true | _ -> false) l with
    | Some (`About u) -> u
    | _ ->
        raise
          (Error.Error (pos, "Textinput elements MUST have a 'about' attribute"))
  in
  `TextInput ({about; title; description; name; link} : textinput)

let textinput_of_xml =
  let data_producer =
    [ ("title", title_of_xml)
    ; ("description", description_of_xml)
    ; ("name", name_of_xml); ("link", link_of_xml) ]
  in
  let attr_producer = [("about", about_of_xml)] in
  generate_catcher ~namespaces ~attr_producer ~data_producer make_textinput

let textinput_of_xml' =
  let data_producer =
    [ ("title", title_of_xml')
    ; ("description", description_of_xml')
    ; ("name", name_of_xml'); ("link", link_of_xml') ]
  in
  let attr_producer = [("about", about_of_xml')] in
  generate_catcher ~namespaces ~attr_producer ~data_producer (fun ~pos:_ x ->
      `TextInput x )

type rdf =
  { channel: channel
  ; image: image option
  ; item: item list
  ; textinput: textinput option }

type rdf' =
  [ `Channel of channel
  | `Image of image
  | `Item of item
  | `TextInput of textinput ]

let make_rdf ~pos (l : [< rdf'] list) =
  let channel =
    match find (function `Channel _ -> true | _ -> false) l with
    | Some (`Channel channel) -> channel
    | _ ->
        raise
          (Error.Error
             (pos, "<rdf> elements MUST contains exactly one <channel> element"))
  in
  let image =
    match find (function `Image _ -> true | _ -> false) l with
    | Some (`Image image) -> Some image
    | _ -> None
  in
  let textinput =
    match find (function `TextInput _ -> true | _ -> false) l with
    | Some (`TextInput textinput) -> Some textinput
    | _ -> None
  in
  let item =
    List.fold_left (fun acc -> function `Item x -> x :: acc | _ -> acc) [] l
  in
  ({channel; image; item; textinput} : rdf)

let rdf_of_xml =
  let data_producer =
    [ ("channel", channel_of_xml)
    ; ("image", image_of_xml); ("item", item_of_xml)
    ; ("textinput", textinput_of_xml) ]
  in
  generate_catcher ~namespaces ~data_producer make_rdf

let rdf_of_xml' =
  let data_producer =
    [ ("channel", channel_of_xml')
    ; ("image", image_of_xml'); ("item", item_of_xml')
    ; ("textinput", textinput_of_xml') ]
  in
  generate_catcher ~namespaces ~data_producer (fun ~pos:_ x -> x)

let parse ?xmlbase input =
  match XML.of_xmlm input |> snd with
  | XML.Node (pos, tag, datas) when tag_is tag "RDF" ->
      rdf_of_xml ~xmlbase (pos, tag, datas)
  | _ ->
      raise
        (Error.Error
           ((0, 0), "document MUST contains exactly one <rdf> element"))

let read ?xmlbase fname =
  let fh = open_in fname in
  try
    let x = parse ?xmlbase (XML.input_of_channel fh) in
    close_in fh ; x
  with e -> close_in fh ; raise e

type uri = Uri.t option * string

let unsafe ?xmlbase input =
  match XML.of_xmlm input |> snd with
  | XML.Node (pos, tag, datas) when tag_is tag "RDF" ->
      `RDF (rdf_of_xml' ~xmlbase (pos, tag, datas))
  | _ -> `RDF []
