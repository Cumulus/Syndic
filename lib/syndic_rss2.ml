open Syndic_common.XML
open Syndic_common.Util
module XML = Syndic_xml
module Atom = Syndic_atom
module Date = Syndic_date
module Error = Syndic_error

type image =
  { url: Uri.t
  ; title: string
  ; link: Uri.t
  ; width: int
  ; (* default 88 *)
    height: int
  ; (* default 31 *)
    description: string option }

type image' =
  [ `URL of Uri.t
  | `Title of string
  | `Link of Uri.t
  | `Width of int
  | `Height of int
  | `Description of string ]

let make_image ~pos (l : [< image'] list) =
  let url =
    match find (function `URL _ -> true | _ -> false) l with
    | Some (`URL u) -> u
    | _ ->
        raise
          (Error.Error
             (pos, "<image> elements MUST contains exactly one <url> element"))
  in
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
    | Some (`Link l) -> l
    | _ ->
        raise
          (Error.Error
             (pos, "<image> elements MUST contains exactly one <link> element"))
  in
  let width =
    match find (function `Width _ -> true | _ -> false) l with
    | Some (`Width w) -> w
    | _ -> 88
    (* cf. RFC *)
  in
  let height =
    match find (function `Height _ -> true | _ -> false) l with
    | Some (`Height h) -> h
    | _ -> 31
    (* cf. RFC *)
  in
  let description =
    match find (function `Description _ -> true | _ -> false) l with
    | Some (`Description s) -> Some s
    | _ -> None
  in
  `Image ({url; title; link; width; height; description} : image)

let url_of_xml ~xmlbase a = `URL (XML.resolve ~xmlbase (Uri.of_string a))
let url_of_xml' ~xmlbase a = `URL (xmlbase, a)

let image_url_of_xml ~xmlbase (pos, _tag, datas) =
  try url_of_xml ~xmlbase (get_leaf datas) with Not_found ->
    raise
      (Error.Error (pos, "The content of <uri> MUST be a non-empty string"))

let image_title_of_xml ~xmlbase:_ (_pos, _tag, datas) =
  `Title (try get_leaf datas with Not_found -> "")

let image_link_of_xml ~xmlbase (pos, _tag, datas) =
  try `Link (XML.resolve ~xmlbase (Uri.of_string (get_leaf datas)))
  with Not_found ->
    raise
      (Error.Error (pos, "The content of <link> MUST be a non-empty string"))

let image_size_of_xml ~max ~xmlbase:_ (pos, tag, datas) =
  try
    let size = int_of_string (get_leaf datas) in
    if size > max then
      raise
        (Error.Error
           ( pos
           , "size of "
             ^ get_tag_name tag
             ^ " exceeded (max is "
             ^ string_of_int max
             ^ ")" ))
    else size
  with
  | Not_found ->
      raise
        (Error.Error
           ( pos
           , "The content of <"
             ^ get_tag_name tag
             ^ "> MUST be a non-empty string" ))
  | Failure _ ->
      raise
        (Error.Error
           (pos, "The content of <" ^ get_tag_name tag ^ "> MUST be an integer"))

let image_width_of_xml ~xmlbase a =
  `Width (image_size_of_xml ~max:144 ~xmlbase a)

let image_height_of_xml ~xmlbase a =
  `Height (image_size_of_xml ~max:400 ~xmlbase a)

let image_description_of_xml ~xmlbase:_ (pos, _tag, datas) =
  try `Description (get_leaf datas) with Not_found ->
    raise
      (Error.Error
         (pos, "The content of <description> MUST be a non-empty string"))

let image_of_xml =
  let data_producer =
    [ ("url", image_url_of_xml)
    ; ("title", image_title_of_xml)
    ; ("link", image_link_of_xml)
    ; ("width", image_width_of_xml)
    ; ("height", image_height_of_xml)
    ; ("description", image_description_of_xml) ]
  in
  generate_catcher ~data_producer make_image

let image_of_xml' =
  let data_producer =
    [ ("url", dummy_of_xml ~ctor:url_of_xml')
    ; ("title", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Title a))
    ; ("link", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Link (xmlbase, a)))
    ; ("width", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Width a))
    ; ("height", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Height a))
    ; ("description", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Description a))
    ]
  in
  generate_catcher ~data_producer (fun ~pos:_ x -> `Image x)

type cloud = {uri: Uri.t; registerProcedure: string; protocol: string}

type cloud' =
  [ `Domain of string
  | `Port of string
  | `Path of string
  | `RegisterProcedure of string
  | `Protocol of string ]

let make_cloud ~pos (l : [< cloud'] list) =
  let domain =
    match find (function `Domain _ -> true | _ -> false) l with
    | Some (`Domain u) -> u
    | _ ->
        raise
          (Error.Error (pos, "Cloud elements MUST have a 'domain' attribute"))
  in
  let port =
    match find (function `Port _ -> true | _ -> false) l with
    | Some (`Port p) -> int_of_string p
    | _ ->
        raise
          (Error.Error (pos, "Cloud elements MUST have a 'port' attribute"))
  in
  let path =
    match find (function `Path _ -> true | _ -> false) l with
    | Some (`Path p) -> p
    | _ ->
        raise
          (Error.Error (pos, "Cloud elements MUST have a 'path' attribute"))
  in
  let registerProcedure =
    match find (function `RegisterProcedure _ -> true | _ -> false) l with
    | Some (`RegisterProcedure r) -> r
    | _ ->
        raise
          (Error.Error
             (pos, "Cloud elements MUST have a 'registerProcedure' attribute"))
  in
  let protocol =
    match find (function `Protocol _ -> true | _ -> false) l with
    | Some (`Protocol p) -> p
    | _ ->
        raise
          (Error.Error (pos, "Cloud elements MUST have a 'protocol' attribute"))
  in
  let uri = Uri.make ~host:domain ~port ~path () in
  `Cloud ({uri; registerProcedure; protocol} : cloud)

let cloud_attr_producer =
  [ ("domain", fun ~xmlbase:_ a -> `Domain a)
  ; ("port", fun ~xmlbase:_ a -> `Port a)
  ; ("path", fun ~xmlbase:_ a -> `Path a)
  ; (* XXX: it's RFC compliant ? *)
    ("registerProcedure", fun ~xmlbase:_ a -> `RegisterProcedure a)
  ; ("protocol", fun ~xmlbase:_ a -> `Protocol a) ]

let cloud_of_xml =
  generate_catcher ~attr_producer:cloud_attr_producer make_cloud

let cloud_of_xml' =
  generate_catcher ~attr_producer:cloud_attr_producer (fun ~pos:_ x -> `Cloud x)

type textinput = {title: string; description: string; name: string; link: Uri.t}

type textinput' =
  [`Title of string | `Description of string | `Name of string | `Link of Uri.t]

let make_textinput ~pos (l : [< textinput'] list) =
  let title =
    match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title t) -> t
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
    | Some (`Name s) -> s
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
  `TextInput ({title; description; name; link} : textinput)

let textinput_title_of_xml ~xmlbase:_ (pos, _tag, datas) =
  try `Title (get_leaf datas) with Not_found ->
    raise
      (Error.Error (pos, "The content of <title> MUST be a non-empty string"))

let textinput_description_of_xml ~xmlbase:_ (pos, _tag, datas) =
  try `Description (get_leaf datas) with Not_found ->
    raise
      (Error.Error
         (pos, "The content of <description> MUST be a non-empty string"))

let textinput_name_of_xml ~xmlbase:_ (pos, _tag, datas) =
  try `Name (get_leaf datas) with Not_found ->
    raise
      (Error.Error (pos, "The content of <name> MUST be a non-empty string"))

let textinput_link_of_xml ~xmlbase (pos, _tag, datas) =
  try `Link (XML.resolve ~xmlbase (Uri.of_string (get_leaf datas)))
  with Not_found ->
    raise
      (Error.Error (pos, "The content of <link> MUST be a non-empty string"))

let textinput_of_xml =
  let data_producer =
    [ ("title", textinput_title_of_xml)
    ; ("description", textinput_description_of_xml)
    ; ("name", textinput_name_of_xml)
    ; ("link", textinput_link_of_xml) ]
  in
  generate_catcher ~data_producer make_textinput

let textinput_of_xml' =
  let data_producer =
    [ ("title", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Title a))
    ; ("description", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Description a))
    ; ("name", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Name a))
    ; ("link", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Link (xmlbase, a))) ]
  in
  generate_catcher ~data_producer (fun ~pos:_ x -> `TextInput x)

type category = {data: string; domain: Uri.t option}
type category' = [`Data of string | `Domain of Uri.t]

let make_category ~pos:_ (l : [< category'] list) =
  let data =
    match find (function `Data _ -> true | _ -> false) l with
    | Some (`Data s) -> s
    | _ -> ""
  in
  let domain =
    match find (function `Domain _ -> true | _ -> false) l with
    | Some (`Domain d) -> Some d
    | _ -> None
  in
  `Category ({data; domain} : category)

let category_of_xml =
  let attr_producer =
    [("domain", fun ~xmlbase:_ a -> `Domain (Uri.of_string a))]
  in
  let leaf_producer ~xmlbase:_ _pos data = `Data data in
  generate_catcher ~attr_producer ~leaf_producer make_category

let category_of_xml' =
  let attr_producer = [("domain", fun ~xmlbase:_ a -> `Domain a)] in
  let leaf_producer ~xmlbase:_ _pos data = `Data data in
  generate_catcher ~attr_producer ~leaf_producer (fun ~pos:_ x -> `Category x)

type enclosure = {url: Uri.t; length: int; mime: string}
type enclosure' = [`URL of Uri.t | `Length of string | `Mime of string]

let make_enclosure ~pos (l : [< enclosure'] list) =
  let url =
    match find (function `URL _ -> true | _ -> false) l with
    | Some (`URL u) -> u
    | _ ->
        raise
          (Error.Error (pos, "Enclosure elements MUST have a 'url' attribute"))
  in
  let length =
    match find (function `Length _ -> true | _ -> false) l with
    | Some (`Length l) -> int_of_string l
    | _ ->
        raise
          (Error.Error
             (pos, "Enclosure elements MUST have a 'length' attribute"))
  in
  let mime =
    match find (function `Mime _ -> true | _ -> false) l with
    | Some (`Mime m) -> m
    | _ ->
        raise
          (Error.Error (pos, "Enclosure elements MUST have a 'type' attribute"))
  in
  `Enclosure ({url; length; mime} : enclosure)

let enclosure_of_xml =
  let attr_producer =
    [ ("url", url_of_xml)
    ; ("length", fun ~xmlbase:_ a -> `Length a)
    ; ("type", fun ~xmlbase:_ a -> `Mime a) ]
  in
  generate_catcher ~attr_producer make_enclosure

let enclosure_of_xml' =
  let attr_producer =
    [ ("url", url_of_xml')
    ; ("length", fun ~xmlbase:_ a -> `Length a)
    ; ("type", fun ~xmlbase:_ a -> `Mime a) ]
  in
  generate_catcher ~attr_producer (fun ~pos:_ x -> `Enclosure x)

type guid = {data: Uri.t; (* must be uniq *) permalink: bool (* default true *)}
type guid' = [`Data of Uri.t option * string | `Permalink of string]

(* Some RSS2 server output <guid isPermaLink="false"></guid> ! *)
let make_guid ~pos:_ (l : [< guid'] list) =
  let permalink =
    match find (function `Permalink _ -> true | _ -> false) l with
    | Some (`Permalink b) -> bool_of_string b
    | _ -> true
    (* cf. RFC *)
  in
  match find (function `Data _ -> true | _ -> false) l with
  | Some (`Data (xmlbase, u)) ->
      if u = "" then `Guid None
      else
        (* When the GUID is declared as a permlink, resolve it using xml:base *)
        let data =
          if permalink then XML.resolve ~xmlbase (Uri.of_string u)
          else Uri.of_string u
        in
        `Guid (Some ({data; permalink} : guid))
  | _ -> `Guid None

let guid_of_xml, guid_of_xml' =
  let attr_producer = [("isPermaLink", fun ~xmlbase:_ a -> `Permalink a)] in
  let leaf_producer ~xmlbase _pos data = `Data (xmlbase, data) in
  ( generate_catcher ~attr_producer ~leaf_producer make_guid
  , generate_catcher ~attr_producer ~leaf_producer (fun ~pos:_ x -> `Guid x) )

type source = {data: string; url: Uri.t}
type source' = [`Data of string | `URL of Uri.t]

let make_source ~pos (l : [< source'] list) =
  let data =
    match find (function `Data _ -> true | _ -> false) l with
    | Some (`Data s) -> s
    | _ ->
        raise
          (Error.Error
             (pos, "The content of <source> MUST be a non-empty string"))
  in
  let url =
    match find (function `URL _ -> true | _ -> false) l with
    | Some (`URL u) -> u
    | _ ->
        raise
          (Error.Error (pos, "Source elements MUST have a 'url' attribute"))
  in
  `Source ({data; url} : source)

let source_of_xml =
  let attr_producer = [("url", url_of_xml)] in
  let leaf_producer ~xmlbase:_ _pos data = `Data data in
  generate_catcher ~attr_producer ~leaf_producer make_source

let source_of_xml' =
  let attr_producer = [("url", url_of_xml')] in
  let leaf_producer ~xmlbase:_ _pos data = `Data data in
  generate_catcher ~attr_producer ~leaf_producer (fun ~pos:_ x -> `Source x)

type story =
  | All of string * Uri.t option * string
  | Title of string
  | Description of Uri.t option * string

type item =
  { story: story
  ; content: Uri.t option * string
  ; link: Uri.t option
  ; author: string option
  ; (* e-mail *)
    categories: category list
  ; comments: Uri.t option
  ; enclosure: enclosure option
  ; guid: guid option
  ; pubDate: Date.t option
  ; (* date *)
    source: source option }

[@@@warning "-34"]

type item' =
  [ `Title of string
  | `Description of Uri.t option * string (* xmlbase, description *)
  | `Content of Uri.t option * string
  | `Link of Uri.t
  | `Author of string (* e-mail *)
  | `Category of category
  | `Comments of Uri.t
  | `Enclosure of enclosure
  | `Guid of guid
  | `PubDate of Date.t
  | `Source of source ]

let make_item ~pos (l : _ list) =
  let story =
    match
      ( find (function `Title _ -> true | _ -> false) l
      , find (function `Description _ -> true | _ -> false) l )
    with
    | Some (`Title t), Some (`Description (x, d)) -> All (t, x, d)
    | Some (`Title t), _ -> Title t
    | _, Some (`Description (x, d)) -> Description (x, d)
    | _, _ ->
        raise (Error.Error (pos, "Item expected <title> or <description> tag"))
  in
  let content =
    match find (function `Content _ -> true | _ -> false) l with
    | Some (`Content (x, c)) -> (x, c)
    | _ -> (None, "")
  in
  let link =
    match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link l) -> l
    | _ -> None
  in
  let author =
    match find (function `Author _ -> true | _ -> false) l with
    | Some (`Author a) -> Some a
    | _ -> None
  in
  let categories =
    let fn = fun acc -> function `Category x -> x :: acc | _ -> acc in
    List.fold_left fn [] l |> List.rev
  in
  let comments =
    match find (function `Comments _ -> true | _ -> false) l with
    | Some (`Comments c) -> Some c
    | _ -> None
  in
  let enclosure =
    match find (function `Enclosure _ -> true | _ -> false) l with
    | Some (`Enclosure e) -> Some e
    | _ -> None
  in
  let guid =
    match find (function `Guid _ -> true | _ -> false) l with
    | Some (`Guid g) -> g
    | _ -> None
  in
  let pubDate =
    match find (function `PubDate _ -> true | _ -> false) l with
    | Some (`PubDate p) -> Some p
    | _ -> None
  in
  let source =
    match find (function `Source _ -> true | _ -> false) l with
    | Some (`Source s) -> Some s
    | _ -> None
  in
  `Item
    ( { story
      ; content
      ; link
      ; author
      ; categories
      ; comments
      ; enclosure
      ; guid
      ; pubDate
      ; source }
      : item )

let item_title_of_xml ~xmlbase:_ (pos, _tag, datas) =
  try `Title (get_leaf datas) with Not_found ->
    raise
      (Error.Error (pos, "The content of <title> MUST be a non-empty string"))

let item_description_of_xml ~xmlbase (_pos, _tag, datas) =
  `Description (xmlbase, try get_leaf datas with Not_found -> "")

let item_content_of_xml ~xmlbase (_pos, _tag, datas) =
  `Content (xmlbase, try get_leaf datas with Not_found -> "")

let item_link_of_xml ~xmlbase (_pos, _tag, datas) =
  `Link
    ( try Some (XML.resolve ~xmlbase (Uri.of_string (get_leaf datas)))
      with Not_found -> None )

let item_author_of_xml ~xmlbase:_ (pos, _tag, datas) =
  try `Author (get_leaf datas) with Not_found ->
    raise
      (Error.Error (pos, "The content of <author> MUST be a non-empty string"))

let item_comments_of_xml ~xmlbase (pos, _tag, datas) =
  try `Comments (XML.resolve ~xmlbase (Uri.of_string (get_leaf datas)))
  with Not_found ->
    raise
      (Error.Error (pos, "The content of <comments> MUST be a non-empty string"))

let item_pubdate_of_xml ~xmlbase:_ (pos, _tag, datas) =
  try `PubDate (Date.of_rfc822 (get_leaf datas)) with Not_found ->
    raise
      (Error.Error (pos, "The content of <pubDate> MUST be a non-empty string"))

let item_namespaces = [""; "http://purl.org/rss/1.0/modules/content/"]

let item_of_xml =
  let data_producer =
    [ ("title", item_title_of_xml)
    ; ("description", item_description_of_xml)
    ; (* <content:encoded> where
         xmlns:content="http://purl.org/rss/1.0/modules/content/" *)
      ("encoded", item_content_of_xml)
    ; ("link", item_link_of_xml)
    ; ("author", item_author_of_xml)
    ; ("category", category_of_xml)
    ; ("comments", item_comments_of_xml)
    ; ("enclosure", enclosure_of_xml)
    ; ("guid", guid_of_xml)
    ; ("pubDate", item_pubdate_of_xml)
    ; ("source", source_of_xml) ]
  in
  generate_catcher ~data_producer make_item ~namespaces:item_namespaces

let item_of_xml' =
  let data_producer =
    [ ("title", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Title a))
    ; ("description", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Description a))
    ; ("encoded", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Content a))
    ; ("link", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Link (xmlbase, a)))
    ; ("author", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Author a))
    ; ("category", category_of_xml')
    ; ("comments", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Comments a))
    ; ("enclosure", enclosure_of_xml')
    ; ("guid", guid_of_xml')
    ; ("pubdate", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `PubDate a))
    ; ("source", source_of_xml') ]
  in
  generate_catcher ~data_producer
    (fun ~pos:_ x -> `Item x)
    ~namespaces:item_namespaces

type channel =
  { title: string
  ; link: Uri.t
  ; description: string
  ; language: string option
  ; copyright: string option
  ; managingEditor: string option
  ; webMaster: string option
  ; pubDate: Date.t option
  ; lastBuildDate: Date.t option
  ; category: string option
  ; generator: string option
  ; docs: Uri.t option
  ; cloud: cloud option
  ; ttl: int option
  ; image: image option
  ; rating: int option
  ; textInput: textinput option
  ; skipHours: int option
  ; skipDays: int option
  ; items: item list }

type channel' =
  [ `Title of string
  | `Link of Uri.t
  | `Description of string
  | `Language of string
  | `Copyright of string
  | `ManagingEditor of string
  | `WebMaster of string
  | `PubDate of Date.t
  | `LastBuildDate of Date.t
  | `Category of string
  | `Generator of string
  | `Docs of Uri.t
  | `Cloud of cloud
  | `TTL of int
  | `Image of image
  | `Rating of int
  | `TextInput of textinput
  | `SkipHours of int
  | `SkipDays of int
  | `Item of item ]

let make_channel ~pos (l : [< channel'] list) =
  let title =
    match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title t) -> t
    | _ ->
        raise
          (Error.Error
             ( pos
             , "<channel> elements MUST contains exactly one <title> element"
             ))
  in
  let link =
    match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link l) -> l
    | _ ->
        raise
          (Error.Error
             ( pos
             , "<channel> elements MUST contains exactly one <link> element" ))
  in
  let description =
    match find (function `Description _ -> true | _ -> false) l with
    | Some (`Description l) -> l
    | _ ->
        raise
          (Error.Error
             ( pos
             , "<channel> elements MUST contains exactly one <description> \
                element" ))
  in
  let language =
    match find (function `Language _ -> true | _ -> false) l with
    | Some (`Language a) -> Some a
    | _ -> None
  in
  let copyright =
    match find (function `Copyright _ -> true | _ -> false) l with
    | Some (`Copyright a) -> Some a
    | _ -> None
  in
  let managingEditor =
    match find (function `ManagingEditor _ -> true | _ -> false) l with
    | Some (`ManagingEditor a) -> Some a
    | _ -> None
  in
  let webMaster =
    match find (function `WebMaster _ -> true | _ -> false) l with
    | Some (`WebMaster a) -> Some a
    | _ -> None
  in
  let pubDate =
    match find (function `PubDate _ -> true | _ -> false) l with
    | Some (`PubDate a) -> Some a
    | _ -> None
  in
  let lastBuildDate =
    match find (function `LastBuildDate _ -> true | _ -> false) l with
    | Some (`LastBuildDate a) -> Some a
    | _ -> None
  in
  let category =
    match find (function `Category _ -> true | _ -> false) l with
    | Some (`Category a) -> Some a
    | _ -> None
  in
  let generator =
    match find (function `Generator _ -> true | _ -> false) l with
    | Some (`Generator a) -> Some a
    | _ -> None
  in
  let docs =
    match find (function `Docs _ -> true | _ -> false) l with
    | Some (`Docs a) -> Some a
    | _ -> None
  in
  let cloud =
    match find (function `Cloud _ -> true | _ -> false) l with
    | Some (`Cloud a) -> Some a
    | _ -> None
  in
  let ttl =
    match find (function `TTL _ -> true | _ -> false) l with
    | Some (`TTL a) -> Some a
    | _ -> None
  in
  let image =
    match find (function `Image _ -> true | _ -> false) l with
    | Some (`Image a) -> Some a
    | _ -> None
  in
  let rating =
    match find (function `Rating _ -> true | _ -> false) l with
    | Some (`Rating a) -> Some a
    | _ -> None
  in
  let textInput =
    match find (function `TextInput _ -> true | _ -> false) l with
    | Some (`TextInput a) -> Some a
    | _ -> None
  in
  let skipHours =
    match find (function `SkipHours _ -> true | _ -> false) l with
    | Some (`SkipHours a) -> Some a
    | _ -> None
  in
  let skipDays =
    match find (function `SkipDays _ -> true | _ -> false) l with
    | Some (`SkipDays a) -> Some a
    | _ -> None
  in
  let items =
    List.fold_left (fun acc -> function `Item x -> x :: acc | _ -> acc) [] l
  in
  ( { title
    ; link
    ; description
    ; language
    ; copyright
    ; managingEditor
    ; webMaster
    ; pubDate
    ; lastBuildDate
    ; category
    ; generator
    ; docs
    ; cloud
    ; ttl
    ; image
    ; rating
    ; textInput
    ; skipHours
    ; skipDays
    ; items }
    : channel )

let channel_title_of_xml ~xmlbase:_ (pos, _tag, datas) =
  try `Title (get_leaf datas) with Not_found ->
    raise
      (Error.Error (pos, "The content of <title> MUST be a non-empty string"))

let channel_description_of_xml ~xmlbase:_ (_pos, _tag, datas) =
  `Description (try get_leaf datas with Not_found -> "")

let channel_link_of_xml ~xmlbase (pos, _tag, datas) =
  try `Link (XML.resolve ~xmlbase (Uri.of_string (get_leaf datas)))
  with Not_found ->
    raise
      (Error.Error (pos, "The content of <link> MUST be a non-empty string"))

let channel_language_of_xml ~xmlbase:_ (pos, _tag, datas) =
  try `Language (get_leaf datas) with Not_found ->
    raise
      (Error.Error (pos, "The content of <language> MUST be a non-empty string"))

let channel_copyright_of_xml ~xmlbase:_ (_pos, _tag, datas) =
  try `Copyright (get_leaf datas) with Not_found -> `Copyright ""

(* XXX(dinosaure): aempty copyright is allowed. *)

let channel_managingeditor_of_xml ~xmlbase:_ (pos, _tag, datas) =
  try `ManagingEditor (get_leaf datas) with Not_found ->
    raise
      (Error.Error
         (pos, "The content of <managingEditor> MUST be a non-empty string"))

let channel_webmaster_of_xml ~xmlbase:_ (pos, _tag, datas) =
  try `WebMaster (get_leaf datas) with Not_found ->
    raise
      (Error.Error
         (pos, "The content of <webMaster> MUST be a non-empty string"))

let channel_pubdate_of_xml ~xmlbase:_ (pos, _tag, datas) =
  try `PubDate (Date.of_rfc822 (get_leaf datas)) with Not_found ->
    raise
      (Error.Error (pos, "The content of <pubDate> MUST be a non-empty string"))

let channel_lastbuilddate_of_xml ~xmlbase:_ (pos, _tag, datas) =
  try `LastBuildDate (Date.of_rfc822 (get_leaf datas)) with Not_found ->
    raise
      (Error.Error
         (pos, "The content of <lastBuildDate> MUST be a non-empty string"))

let channel_category_of_xml ~xmlbase:_ (pos, _tag, datas) =
  try `Category (get_leaf datas) with Not_found ->
    raise
      (Error.Error (pos, "The content of <category> MUST be a non-empty string"))

let channel_generator_of_xml ~xmlbase:_ (pos, _tag, datas) =
  try `Generator (get_leaf datas) with Not_found ->
    raise
      (Error.Error
         (pos, "The content of <generator> MUST be a non-empty string"))

let channel_docs_of_xml ~xmlbase (pos, _tag, datas) =
  try `Docs (XML.resolve ~xmlbase (Uri.of_string (get_leaf datas)))
  with Not_found ->
    raise
      (Error.Error (pos, "The content of <docs> MUST be a non-empty string"))

let channel_ttl_of_xml ~xmlbase:_ (pos, _tag, datas) =
  try `TTL (int_of_string (get_leaf datas)) with _ ->
    raise
      (Error.Error
         ( pos
         , "The content of <ttl> MUST be a non-empty string representing an \
            integer" ))

let channel_rating_of_xml ~xmlbase:_ (pos, _tag, datas) =
  try `Rating (int_of_string (get_leaf datas)) with _ ->
    raise
      (Error.Error
         ( pos
         , "The content of <rating> MUST be a non-empty string representing \
            an integer" ))

let channel_skipHours_of_xml ~xmlbase:_ (pos, _tag, datas) =
  try `SkipHours (int_of_string (get_leaf datas)) with _ ->
    raise
      (Error.Error
         ( pos
         , "The content of <skipHours> MUST be a non-empty string \
            representing an integer" ))

let channel_skipDays_of_xml ~xmlbase:_ (pos, _tag, datas) =
  try `SkipDays (int_of_string (get_leaf datas)) with _ ->
    raise
      (Error.Error
         ( pos
         , "The content of <skipDays> MUST be a non-empty string representing \
            an integer" ))

let channel_of_xml =
  let data_producer =
    [ ("title", channel_title_of_xml)
    ; ("link", channel_link_of_xml)
    ; ("description", channel_description_of_xml)
    ; ("Language", channel_language_of_xml)
    ; ("copyright", channel_copyright_of_xml)
    ; ("managingeditor", channel_managingeditor_of_xml)
    ; ("webmaster", channel_webmaster_of_xml)
    ; ("pubdate", channel_pubdate_of_xml)
    ; ("lastbuilddate", channel_lastbuilddate_of_xml)
    ; ("category", channel_category_of_xml)
    ; ("generator", channel_generator_of_xml)
    ; ("docs", channel_docs_of_xml)
    ; ("cloud", cloud_of_xml)
    ; ("ttl", channel_ttl_of_xml)
    ; ("image", image_of_xml)
    ; ("rating", channel_rating_of_xml)
    ; ("textinput", textinput_of_xml)
    ; ("skiphours", channel_skipHours_of_xml)
    ; ("skipdays", channel_skipDays_of_xml)
    ; ("item", item_of_xml) ]
  in
  generate_catcher ~data_producer make_channel

let channel_of_xml' =
  let data_producer =
    [ ("title", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Title a))
    ; ("link", dummy_of_xml ~ctor:(fun ~xmlbase a -> `Link (xmlbase, a)))
    ; ("description", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Description a))
    ; ("Language", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Language a))
    ; ("copyright", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Copyright a))
    ; ( "managingeditor"
      , dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `ManagingEditor a) )
    ; ("webmaster", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `WebMaster a))
    ; ("pubdate", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `PubDate a))
    ; ( "lastbuilddate"
      , dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `LastBuildDate a) )
    ; ("category", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Category a))
    ; ("generator", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Generator a))
    ; ("docs", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Docs a))
    ; ("cloud", cloud_of_xml')
    ; ("ttl", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `TTL a))
    ; ("image", image_of_xml')
    ; ("rating", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `Rating a))
    ; ("textinput", textinput_of_xml')
    ; ("skiphours", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `SkipHours a))
    ; ("skipdays", dummy_of_xml ~ctor:(fun ~xmlbase:_ a -> `SkipDays a))
    ; ("item", item_of_xml') ]
  in
  generate_catcher ~data_producer (fun ~pos:_ x -> x)

let find_channel l =
  find
    (function
      | XML.Node (_pos, tag, _data) -> tag_is tag "channel"
      | XML.Data _ -> false)
    l

let parse ?xmlbase input =
  match XML.of_xmlm input |> snd with
  | XML.Node (pos, tag, data) -> (
      if tag_is tag "channel" then channel_of_xml ~xmlbase (pos, tag, data)
      else
        match find_channel data with
        | Some (XML.Node (p, t, d)) -> channel_of_xml ~xmlbase (p, t, d)
        | Some (XML.Data _) | _ ->
            raise
              (Error.Error
                 ( (0, 0)
                 , "document MUST contains exactly one <channel> element" )) )
  | _ ->
      raise
        (Error.Error
           ((0, 0), "document MUST contains exactly one <channel> element"))

let read ?xmlbase fname =
  let fh = open_in fname in
  try
    let x = parse ?xmlbase (XML.input_of_channel fh) in
    close_in fh ; x
  with e -> close_in fh ; raise e

type uri = Uri.t option * string

let unsafe ?xmlbase input =
  match XML.of_xmlm input |> snd with
  | XML.Node (pos, tag, data) -> (
      if tag_is tag "channel" then
        `Channel (channel_of_xml' ~xmlbase (pos, tag, data))
      else
        match find_channel data with
        | Some (XML.Node (p, t, d)) ->
            `Channel (channel_of_xml' ~xmlbase (p, t, d))
        | Some (XML.Data _) | None -> `Channel [] )
  | _ -> `Channel []

(* Conversion to Atom *)

let map_option o f = match o with None -> None | Some v -> Some (f v)

(* Assume ASCII or a superset like UTF-8. *)
let valid_local_part =
  let is_valid c =
    let c = Char.unsafe_chr c in
    ('a' <= c && c <= 'z')
    || ('A' <= c && c <= 'Z')
    || ('0' <= c && c <= '9')
    || c = '.'
    (* shouldn't be the 1st char and not appear twice consecutively *)
    || c = '!'
    || c = '#'
    || c = '$'
    || c = '%'
    || c = '&'
    || c = '\''
    || c = '*'
    || c = '+'
    || c = '-'
    || c = '/'
    || c = '='
    || c = '?'
    || c = '^'
    || c = '_'
    || c = '`'
    || c = '{'
    || c = '|'
    || c = '}'
    || c = '~'
  in
  Array.init 256 is_valid

let is_valid_local_part c = valid_local_part.(Char.code c)

let valid_domain_part =
  let is_valid c =
    let c = Char.unsafe_chr c in
    ('a' <= c && c <= 'z')
    || ('A' <= c && c <= 'Z')
    || ('0' <= c && c <= '9')
    || c = '.'
    || c = '.'
  in
  Array.init 256 is_valid

let is_valid_domain_part c = valid_domain_part.(Char.code c)

(* Valid range [s.[i]], [i0 ≤ i < i1]. *)
let sub_no_braces s i0 i1 =
  let i0 = if s.[i0] = '(' then i0 + 1 else i0 in
  let i1 = if s.[i1 - 1] = ')' then i1 - 1 else i1 in
  String.sub s i0 (i1 - i0)

(* The item author sometimes contains the name and email under the form "name
   <email>" or "email (name)". Try to extract both compnents. *)
let extract_name_email a =
  try
    let i = String.index a '@' in
    (* or Not_found *)
    let len = String.length a in
    let i0 = ref (i - 1) in
    while !i0 >= 0 && is_valid_local_part a.[!i0] do
      decr i0
    done ;
    incr i0 ;
    (* !i0 >= 0 is the first char of the possible email. *)
    let i1 = ref (i + 1) in
    while !i1 < len && is_valid_domain_part a.[!i1] do
      incr i1
    done ;
    if !i0 < i && i + 1 < !i1 then (
      let email = String.sub a !i0 (!i1 - !i0) in
      if !i0 > 0 && a.[!i0 - 1] = '<' then decr i0 ;
      if !i1 < len && a.[!i1] = '>' then incr i1 ;
      while !i1 < len && a.[!i1] = ' ' do
        incr i1
      done ;
      (* skip spaces *)
      let name =
        if !i0 <= 0 then
          if !i1 >= len then email (* no name *) else sub_no_braces a !i1 len
        else
          (* !i0 > 0 *)
          let name0 = String.trim (String.sub a 0 !i0) in
          if !i1 >= len then name0 else name0 ^ String.sub a !i1 (len - !i1)
      in
      (name, Some email) )
    else (a, None)
  with Not_found -> (a, None)

let looks_like_a_link u =
  (Uri.scheme u = Some "http" || Uri.scheme u = Some "https")
  && match Uri.host u with None | Some "" -> false | Some _ -> true

let entry_of_item ch_link ch_updated (it : item) : Atom.entry =
  let author =
    match it.author with
    | Some a ->
        let name, email = extract_name_email a in
        {Atom.name; uri= None; email}
    | None ->
        (* If no author is specified for the item, there is little one can do
           just using the RSS2 feed. The user will have to set it using Atom
           convenience functions. *)
        {Atom.name= ""; uri= None; email= None}
  in
  let categories =
    let fn (c : category) = { Atom.term= c.data; scheme= map_option c.domain (fun d -> d); label= None } in
    List.map fn it.categories
  in
  let (title : Atom.title), content =
    match it.story with
    | All (t, xmlbase, d) ->
        let content =
          match it.content with
          | _, "" -> if d = "" then None else Some (Atom.Html (xmlbase, d))
          | x, c -> Some (Atom.Html (x, c))
        in
        (Atom.Text t, content)
    | Title t ->
        let content =
          match it.content with
          | _, "" -> None
          | x, c -> Some (Atom.Html (x, c))
        in
        (Atom.Text t, content)
    | Description (xmlbase, d) ->
        let content =
          match it.content with
          | _, "" -> if d = "" then None else Some (Atom.Html (xmlbase, d))
          | x, c -> Some (Atom.Html (x, c))
        in
        (Atom.Text "", content)
  in
  let id =
    match it.guid with
    | Some g ->
        if g.permalink || looks_like_a_link g.data then g.data
        else
          let d = Digest.to_hex (Digest.string (Uri.to_string g.data)) in
          Uri.with_fragment ch_link (Some d)
    | None ->
        (* The [it.link] may not be a permanent link and may also be used by
           other items. We use a digest to make it unique. *)
        let link = match it.link with Some l -> l | None -> ch_link in
        let s =
          match it.story with
          | All (t, _, d) -> t ^ d
          | Title t -> t
          | Description (_, d) -> d
        in
        let d = Digest.to_hex (Digest.string s) in
        Uri.with_fragment link (Some d)
  in
  let links =
    match (it.guid, it.link) with
    | Some g, _ when g.permalink -> [Atom.link g.data ~rel:Atom.Alternate]
    | _, Some l -> [Atom.link l ~rel:Atom.Alternate]
    | Some g, _ ->
        (* Sometimes the guid sets [l.permalink = false] but is nonetheless the
           only URI we have. *)
        if looks_like_a_link g.data then [Atom.link g.data ~rel:Atom.Alternate]
        else []
    | _, None -> []
  in
  let links =
    match it.comments with
    | Some l ->
        { Atom.href= l
        ; rel= Atom.Related
        ; type_media= None
        ; hreflang= None
        ; title= ""
        ; length= None }
        :: links
    | None -> links
  in
  let links =
    match it.enclosure with
    | Some e ->
        { Atom.href= e.url
        ; rel= Atom.Enclosure
        ; type_media= Some e.mime
        ; hreflang= None
        ; title= ""
        ; length= Some e.length }
        :: links
    | None -> links
  in
  let source =
    match it.source with
    | Some s ->
        Some
          { Atom.authors= [author]
          ; (* Best guess *)
            categories= []
          ; contributors= []
          ; generator= None
          ; icon= None
          ; id= ch_link
          ; (* declared as the ID of the whole channel *)
            links=
              [ { Atom.href= s.url
                ; rel= Atom.Related
                ; type_media= None
                ; hreflang= None
                ; title= ""
                ; length= None } ]
          ; logo= None
          ; rights= None
          ; subtitle= None
          ; title= Atom.Text s.data
          ; updated= None }
    | None -> None
  in
  { Atom.authors= (author, [])
  ; categories
  ; content
  ; contributors= []
  ; id
  ; links
  ; published= None
  ; rights= None
  ; source
  ; summary= None
  ; title
  ; updated= (match it.pubDate with Some d -> d | None -> ch_updated) }

let more_recent_of_item date (it : item) =
  match (date, it.pubDate) with
  | _, None -> date
  | None, Some _ -> it.pubDate
  | Some d, Some di -> if Date.compare d di >= 0 then date else it.pubDate

let max_date_opt d = function None -> d | Some d' -> Date.max d d'

let to_atom ?self (ch : channel) : Atom.feed =
  let contributors =
    match ch.webMaster with
    | Some p -> [{Atom.name= "Webmaster"; uri= None; email= Some p}]
    | None -> []
  in
  let contributors =
    match ch.managingEditor with
    | Some p ->
        {Atom.name= "Managing Editor"; uri= None; email= Some p}
        :: contributors
    | None -> contributors
  in
  let links =
    [ { Atom.href= ch.link
      ; rel= Atom.Related
      ; type_media= Some "text/html"
      ; hreflang= None
      ; title= ch.title
      ; length= None } ]
  in
  let links =
    match self with
    | Some self ->
        { Atom.href= self
        ; rel= Atom.Self
        ; type_media= Some "application/rss+xml"
        ; hreflang= None
        ; title= ch.title
        ; length= None }
        :: links
    | None -> links
  in
  let updated =
    match List.fold_left more_recent_of_item None ch.items with
    | None -> max_date_opt Date.epoch ch.lastBuildDate
    | Some d -> max_date_opt d ch.lastBuildDate
  in
  { Atom.authors= []
  ; categories=
      ( match ch.category with
      | None -> []
      | Some c -> [{Atom.term= c; scheme= None; label= None}] )
  ; contributors
  ; generator=
      map_option ch.generator (fun g ->
          {Atom.content= g; version= None; uri= None} )
  ; icon= None
  ; id= ch.link
  ; (* FIXME: Best we can do? *)
    links
  ; logo= map_option ch.image (fun i -> i.url)
  ; rights= map_option ch.copyright (fun c -> (Atom.Text c : Atom.rights))
  ; subtitle= None
  ; title= Atom.Text ch.title
  ; updated
  ; entries= List.map (entry_of_item ch.link updated) ch.items }
