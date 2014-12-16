open Syndic_common.XML
open Syndic_common.Util
module XML = Syndic_xml
module Atom = Syndic_atom

module Date = Syndic_date
module Error = Syndic_error

type image =
  {
    url: Uri.t;
    title: string;
    link: Uri.t;
    width: int; (* default 88 *)
    height: int; (* default 31 *)
    description: string option;
  }

type image' = [
  | `URL of Uri.t
  | `Title of string
  | `Link of Uri.t
  | `Width of int
  | `Height of int
  | `Description of string
]

let make_image ~pos (l : [< image' ] list) =
  let url = match find (function `URL _ -> true | _ -> false) l with
    | Some (`URL u) -> u
    | _ ->
      raise (Error.Error (pos,
                            "<image> elements MUST contains exactly one \
                             <url> element"))
  in
  let title = match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title t) -> t
    | _ ->
      raise (Error.Error (pos,
                            "<image> elements MUST contains exactly one \
                             <title> element"))
  in
  let link = match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link l) -> l
    | _ ->
      raise (Error.Error (pos,
                            "<image> elements MUST contains exactly one \
                             <link> element"))
  in
  let width = match find (function `Width _ -> true | _ -> false) l with
    | Some (`Width w) -> w
    | _ -> 88 (* cf. RFC *)
  in
  let height = match find (function `Height _ -> true | _ -> false) l with
    | Some (`Height h) -> h
    | _ -> 31 (* cf. RFC *)
  in
  let description =
    match find (function `Description _ -> true | _ -> false) l with
    | Some (`Description s) -> Some s
    | _ -> None
  in
  `Image ({ url; title; link; width; height; description } : image)

let image_url_of_xml (pos, tag, datas) =
  try `URL(Uri.of_string (get_leaf datas))
  with Not_found -> raise (Error.Error (pos,
                            "The content of <uri> MUST be \
                             a non-empty string"))

let image_title_of_xml (pos, tag, datas) =
  `Title(try get_leaf datas
         with Not_found -> "")

let image_link_of_xml (pos, tag, datas) =
  try `Link(Uri.of_string (get_leaf datas))
  with Not_found -> raise (Error.Error (pos,
                            "The content of <link> MUST be \
                             a non-empty string"))

let image_size_of_xml ~max (pos, tag, datas) =
  try let size = int_of_string (get_leaf datas) in
    if size > max
    then raise (Error.Error
                  (pos, ("size of "  ^ (get_tag_name tag)
                         ^ " exceeded (max is " ^ (string_of_int max) ^ ")")))
    else size
  with Not_found -> raise (Error.Error (pos,
                            ("The content of <"^(get_tag_name tag)^"> MUST be \
                              a non-empty string")))
     | Failure "int_of_string" -> raise (Error.Error (pos,
                            ("The content of <"^(get_tag_name tag)^"> MUST be \
                              an integer")))

let image_width_of_xml a = `Width(image_size_of_xml ~max:144 a)
let image_height_of_xml a = `Height(image_size_of_xml ~max:400 a)

let image_description_of_xml (pos, tag, datas) =
  try `Description(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <description> MUST be \
                             a non-empty string"))

let image_of_xml =
  let data_producer = [
    ("url", image_url_of_xml);
    ("title", image_title_of_xml);
    ("link", image_link_of_xml);
    ("width", image_width_of_xml);
    ("height", image_height_of_xml);
    ("description", image_description_of_xml);
  ] in
  generate_catcher ~data_producer make_image

let image_of_xml' =
  let data_producer = [
    ("url", dummy_of_xml ~ctor:(fun a -> `URL a));
    ("title", dummy_of_xml ~ctor:(fun a -> `Title a));
    ("link", dummy_of_xml ~ctor:(fun a -> `Link a));
    ("width", dummy_of_xml ~ctor:(fun a -> `Width a));
    ("height", dummy_of_xml ~ctor:(fun a -> `Height a));
    ("description", dummy_of_xml ~ctor:(fun a -> `Description a));
  ] in
  generate_catcher ~data_producer (fun ~pos x -> `Image x)

type cloud = {
  domain: Uri.t;
  port: int;
  path: string;
  registerProcedure: string;
  protocol: string;
}

type cloud' = [
  | `Domain of string
  | `Port of string
  | `Path of string
  | `RegisterProcedure of string
  | `Protocol of string
]

let make_cloud ~pos (l : [< cloud' ] list) =
  let domain = match find (function `Domain _ -> true | _ -> false) l with
    | Some (`Domain u) -> (Uri.of_string u)
    | _ ->
      raise (Error.Error (pos,
                            "Cloud elements MUST have a 'domain' \
                             attribute"))
  in
  let port = match find (function `Port _ -> true | _ -> false) l with
    | Some (`Port p) -> (int_of_string p)
    | _ ->
      raise (Error.Error (pos,
                            "Cloud elements MUST have a 'port' \
                             attribute"))
  in
  let path = match find (function `Path _ -> true | _ -> false) l with
    | Some (`Path p) -> p
    | _ ->
      raise (Error.Error (pos,
                            "Cloud elements MUST have a 'path' \
                             attribute"))
  in
  let registerProcedure =
    match find (function `RegisterProcedure _ -> true | _ -> false) l with
    | Some (`RegisterProcedure r) -> r
    | _ ->
      raise (Error.Error (pos,
                            "Cloud elements MUST have a 'registerProcedure' \
                             attribute"))
  in
  let protocol = match find (function `Protocol _ -> true | _ -> false) l with
    | Some (`Protocol p) -> p
    | _ ->
      raise (Error.Error (pos,
                            "Cloud elements MUST have a 'protocol' \
                             attribute"))
  in
  `Cloud ({ domain; port; path; registerProcedure; protocol; } : cloud)

let cloud_of_xml, cloud_of_xml' =
  let attr_producer = [
    ("domain", (fun a -> `Domain a));
    ("port", (fun a -> `Port a));
    ("path", (fun a -> `Path a)); (* XXX: it's RFC compliant ? *)
    ("registerProcedure", (fun a -> `RegisterProcedure a));
    ("protocol", (fun a -> `Protocol a));
  ] in
  generate_catcher ~attr_producer make_cloud,
  generate_catcher ~attr_producer (fun ~pos x -> `Cloud x)

type textinput =
  {
    title: string;
    description: string;
    name: string;
    link: Uri.t;
  }

type textinput' = [
  | `Title of string
  | `Description of string
  | `Name of string
  | `Link of Uri.t
]

let make_textinput ~pos (l : [< textinput'] list) =
  let title = match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title t) -> t
    | _ ->
      raise (Error.Error (pos,
                            "<textinput> elements MUST contains exactly one \
                             <title> element"))
  in
  let description =
    match find (function `Description _ -> true | _ -> false) l with
    | Some (`Description s) -> s
    | _ ->
      raise (Error.Error (pos,
                            "<textinput> elements MUST contains exactly one \
                             <description> element"))
  in
  let name = match find (function `Name _ -> true | _ -> false) l with
    | Some (`Name s) -> s
    | _ ->
      raise (Error.Error (pos,
                            "<textinput> elements MUST contains exactly one \
                             <name> element"))
  in
  let link = match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link u) -> u
    | _ ->
      raise (Error.Error (pos,
                            "<textinput> elements MUST contains exactly one \
                             <link> element"))
  in
  `TextInput ({ title; description; name; link; } : textinput)

let textinput_title_of_xml (pos, tag, datas) =
  try `Title(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <title> MUST be \
                             a non-empty string"))

let textinput_description_of_xml (pos, tag, datas) =
  try `Description(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <description> MUST be \
                             a non-empty string"))

let textinput_name_of_xml (pos, tag, datas) =
  try `Name(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <name> MUST be \
                             a non-empty string"))

let textinput_link_of_xml (pos, tag, datas) =
  try `Link(Uri.of_string (get_leaf datas))
  with Not_found -> raise (Error.Error (pos,
                            "The content of <link> MUST be \
                             a non-empty string"))

let textinput_of_xml =
  let data_producer = [
    ("title", textinput_title_of_xml);
    ("description", textinput_description_of_xml);
    ("name", textinput_name_of_xml);
    ("link", textinput_link_of_xml);
  ] in
  generate_catcher ~data_producer make_textinput

let textinput_of_xml' =
  let data_producer = [
    ("title", dummy_of_xml ~ctor:(fun a -> `Title a));
    ("description", dummy_of_xml ~ctor:(fun a -> `Description a));
    ("name", dummy_of_xml ~ctor:(fun a -> `Name a));
    ("link", dummy_of_xml ~ctor:(fun a -> `Link a));
  ] in
  generate_catcher ~data_producer (fun ~pos x -> `TextInput x)

type category =
  {
    data: string;
    domain: Uri.t option;
  }

type category' = [
  | `Data of string
  | `Domain of string
]

let make_category ~pos (l : [< category' ] list) =
  let data = match find (function `Data _ -> true | _ -> false) l with
    | Some (`Data s)-> s
    | _ -> ""
  in let domain = match find (function `Domain _ -> true | _ -> false) l with
    | Some (`Domain d) -> Some (Uri.of_string d)
    | _ -> None
  in
  `Category({ data; domain; } : category )

let category_of_xml, category_of_xml' =
  let attr_producer = [ ("domain", (fun a -> `Domain a)); ] in
  let leaf_producer pos data = `Data data in
  generate_catcher ~attr_producer ~leaf_producer make_category,
  generate_catcher ~attr_producer ~leaf_producer (fun ~pos x -> `Category x)

type enclosure =
  {
    url: Uri.t;
    length: int;
    mime: string;
  }

type enclosure' = [
  | `URL of string
  | `Length of string
  | `Mime of string
]

let make_enclosure ~pos (l : [< enclosure' ] list) =
  let url = match find (function `URL _ -> true | _ -> false) l with
    | Some (`URL u) -> Uri.of_string u
    | _ ->
      raise (Error.Error (pos,
                            "Enclosure elements MUST have a 'url' \
                             attribute"))
  in
  let length = match find (function `Length _ -> true | _ -> false) l with
    | Some (`Length l) -> int_of_string l
    | _ ->
      raise (Error.Error (pos,
                            "Enclosure elements MUST have a 'length' \
                             attribute"))
  in
  let mime = match find (function `Mime _ -> true | _ -> false) l with
    | Some (`Mime m) -> m
    | _ ->
      raise (Error.Error (pos,
                            "Enclosure elements MUST have a 'type' \
                             attribute"))
  in
  `Enclosure ({ url; length; mime; } : enclosure)

let enclosure_of_xml, enclosure_of_xml' =
  let attr_producer = [
    ("url", (fun a -> `URL a));
    ("length", (fun a -> `Length a));
    ("type", (fun a -> `Mime a));
  ] in
  generate_catcher ~attr_producer make_enclosure,
  generate_catcher ~attr_producer (fun ~pos x -> `Enclosure x)

type guid =
  {
    data: Uri.t; (* must be uniq *)
    permalink: bool; (* default true *)
  }

type guid' = [
  | `Data of string
  | `Permalink of string
]

(* Some RSS2 server output <guid isPermaLink="false"></guid> ! *)
let make_guid ~pos (l : [< guid' ] list) =
  let data = match find (function `Data _ -> true | _ -> false) l with
    | Some (`Data u) -> u
    | _ -> ""
  in
  let permalink = match find (function `Permalink _ -> true | _ -> false) l with
    | Some (`Permalink b) -> bool_of_string b
    | _ -> true (* cf. RFC *)
  in
  `Guid(if data = "" then None
        else Some({ data = Uri.of_string data;  permalink } : guid))

let guid_of_xml, guid_of_xml' =
  let attr_producer = [ ("isPermalink", (fun a -> `Permalink a)); ] in
  let leaf_producer pos data = `Data data in
  generate_catcher ~attr_producer ~leaf_producer make_guid,
  generate_catcher ~attr_producer ~leaf_producer (fun ~pos x -> `Guid x)

type source =
  {
    data: string;
    url: Uri.t;
  }

type source' = [
  | `Data of string
  | `URL of string
]

let make_source ~pos (l : [< source' ] list) =
  let data = match find (function `Data _ -> true | _ -> false) l with
    | Some (`Data s) -> s
    | _  -> raise (Error.Error (pos,
                            "The content of <source> MUST be \
                             a non-empty string"))
  in
  let url = match find (function `URL _ -> true | _ -> false) l with
    | Some (`URL u) -> Uri.of_string u
    | _ ->
      raise (Error.Error (pos,
                            "Source elements MUST have a 'url' \
                             attribute"))
  in
  `Source ({ data; url; } : source)

let source_of_xml, source_of_xml' =
  let attr_producer = [ ("url", (fun a -> `URL a)); ] in
  let leaf_producer pos data = `Data data in
  generate_catcher ~attr_producer ~leaf_producer make_source,
  generate_catcher ~attr_producer ~leaf_producer (fun ~pos x -> `Source x)

type story =
  | All of string * string
  | Title of string
  | Description of string

type item =
  {
    story: story;
    link: Uri.t option;
    author:  string option; (* e-mail *)
    category: category option;
    comments: Uri.t option;
    enclosure: enclosure option;
    guid: guid option;
    pubDate: Date.t option; (* date *)
    source: source option;
  }

type item' = [
  | `Title of string
  | `Description of string
  | `Link of Uri.t
  | `Author of string (* e-mail *)
  | `Category of category
  | `Comments of Uri.t
  | `Enclosure of enclosure
  | `Guid of guid
  | `PubDate of Date.t
  | `Source of source
]

let make_item ~pos (l : _ list) =
  let story = match
      find (function `Title _ -> true | _ -> false) l,
      find (function `Description _ -> true | _ -> false) l
    with
    | Some (`Title t), Some (`Description d) -> All (t, d)
    | Some (`Title t), _ -> Title t
    | _, Some (`Description d) -> Description d
    | _, _ -> raise (Error.Error (pos,
                                  "Item expected <title> or <description> tag"))
  in
  let link = match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link l) -> l
    | _ -> None
  in
  let author = match find (function `Author _ -> true | _ -> false) l with
    | Some (`Author a) -> Some a
    | _ -> None
  in
  let category = match find (function `Category _ -> true | _ -> false) l with
    | Some (`Category c) -> Some c
    | _ -> None
  in
  let comments = match find (function `Comments _ -> true | _ -> false) l with
    | Some (`Comments c) -> Some c
    | _ -> None
  in
  let enclosure = match find (function `Enclosure _ -> true | _ -> false) l with
    | Some (`Enclosure e) -> Some e
    | _ -> None
  in
  let guid = match find (function `Guid _ -> true | _ -> false) l with
    | Some (`Guid g) -> g
    | _ -> None
  in
  let pubDate = match find (function `PubDate _ -> true | _ -> false) l with
    | Some (`PubDate p) -> Some p
    | _ -> None
  in
  let source = match find (function `Source _ -> true | _ -> false) l with
    | Some (`Source s) -> Some s
    | _ -> None
  in
  `Item ({ story;
           link;
           author;
           category;
           comments;
           enclosure;
           guid;
           pubDate;
           source; } : item)

let item_title_of_xml (pos, tag, datas) =
  try `Title(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <title> MUST be \
                             a non-empty string"))

let item_description_of_xml (pos, tag, datas) =
  `Description(try get_leaf datas
               with Not_found -> "")

let item_link_of_xml (pos, tag, datas) =
  `Link(try Some(Uri.of_string (get_leaf datas))
        with Not_found -> None)

let item_author_of_xml (pos, tag, datas) =
  try `Author(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <author> MUST be \
                             a non-empty string"))

let item_comments_of_xml (pos, tag, datas) =
  try `Comments(Uri.of_string (get_leaf datas))
  with Not_found -> raise (Error.Error (pos,
                            "The content of <comments> MUST be \
                             a non-empty string"))

let item_pubdate_of_xml (pos, tag, datas) =
  try `PubDate(Date.of_rfc822 (get_leaf datas))
  with Not_found -> raise (Error.Error (pos,
                            "The content of <pubDate> MUST be \
                             a non-empty string"))

let item_of_xml =
  let data_producer = [
    ("title", item_title_of_xml);
    ("description", item_description_of_xml);
    ("link", item_link_of_xml);
    ("author", item_author_of_xml);
    ("category", category_of_xml);
    ("comments", item_comments_of_xml);
    ("enclosure", enclosure_of_xml);
    ("guid", guid_of_xml);
    ("pubDate", item_pubdate_of_xml);
    ("source", source_of_xml);
  ] in
  generate_catcher ~data_producer make_item

let item_of_xml' =
  let data_producer = [
    ("title", dummy_of_xml ~ctor:(fun a -> `Title a));
    ("description", dummy_of_xml ~ctor:(fun a -> `Description a));
    ("link", dummy_of_xml ~ctor:(fun a -> `Link a));
    ("author", dummy_of_xml ~ctor:(fun a -> `Author a));
    ("category", category_of_xml');
    ("comments", dummy_of_xml ~ctor:(fun a -> `Comments a));
    ("enclosure", enclosure_of_xml');
    ("guid", guid_of_xml');
    ("pubdate", dummy_of_xml ~ctor:(fun a -> `PubDate a));
    ("source", source_of_xml');
  ] in
  generate_catcher ~data_producer (fun ~pos x -> `Item x)

type channel =
  {
    title: string;
    link: Uri.t;
    description: string;
    language: string option;
    copyright: string option;
    managingEditor: string option;
    webMaster: string option;
    pubDate: Date.t option;
    lastBuildDate: Date.t option;
    category: string option;
    generator: string option;
    docs: Uri.t option;
    cloud: cloud option;
    ttl: int option;
    image: image option;
    rating: int option;
    textInput: textinput option;
    skipHours: int option;
    skipDays: int option;
    items: item list;
  }

type channel' = [
  | `Title of string
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
  | `Item of item
]

let make_channel ~pos (l : [< channel' ] list) =
  let title = match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title t) -> t
    | _ ->
      raise (Error.Error (pos,
                            "<channel> elements MUST contains exactly one \
                             <title> element"))
  in
  let link = match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link l) -> l
    | _ ->
      raise (Error.Error (pos,
                            "<channel> elements MUST contains exactly one \
                             <link> element"))
  in
  let description =
    match find (function `Description _ -> true | _ -> false) l with
    | Some (`Description l) -> l
    | _ ->
      raise (Error.Error (pos,
                            "<channel> elements MUST contains exactly one \
                             <description> element"))
  in
  let language = match find (function `Language _ -> true | _ -> false) l with
    | Some (`Language a) -> Some a
    | _ -> None
  in
  let copyright = match find (function `Copyright _ -> true | _ -> false) l with
    | Some (`Copyright a) -> Some a
    | _ -> None
  in
  let managingEditor =
    match find (function `ManagingEditor _ -> true | _ -> false) l with
    | Some (`ManagingEditor a) -> Some a
    | _ -> None
  in
  let webMaster = match find (function `WebMaster _ -> true | _ -> false) l with
    | Some (`WebMaster a) -> Some a
    | _ -> None
  in
  let pubDate = match find (function `PubDate _ -> true | _ -> false) l with
    | Some (`PubDate a) -> Some a
    | _ -> None
  in
  let lastBuildDate =
    match find (function `LastBuildDate _ -> true | _ -> false) l with
    | Some (`LastBuildDate a) -> Some a
    | _ -> None
  in
  let category = match find (function `Category _ -> true | _ -> false) l with
    | Some (`Category a) -> Some a
    | _ -> None
  in
  let generator = match find (function `Generator _ -> true | _ -> false) l with
    | Some (`Generator a) -> Some a
    | _ -> None
  in
  let docs = match find (function `Docs _ -> true | _ -> false) l with
    | Some (`Docs a) -> Some a
    | _ -> None
  in
  let cloud = match find (function `Cloud _ -> true | _ -> false) l with
    | Some (`Cloud a) -> Some a
    | _ -> None
  in
  let ttl = match find (function `TTL _ -> true | _ -> false) l with
    | Some (`TTL a) -> Some a
    | _ -> None
  in
  let image = match find (function `Image _ -> true | _ -> false) l with
    | Some (`Image a) -> Some a
    | _ -> None
  in
  let rating = match find (function `Rating _ -> true | _ -> false) l with
    | Some (`Rating a) -> Some a
    | _ -> None
  in
  let textInput = match find (function `TextInput _ -> true | _ -> false) l with
    | Some (`TextInput a) -> Some a
    | _ -> None
  in
  let skipHours = match find (function `SkipHours _ -> true | _ -> false) l with
    | Some (`SkipHours a) -> Some a
    | _ -> None
  in
  let skipDays = match find (function `SkipDays _ -> true | _ -> false) l with
    | Some (`SkipDays a) -> Some a
    | _ -> None
  in
  let items = List.fold_left
      (fun acc -> function `Item x -> x :: acc | _ -> acc) [] l in
  ({ title;
     link;
     description;
     language;
     copyright;
     managingEditor;
     webMaster;
     pubDate;
     lastBuildDate;
     category;
     generator;
     docs;
     cloud;
     ttl;
     image;
     rating;
     textInput;
     skipHours;
     skipDays;
     items; } : channel)

let channel_title_of_xml (pos, tag, datas) =
  try `Title(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <title> MUST be \
                             a non-empty string"))

let channel_description_of_xml (pos, tag, datas) =
  `Description(try get_leaf datas
               with Not_found -> "")

let channel_link_of_xml (pos, tag, datas) =
  try `Link(Uri.of_string (get_leaf datas))
  with Not_found -> raise (Error.Error (pos,
                            "The content of <link> MUST be \
                             a non-empty string"))

let channel_language_of_xml (pos, tag, datas) =
  try `Language(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <language> MUST be \
                             a non-empty string"))

let channel_copyright_of_xml (pos, tag, datas) =
  try `Copyright(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <copyright> MUST be \
                             a non-empty string"))

let channel_managingeditor_of_xml (pos, tag, datas) =
  try `ManagingEditor(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <managingEditor> MUST be \
                             a non-empty string"))

let channel_webmaster_of_xml (pos, tag, datas) =
  try `WebMaster(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <webMaster> MUST be \
                             a non-empty string"))

let channel_pubdate_of_xml (pos, tag, datas) =
  try `PubDate(Date.of_rfc822 (get_leaf datas))
  with Not_found -> raise (Error.Error (pos,
                            "The content of <pubDate> MUST be \
                             a non-empty string"))

let channel_lastbuilddate_of_xml (pos, tag, datas) =
  try `LastBuildDate(Date.of_rfc822 (get_leaf datas))
  with Not_found -> raise (Error.Error (pos,
                            "The content of <lastBuildDate> MUST be \
                             a non-empty string"))

let channel_category_of_xml (pos, tag, datas) =
  try `Category(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <category> MUST be \
                             a non-empty string"))

let channel_generator_of_xml (pos, tag, datas) =
  try `Generator(get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <generator> MUST be \
                             a non-empty string"))

let channel_docs_of_xml (pos, tag, datas) =
  try `Docs(Uri.of_string (get_leaf datas))
  with Not_found -> raise (Error.Error (pos,
                            "The content of <docs> MUST be \
                             a non-empty string"))

let channel_ttl_of_xml (pos, tag, datas) =
  try `TTL(int_of_string (get_leaf datas))
  with _ -> raise (Error.Error (pos,
                            "The content of <ttl> MUST be \
                             a non-empty string representing an integer"))

let channel_rating_of_xml (pos, tag, datas) =
  try `Rating(int_of_string (get_leaf datas))
  with _ -> raise (Error.Error (pos,
                            "The content of <rating> MUST be \
                             a non-empty string representing an integer"))

let channel_skipHours_of_xml (pos, tag, datas) =
  try `SkipHours(int_of_string (get_leaf datas))
  with _ -> raise (Error.Error (pos,
                            "The content of <skipHours> MUST be \
                             a non-empty string representing an integer"))

let channel_skipDays_of_xml (pos, tag, datas) =
  try `SkipDays(int_of_string (get_leaf datas))
  with _ -> raise (Error.Error (pos,
                            "The content of <skipDays> MUST be \
                             a non-empty string representing an integer"))

let channel_of_xml =
  let data_producer = [
    ("title", channel_title_of_xml);
    ("link", channel_link_of_xml);
    ("description", channel_description_of_xml);
    ("Language", channel_language_of_xml);
    ("copyright", channel_copyright_of_xml);
    ("managingeditor", channel_managingeditor_of_xml);
    ("webmaster", channel_webmaster_of_xml);
    ("pubdate", channel_pubdate_of_xml);
    ("lastbuilddate", channel_lastbuilddate_of_xml);
    ("category", channel_category_of_xml);
    ("generator", channel_generator_of_xml);
    ("docs", channel_docs_of_xml);
    ("cloud", cloud_of_xml);
    ("ttl", channel_ttl_of_xml);
    ("image", image_of_xml);
    ("rating", channel_rating_of_xml);
    ("textinput", textinput_of_xml);
    ("skiphours", channel_skipHours_of_xml);
    ("skipdays", channel_skipDays_of_xml);
    ("item", item_of_xml);
  ] in
  generate_catcher ~data_producer make_channel

let channel_of_xml' =
  let data_producer = [
    ("title", dummy_of_xml ~ctor:(fun a -> `Title a));
    ("link", dummy_of_xml ~ctor:(fun a -> `Link a));
    ("description", dummy_of_xml ~ctor:(fun a -> `Description a));
    ("Language", dummy_of_xml ~ctor:(fun a -> `Language a));
    ("copyright", dummy_of_xml ~ctor:(fun a -> `Copyright a));
    ("managingeditor", dummy_of_xml ~ctor:(fun a -> `ManagingEditor a));
    ("webmaster", dummy_of_xml ~ctor:(fun a -> `WebMaster a));
    ("pubdate", dummy_of_xml ~ctor:(fun a -> `PubDate a));
    ("lastbuilddate", dummy_of_xml ~ctor:(fun a -> `LastBuildDate a));
    ("category", dummy_of_xml ~ctor:(fun a -> `Category a));
    ("generator", dummy_of_xml ~ctor:(fun a -> `Generator a));
    ("docs", dummy_of_xml ~ctor:(fun a -> `Docs a));
    ("cloud", cloud_of_xml');
    ("ttl", dummy_of_xml ~ctor:(fun a -> `TTL a));
    ("image", image_of_xml');
    ("rating", dummy_of_xml ~ctor:(fun a -> `Rating a));
    ("textinput", textinput_of_xml');
    ("skiphours", dummy_of_xml ~ctor:(fun a -> `SkipHours a));
    ("skipdays", dummy_of_xml ~ctor:(fun a -> `SkipDays a));
    ("item", item_of_xml');
  ] in
  generate_catcher ~data_producer (fun ~pos x -> x)

let find_channel l =
  find (function XML.Node(pos, tag, data) -> tag_is tag "channel"
                | XML.Data _ -> false) l

let parse input =
  match XML.of_xmlm input |> snd with
  | XML.Node (pos, tag, data) ->
     if tag_is tag "channel" then
       channel_of_xml (pos, tag, data)
     else (
       match find_channel data with
       | Some(XML.Node(p, t, d)) -> channel_of_xml (p, t, d)
       | Some(XML.Data _)
       | _ -> raise (Error.Error ((0, 0),
                              "document MUST contains exactly one \
                               <channel> element")))
  | _ -> raise (Error.Error ((0, 0),
                         "document MUST contains exactly one \
                          <channel> element"))

let unsafe input =
  match XML.of_xmlm input |> snd with
  | XML.Node (pos, tag, data) ->
     if tag_is tag "channel" then `Channel (channel_of_xml' (pos, tag, data))
     else (match find_channel data with
           | Some(XML.Node(p, t, d)) -> `Channel (channel_of_xml' (p, t, d))
           | Some(XML.Data _) | None -> `Channel [])
  | _ -> `Channel []


(* Conversion to Atom *)

let map_option o f = match o with
  | None -> None
  | Some v -> Some(f v)

let cmp_date_opt d1 d2 = match d1, d2 with
  | Some d1, Some d2 -> Date.compare d1 d2
  | Some _, None -> 1
  | None, Some _ -> -1
  | None, None -> 0

let epoch = Date.from_unixfloat 0. (* 1970-1-1 *)

let entry_of_item (it: item) : Atom.entry =
  let author = match it.author with
    | Some a -> { Atom.name = a;  uri = None;  email = Some a }
    | None -> { Atom.name = "";  uri = None;  email = None } in
  let categories =
    match it.category with
    | Some c -> [ { Atom.term = c.data;
                   scheme = map_option c.domain (fun d -> d);
                   label = None } ]
    | None -> [] in
  let (title: Atom.title), content = match it.story with
    | All(t, d) -> Atom.Text t, Some(Atom.Html d)
    | Title t -> Atom.Text t, None
    | Description d -> Atom.Text "", Some(Atom.Html d) in
  let id = match it.guid with
    | Some g -> Uri.to_string g.data
    | None -> match it.link with
             | Some l -> Uri.to_string l
             | None ->
                let s = match it.story with
                  | All(t, d) -> t ^ d
                  | Title t -> t
                  | Description d -> d in
                Digest.to_hex (Digest.string s) in
  let links = match it.link with
    | Some l -> [ { Atom.href = l;  rel = Atom.Alternate;
                   type_media = None;  hreflang = None;  title = None;
                   length = None } ]
    | None -> [] in
  let links = match it.comments with
    | Some l -> { Atom.href = l;  rel = Atom.Related;
                 type_media = None;  hreflang = None;  title = None;
                 length = None }
               :: links
    | None -> links in
  let links = match it.enclosure with
    | Some e -> { Atom.href = e.url;  rel = Atom.Enclosure;
                 type_media = Some e.mime;
                 hreflang = None;  title = None;  length = Some e.length }
               :: links
    | None -> links in
  let source = match it.source with
    | Some s ->
       Some
         { Atom.authors = [author]; (* Best guess *)
           categories = [];
           contributors = [];
           generator = None;
           icon = None;
           id;
           links = [ { Atom.href = s.url;  rel = Atom.Related;
                       type_media = None;  hreflang = None;  title = None;
                       length = None} ];
           logo = None;
           rights = None;
           subtitle = None;
           title = Atom.Text s.data;
           updated = None }
    | None -> None in
  { Atom.
    authors = (author, []);
    categories;
    content;
    contributors = [];
    id;
    links;
    published = None;
    rights = None;
    source;
    summary = None;
    title;
    updated = (match it.pubDate with
               | Some d -> d
               | None -> epoch);
  }

let to_atom (ch: channel) : Atom.feed =
  let contributors = match ch.webMaster with
    | Some p -> [ { Atom.name = "Webmaster";  uri = None;  email = Some p } ]
    | None -> [] in
  let contributors = match ch.managingEditor with
    | Some p -> { Atom.name = "Managing Editor";  uri = None;  email = Some p }
               :: contributors
    | None -> contributors in
  let updated =
    let d = List.map (fun (it: item) -> it.pubDate) ch.items in
    let d = List.sort cmp_date_opt (ch.lastBuildDate :: d) in
    match d with
    | Some d :: _ -> d
    | None :: _ -> epoch
    | [] -> assert false in
  { Atom.authors = [];
    categories = (match ch.category with
                  | None -> []
                  | Some c -> [ { Atom.term =c;
                                 scheme = None;  label = None} ]);
    contributors;
    generator = map_option ch.generator
                           (fun g -> { Atom.content = g;
                                    version = None;  uri = None });
    icon = None;
    id = Uri.to_string ch.link; (* FIXME: Best we can do? *)
    links = [ { Atom.href = ch.link;  rel = Atom.Related;
                type_media = Some "text/html";  hreflang = None;
                title = None;  length = None } ];
    logo = map_option ch.image (fun i -> i.url);
    rights = map_option ch.copyright (fun c -> (Atom.Text c: Atom.rights));
    subtitle = None;
    title = Atom.Text ch.title;
    updated;
    entries = List.map entry_of_item ch.items;
  }
