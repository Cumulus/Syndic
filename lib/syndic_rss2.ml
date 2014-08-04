module XML = Syndic_common.XML
open Syndic_common.Util

module Date = struct
  open CalendarLib
  open Printf
  open Scanf

  let month_to_int = Hashtbl.create 12
  let () =
    let add m i = Hashtbl.add month_to_int m i in
    add "Jan" 1;  add "Feb" 2;   add "Mar" 3;  add "Apr" 4;
    add "May" 5;  add "Jun" 6;   add "Jul" 7;  add "Aug" 8;
    add "Sep" 9;  add "Oct" 10;  add "Nov" 11; add "Dec" 12

  (* Format: http://www.rssboard.org/rss-specification#ltpubdategtSubelementOfLtitemgt
     Examples: Sun, 19 May 2002 15:21:36 GMT
               Sat, 25 Sep 2010 08:01:00 -0700 *)
  let of_string s =
    let make_date day month year h m s z =
      let month = Hashtbl.find month_to_int month in
      let date = Calendar.Date.make year month day in
      let t = Calendar.Time.(make h m (Second.from_float s)) in
      if z = "" || z = "GMT" then
        Calendar.(create date t)
      else
        let zh = int_of_string(String.sub z 0 3)
        and zm = int_of_string(String.sub z 3 2) in
        let tz = Calendar.Time.(Period.make zh zm (Second.from_int 0)) in
        Calendar.(create date (Time.add t tz))
    in
    try sscanf s "%_s %i %s %i %i:%i:%f %s" make_date
    with Scanf.Scan_failure _ ->
      invalid_arg(sprintf "Syndic.Rss2.Date.of_string: cannot parse %S" s)
end

module Error = struct
  include Syndic_error

  exception Size_Exceeded of string * int * int

  let raise_size_exceeded tag value max =
    raise (Size_Exceeded (tag, value, max))

  let string_of_size_exceeded (tag, value, max) =
    let buffer = Buffer.create 16 in
    Buffer.add_string buffer "Size excedeed in ";
    Buffer.add_string buffer tag;
    Buffer.add_string buffer " tag (";
    Buffer.add_string buffer (string_of_int value);
    Buffer.add_string buffer " > ";
    Buffer.add_string buffer (string_of_int max);
    Buffer.add_string buffer ")";
    Buffer.contents buffer

  exception Item_expectation

  let raise_item_exceptation () =
    raise Item_expectation

  let string_of_item_exceptation =
    "Item expected <title> or <description> tag"
end

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

let make_image (l : [< image' ] list) =
  let url = match find (function `URL _ -> true | _ -> false) l with
    | Some (`URL u) -> u
    | _ -> Error.raise_expectation (Error.Tag "url") (Error.Tag "image")
  in
  let title = match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title t) -> t
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "image")
  in
  let link = match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link l) -> l
    | _ -> Error.raise_expectation (Error.Tag "link") (Error.Tag "image")
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
  ({ url; title; link; width; height; description } : image)

let image_url_of_xml (tag, datas) =
  try Uri.of_string (get_leaf datas)
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "image/url")

let image_title_of_xml (tag, datas) =
  try get_leaf datas
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "image/title")

let image_link_of_xml (tag, datas) =
  try Uri.of_string (get_leaf datas)
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "image/link")

let image_size_of_xml ~max (tag, datas) =
  try let size = int_of_string (get_leaf datas) in
    if size > max
    then Error.raise_size_exceeded (get_tag_name tag) size max
    else size
  with Error.Expected_Leaf ->
    Error.raise_expectation
      Error.Data
      (Error.Tag ("image/" ^ (get_tag_name tag)))

let image_width_of_xml = image_size_of_xml ~max:144
let image_height_of_xml = image_size_of_xml ~max:400

let image_description_of_xml (tag, datas) =
  try get_leaf datas
  with Error.Expected_Leaf ->
    Error.raise_expectation
      Error.Data
      (Error.Tag "image/description")

let image_of_xml =
  let data_producer = [
    ("url", (fun ctx a -> `URL (image_url_of_xml a)));
    ("title", (fun ctx a -> `Title (image_title_of_xml a)));
    ("link", (fun ctx a -> `Link (image_link_of_xml a)));
    ("width", (fun ctx a -> `Width (image_width_of_xml a)));
    ("height", (fun ctx a -> `Height (image_height_of_xml a)));
    ("description", (fun ctx a -> `Description (image_description_of_xml a)));
  ] in
  XML.generate_catcher ~data_producer make_image

let image_of_xml' =
  let data_producer = [
    ("url", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `URL a)));
    ("title", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Title a)));
    ("link", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Link a)));
    ("width", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Width a)));
    ("height", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Height a)));
    ("description", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Description a)));
  ] in
  XML.generate_catcher ~data_producer (fun x -> x)

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

let make_cloud (l : [< cloud' ] list) =
  let domain = match find (function `Domain _ -> true | _ -> false) l with
    | Some (`Domain u) -> (Uri.of_string u)
    | _ -> Error.raise_expectation (Error.Attr "domain") (Error.Tag "cloud")
  in
  let port = match find (function `Port _ -> true | _ -> false) l with
    | Some (`Port p) -> (int_of_string p)
    | _ -> Error.raise_expectation (Error.Attr "port") (Error.Tag "cloud")
  in
  let path = match find (function `Path _ -> true | _ -> false) l with
    | Some (`Path p) -> p
    | _ -> Error.raise_expectation (Error.Attr "path") (Error.Tag "cloud")
  in
  let registerProcedure =
    match find (function `RegisterProcedure _ -> true | _ -> false) l with
    | Some (`RegisterProcedure r) -> r
    | _ -> Error.raise_expectation
             (Error.Attr "registerProcedure")
             (Error.Tag "cloud")
  in
  let protocol = match find (function `Protocol _ -> true | _ -> false) l with
    | Some (`Protocol p) -> p
    | _ -> Error.raise_expectation (Error.Attr "protocol") (Error.Tag "cloud")
  in
  ({ domain; port; path; registerProcedure; protocol; } : cloud)

let cloud_of_xml, cloud_of_xml' =
  let attr_producer = [
    ("domain", (fun ctx a -> `Domain a));
    ("port", (fun ctx a -> `Port a));
    ("path", (fun ctx a -> `Path a)); (* XXX: it's RFC compliant ? *)
    ("registerProcedure", (fun ctx a -> `RegisterProcedure a));
    ("protocol", (fun ctx a -> `Protocol a));
  ] in
  XML.generate_catcher ~attr_producer make_cloud,
  XML.generate_catcher ~attr_producer (fun x -> x)

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

let make_textinput (l : [< textinput'] list) =
  let title = match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title t) -> t
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "textinput")
  in
  let description =
    match find (function `Description _ -> true | _ -> false) l with
    | Some (`Description s) -> s
    | _ -> Error.raise_expectation
             (Error.Tag "description")
             (Error.Tag "textinput")
  in
  let name = match find (function `Name _ -> true | _ -> false) l with
    | Some (`Name s) -> s
    | _ -> Error.raise_expectation (Error.Tag "name") (Error.Tag "textinput")
  in
  let link = match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link u) -> u
    | _ -> Error.raise_expectation (Error.Tag "link") (Error.Tag "textinput")
  in
  ({ title; description; name; link; } : textinput)

let textinput_title_of_xml (tag, datas) =
  try get_leaf datas
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "textinput/title")

let textinput_description_of_xml (tag, datas) =
  try get_leaf datas
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "textinput/description")

let textinput_name_of_xml (tag, datas) =
  try get_leaf datas
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "textinput/name")

let textinput_link_of_xml (tag, datas) =
  try Uri.of_string (get_leaf datas)
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "textinput/link")

let textinput_of_xml =
  let data_producer = [
    ("title", (fun ctx a -> `Title (textinput_title_of_xml a)));
    ("description",
     (fun ctx a -> `Description (textinput_description_of_xml a)));
    ("name", (fun ctx a -> `Name (textinput_name_of_xml a)));
    ("link", (fun ctx a -> `Link (textinput_link_of_xml a)));
  ] in
  XML.generate_catcher ~data_producer make_textinput

let textinput_of_xml' =
  let data_producer = [
    ("title", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Title a)));
    ("description",
     (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Description a)));
    ("name", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Name a)));
    ("link", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Link a)));
  ] in
  XML.generate_catcher ~data_producer (fun x -> x)

type category =
  {
    data: string;
    domain: Uri.t option;
  }

type category' = [
  | `Data of string
  | `Domain of string
]

let make_category (l : [< category' ] list) =
  let data = match find (function `Data _ -> true | _ -> false) l with
    | Some (`Data s)-> s
    | _ -> Error.raise_expectation Error.Data (Error.Tag "category")
  in let domain = match find (function `Domain _ -> true | _ -> false) l with
    | Some (`Domain d) -> Some (Uri.of_string d)
    | _ -> None
  in
  ({ data; domain; } : category )

let category_of_xml, category_of_xml' =
  let attr_producer = [ ("domain", (fun ctx a -> `Domain a)); ] in
  let leaf_producer ctx data = `Data data in
  XML.generate_catcher ~attr_producer ~leaf_producer make_category,
  XML.generate_catcher ~attr_producer ~leaf_producer (fun x -> x)

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

let make_enclosure (l : [< enclosure' ] list) =
  let url = match find (function `URL _ -> true | _ -> false) l with
    | Some (`URL u) -> Uri.of_string u
    | _ -> Error.raise_expectation (Error.Attr "url") (Error.Tag "enclosure")
  in
  let length = match find (function `Length _ -> true | _ -> false) l with
    | Some (`Length l) -> int_of_string l
    | _ -> Error.raise_expectation (Error.Attr "length") (Error.Tag "enclosure")
  in
  let mime = match find (function `Mime _ -> true | _ -> false) l with
    | Some (`Mime m) -> m
    | _ -> Error.raise_expectation (Error.Attr "type") (Error.Tag "enclosure")
  in
  ({ url; length; mime; } : enclosure)

let enclosure_of_xml, enclosure_of_xml' =
  let attr_producer = [
    ("url", (fun ctx a -> `URL a));
    ("length", (fun ctx a -> `Length a));
    ("type", (fun ctx a -> `Mime a));
  ] in
  XML.generate_catcher ~attr_producer make_enclosure,
  XML.generate_catcher ~attr_producer (fun x -> x)

type guid =
  {
    data: Uri.t; (* must be uniq *)
    permalink: bool; (* default true *)
  }

type guid' = [
  | `Data of string
  | `Permalink of string
]

let make_guid (l : [< guid' ] list) =
  let data = match find (function `Data _ -> true | _ -> false) l with
    | Some (`Data u) -> Uri.of_string u
    | _ -> Error.raise_expectation Error.Data (Error.Tag "guid")
  in
  let permalink = match find (function `Permalink _ -> true | _ -> false) l with
    | Some (`Permalink b) -> bool_of_string b
    | _ -> true (* cf. RFC *)
  in
  ({ data; permalink; } : guid)

let guid_of_xml, guid_of_xml' =
  let attr_producer = [ ("isPermalink", (fun ctx a -> `Permalink a)); ] in
  let leaf_producer ctx data = `Data data in
  XML.generate_catcher ~attr_producer ~leaf_producer make_guid,
  XML.generate_catcher ~attr_producer ~leaf_producer (fun x -> x)

type source =
  {
    data: string;
    url: Uri.t;
  }

type source' = [
  | `Data of string
  | `URL of string
]

let make_source (l : [< source' ] list) =
  let data = match find (function `Data _ -> true | _ -> false) l with
    | Some (`Data s) -> s
    | _ -> Error.raise_expectation Error.Data (Error.Tag "source")
  in
  let url = match find (function `URL _ -> true | _ -> false) l with
    | Some (`URL u) -> Uri.of_string u
    | _ -> Error.raise_expectation (Error.Attr "url") (Error.Tag "source")
  in
  ({ data; url; } : source)

let source_of_xml, source_of_xml' =
  let attr_producer = [ ("url", (fun ctx a -> `URL a)); ] in
  let leaf_producer ctx data = `Data data in
  XML.generate_catcher ~attr_producer ~leaf_producer make_source,
  XML.generate_catcher ~attr_producer ~leaf_producer (fun x -> x)

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
    pubDate: CalendarLib.Calendar.t option; (* date *)
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
  | `PubDate of CalendarLib.Calendar.t
  | `Source of source
]

let make_item (l : [< item' ] list) =
  let story = match
      find (function `Title _ -> true | _ -> false) l,
      find (function `Description _ -> true | _ -> false) l
    with
    | Some (`Title t), Some (`Description d) -> All (t, d)
    | Some (`Title t), _ -> Title t
    | _, Some (`Description d) -> Description d
    | _, _ -> Error.raise_item_exceptation ()
  in
  let link = match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link l) -> Some l
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
    | Some (`Guid g) -> Some g
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
  ({ story;
     link;
     author;
     category;
     comments;
     enclosure;
     guid;
     pubDate;
     source; } : item)

let item_title_of_xml (tag, datas) =
  try get_leaf datas
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "item/title")

let item_description_of_xml (tag, datas) =
  try get_leaf datas
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "item/description")

let item_link_of_xml (tag, datas) =
  try Uri.of_string (get_leaf datas)
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "item/link")

let item_author_of_xml (tag, datas) =
  try get_leaf datas
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "item/author")

let item_comments_of_xml (tag, datas) =
  try Uri.of_string (get_leaf datas)
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "item/comments")

let item_pubdate_of_xml (tag, datas) =
  try Date.of_string (get_leaf datas)
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "item/pubDate")

let item_of_xml =
  let data_producer = [
    ("title", (fun ctx a -> `Title (item_title_of_xml a)));
    ("description", (fun ctx a -> `Description (item_description_of_xml a)));
    ("link", (fun ctx a -> `Link (item_link_of_xml a)));
    ("author", (fun ctx a -> `Author (item_author_of_xml a)));
    ("category", (fun ctx a -> `Category (category_of_xml a)));
    ("comments", (fun ctx a -> `Comments (item_comments_of_xml a)));
    ("enclosure", (fun ctx a -> `Enclosure (enclosure_of_xml a)));
    ("guid", (fun ctx a -> `Guid (guid_of_xml a)));
    ("pubDate", (fun ctx a -> `PubDate (item_pubdate_of_xml a)));
    ("source", (fun ctx a -> `Source (source_of_xml a)));
  ] in
  XML.generate_catcher ~data_producer make_item

let item_of_xml' =
  let data_producer = [
    ("title", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Title a)));
    ("description",
     (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Description a)));
    ("link", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Link a)));
    ("author", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Author a)));
    ("category", (fun ctx a -> `Category (category_of_xml' a)));
    ("comments", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Comments a)));
    ("enclosure", (fun ctx a -> `Enclosure (enclosure_of_xml' a)));
    ("guid", (fun ctx a -> `Guid (guid_of_xml' a)));
    ("pubdate", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `PubDate a)));
    ("source", (fun ctx a -> `Source (source_of_xml' a)));
  ] in
  XML.generate_catcher ~data_producer (fun x -> x)

type channel =
  {
    title: string;
    link: Uri.t;
    description: string;
    language: string option;
    copyright: string option;
    managingEditor: string option;
    webMaster: string option;
    pubDate: CalendarLib.Calendar.t option;
    lastBuildDate: CalendarLib.Calendar.t option;
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
  | `PubDate of CalendarLib.Calendar.t
  | `LastBuildDate of CalendarLib.Calendar.t
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

let make_channel (l : [< channel' ] list) =
  let title = match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title t) -> t
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "channel")
  in
  let link = match find (function `Link _ -> true | _ -> false) l with
    | Some (`Link l) -> l
    | _ -> Error.raise_expectation (Error.Tag "link") (Error.Tag "channel")
  in
  let description =
    match find (function `Description _ -> true | _ -> false) l with
    | Some (`Description l) -> l
    | _ -> Error.raise_expectation
             (Error.Tag "description")
             (Error.Tag "channel")
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

let channel_title_of_xml (tag, datas) =
  try get_leaf datas
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "channel/title")

let channel_description_of_xml (tag, datas) =
  try get_leaf datas
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "channel/description")

let channel_link_of_xml (tag, datas) =
  try Uri.of_string (get_leaf datas)
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "channel/link")

let channel_language_of_xml (tag, datas) =
  try get_leaf datas
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "channel/language")

let channel_copyright_of_xml (tag, datas) =
  try get_leaf datas
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "channel/copyright")

let channel_managingeditor_of_xml (tag, datas) =
  try get_leaf datas
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "channel/managingEditor")

let channel_webmaster_of_xml (tag, datas) =
  try get_leaf datas
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "channel/webMaster")

let channel_pubdate_of_xml (tag, datas) =
  try Date.of_string (get_leaf datas)
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "channel/pubDate")

let channel_lastbuilddate_of_xml (tag, datas) =
  try Date.of_string (get_leaf datas)
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "channel/lastBuildDate")

let channel_category_of_xml (tag, datas) =
  try get_leaf datas
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "channel/category")

let channel_generator_of_xml (tag, datas) =
  try get_leaf datas
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "channel/generator")

let channel_docs_of_xml (tag, datas) =
  try Uri.of_string (get_leaf datas)
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "channel/docs")

let channel_ttl_of_xml (tag, datas) =
  try int_of_string (get_leaf datas)
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "channel/ttl")

let channel_rating_of_xml (tag, datas) =
  try int_of_string (get_leaf datas)
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "channel/rating")

let channel_skipHours_of_xml (tag, datas) =
  try int_of_string (get_leaf datas)
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "channel/skipHours")

let channel_skipDays_of_xml (tag, datas) =
  try int_of_string (get_leaf datas)
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "channel/skipDays")

let channel_of_xml =
  let data_producer = [
    ("title", (fun ctx a -> `Title (channel_title_of_xml a)));
    ("link", (fun ctx a -> `Link (channel_link_of_xml a)));
    ("description", (fun ctx a -> `Description (channel_description_of_xml a)));
    ("Language", (fun ctx a -> `Language (channel_language_of_xml a)));
    ("copyright", (fun ctx a -> `Copyright (channel_copyright_of_xml a)));
    ("managingeditor",
     (fun ctx a -> `ManagingEditor (channel_managingeditor_of_xml a)));
    ("webmaster", (fun ctx a -> `WebMaster (channel_webmaster_of_xml a)));
    ("pubdate", (fun ctx a -> `PubDate (channel_pubdate_of_xml a)));
    ("lastbuilddate",
     (fun ctx a -> `LastBuildDate (channel_lastbuilddate_of_xml a)));
    ("category", (fun ctx a -> `Category (channel_category_of_xml a)));
    ("generator", (fun ctx a -> `Generator (channel_generator_of_xml a)));
    ("docs", (fun ctx a -> `Docs (channel_docs_of_xml a)));
    ("cloud", (fun ctx a -> `Cloud (cloud_of_xml a)));
    ("ttl", (fun ctx a -> `TTL (channel_ttl_of_xml a)));
    ("image", (fun ctx a -> `Image (image_of_xml a)));
    ("rating", (fun ctx a -> `Rating (channel_rating_of_xml a)));
    ("textinput", (fun ctx a -> `TextInput (textinput_of_xml a)));
    ("skiphours", (fun ctx a -> `SkipHours (channel_skipHours_of_xml a)));
    ("skipdays", (fun ctx a -> `SkipDays (channel_skipDays_of_xml a)));
    ("item", (fun ctx a -> `Item (item_of_xml a)));
  ] in
  XML.generate_catcher ~data_producer make_channel

let channel_of_xml' =
  let data_producer = [
    ("title", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Title a)));
    ("link", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Link a)));
    ("description", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Description a)));
    ("Language", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Language a)));
    ("copyright", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Copyright a)));
    ("managingeditor",
     (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `ManagingEditor a)));
    ("webmaster", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `WebMaster a)));
    ("pubdate", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `PubDate a)));
    ("lastbuilddate",
     (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `LastBuildDate a)));
    ("category", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Category a)));
    ("generator", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Generator a)));
    ("docs", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Docs a)));
    ("cloud", (fun ctx a -> `Cloud (cloud_of_xml' a)));
    ("ttl", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `TTL a)));
    ("image", (fun ctx a -> `Image (image_of_xml' a)));
    ("rating", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `Rating a)));
    ("textinput", (fun ctx a -> `TextInput (textinput_of_xml' a)));
    ("skiphours", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `SkipHours a)));
    ("skipdays", (fun ctx -> XML.dummy_of_xml ~ctor:(fun a -> `SkipDays a)));
    ("item", (fun ctx a -> `Item (item_of_xml' a)));
  ] in
  XML.generate_catcher ~data_producer (fun x -> x)

let analyze input =
  match XML.tree input with
  | XML.Node(_ (* rss *), [XML.Node(tag, datas)])
  | XML.Node (tag, datas) when tag_is tag "channel" ->
     channel_of_xml (tag, datas)
  | _ -> Error.raise_expectation (Error.Tag "channel") Error.Root

let unsafe input =
  match XML.tree input with
  | XML.Node(_ (* rss *), [XML.Node(tag, datas)])
  | XML.Node (tag, datas) when tag_is tag "channel" ->
     `Channel (channel_of_xml' (tag, datas))
  | _ -> `Channel []
