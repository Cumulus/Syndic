open Common.XML
open Common.Util

type image = {
  url: Uri.t;
  title: string;
  link: Uri.t;
  width: int; (* default 88 *)
  height: int; (* default 31 *)
  description: string option;
}

type cloud = {
  domain: Uri.t;
  port: int;
  path: string;
  registerProcedure: string;
  protocol: string;
}

type textinput = {
  title: string;
  description: string;
  name: string;
  link: Uri.t;
}

type category = {
  data: string;
  domain: Uri.t option;
}

type enclosure = {
  url: Uri.t;
  length: int;
  mime: string;
}

type guid = {
  data: Uri.t; (* must be uniq *)
  permalink: bool; (* default true *)
}

type source = {
  data: string;
  url: Uri.t;
}

(* must have title or description *)

type story =
  | All of string * string
  | Title of string
  | Description of string

type item = {
  story: story;
  link: Uri.t option;
  author:  string option; (* e-mail *)
  category: category option;
  comments: Uri.t option;
  enclosure: enclosure option;
  guid: guid option;
  pubDate: string option; (* date *)
  source: source option;
}

type channel = {
  title: string;
  link: Uri.t;
  description: string;
  language: string option;
  copyright: string option;
  managingEditor: string option; (* e-mail *)
  webMaster: string option; (* e-mail *)
  pubDate: string option; (* date *)
  lastBuildDate: string option; (* date *)
  category: string option;
  generator: string option;
  docs: Uri.t option;
  cloud: cloud option; (* lol *)
  ttl: int option;
  image: image option;
  rating: int option; (* lol *)
  textInput: textinput option;
  skipHours: int option;
  skipDays: int option;
  items: item list;
}

module Error = struct
  include Common.Error

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

  let raise_item_exceptation =
    raise Item_expectation

  let string_of_item_exceptation =
    "Item expected <title> or <description> tag"
end

(* RFC RSS 2.0 -
 * http://cyber.law.harvard.edu/rss/rss.html#ltimagegtSubelementOfLtchannelgt *)

type image' = [
  | `ImageURL of Uri.t
  | `ImageTitle of string
  | `ImageLink of Uri.t
  | `ImageWidth of int
  | `ImageHeight of int
  | `ImageDescription of string
]

let make_image (l : [< image'] list) =
  let url = match find (function `ImageURL _ -> true | _ -> false) l with
    | Some (`ImageURL u) -> u
    | _ -> Error.raise_expectation (Error.Tag "url") (Error.Tag "image")
  in
  let title = match find (function `ImageTitle _ -> true | _ -> false) l with
    | Some (`ImageTitle t) -> t
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "image")
  in
  let link = match find (function `ImageLink _ -> true | _ -> false) l with
    | Some (`ImageLink l) -> l
    | _ -> Error.raise_expectation (Error.Tag "link") (Error.Tag "image")
  in
  let width = match find (function `ImageWidth _ -> true | _ -> false) l with
    | Some (`ImageWidth w) -> w
    | _ -> 88 (* cf. RFC *)
  in
  let height = match find (function `ImageHeight _ -> true | _ -> false) l with
    | Some (`ImageHeight h) -> h
    | _ -> 31 (* cf. RFC *)
  in
  let description = match find (function `ImageDescription _ -> true | _ -> false) l with
    | Some (`ImageDescription s) -> Some s
    | _ -> None
  in
  ({ url; title; link; width; height; description } : image)

let image_url_of_xml (tag, datas) =
  try Uri.of_string (get_leaf datas)
  with Error.ExpectedLeaf ->
    Error.raise_expectation Error.Data (Error.Tag "image/url")

let image_title_of_xml (tag, datas) =
  try get_leaf datas
  with Error.ExpectedLeaf ->
    Error.raise_expectation Error.Data (Error.Tag "image/title")

let image_link_of_xml (tag, datas) =
  try Uri.of_string (get_leaf datas)
  with Error.ExpectedLeaf ->
    Error.raise_expectation Error.Data (Error.Tag "image/link")

let image_size_of_xml ~max (tag, datas) =
  try let size = int_of_string (get_leaf datas) in
    if size > max
    then Error.raise_size_exceeded (get_tag_name tag) size max
    else size
  with Error.ExpectedLeaf ->
    Error.raise_expectation Error.Data (Error.Tag ("image/" ^ (get_tag_name tag)))

let image_width_of_xml = image_size_of_xml ~max:144
let image_height_of_xml = image_size_of_xml ~max:400

let image_description_of_xml (tag, datas) =
  try get_leaf datas
  with Error.ExpectedLeaf ->
    Error.raise_expectation Error.Data (Error.Tag "image/description")

let image_of_xml =
  let data_producer = [
    ("url", (fun ctx a -> `ImageURL (image_url_of_xml a)));
    ("title", (fun ctx a -> `ImageTitle (image_title_of_xml a)));
    ("link", (fun ctx a -> `ImageLink (image_link_of_xml a)));
    ("width", (fun ctx a -> `ImageWidth (image_width_of_xml a)));
    ("height", (fun ctx a -> `ImageHeight (image_height_of_xml a)));
    ("description", (fun ctx a -> `ImageDescription (image_description_of_xml a)));
  ] in
  generate_catcher ~data_producer make_image

(* RFC RSS 2.0 -
 * http://cyber.law.harvard.edu/rss/rss.html#ltcloudgtSubelementOfLtchannelgt *)

type cloud' = [
  | `CloudDomain of Uri.t
  | `CloudPort of int
  | `CloudPath of string
  | `CloudRegisterProcedure of string
  | `CloudProtocol of string
]

let make_cloud (l : [< cloud'] list) =
  let domain = match find (function `CloudDomain _ -> true | _ -> false) l with
    | Some (`CloudDomain u) -> u
    | _ -> Error.raise_expectation (Error.Attr "domain") (Error.Tag "cloud")
  in
  let port = match find (function `CloudPort _ -> true | _ -> false) l with
    | Some (`CloudPort p) -> p
    | _ -> Error.raise_expectation (Error.Attr "port") (Error.Tag "cloud")
  in
  let path = match find (function `CloudPath _ -> true | _ -> false) l with
    | Some (`CloudPath p) -> p
    | _ -> Error.raise_expectation (Error.Attr "path") (Error.Tag "cloud")
  in
  let registerProcedure = match find (function `CloudRegisterProcedure _ -> true | _ -> false) l with
    | Some (`CloudRegisterProcedure r) -> r
    | _ -> Error.raise_expectation (Error.Attr "registerProcedure") (Error.Tag "cloud")
  in
  let protocol = match find (function `CloudProtocol _ -> true | _ -> false) l with
    | Some (`CloudProtocol p) -> p
    | _ -> Error.raise_expectation (Error.Attr "protocol") (Error.Tag "cloud")
  in
  ({ domain; port; path; registerProcedure; protocol; } : cloud)

let cloud_of_xml =
  let attr_producer = [
    ("domain", (fun ctx a -> `CloudDomain (Uri.of_string a)));
    ("port", (fun ctx a -> `CloudPort (int_of_string a)));
    ("path", (fun ctx a -> `CloudPath a)); (* it's RFC compliant ? *)
    ("registerProcedure", (fun ctx a -> `CloudRegisterProcedure a));
    ("protocol", (fun ctx a -> `CloudProtocol a));
  ] in
  generate_catcher ~attr_producer make_cloud

(* RFC RSS 2.0 -
 * http://cyber.law.harvard.edu/rss/rss.html#lttextinputgtSubelementOfLtchannelgt *)

type textinput' = [
  | `TextinputTitle of string
  | `TextinputDescription of string
  | `TextinputName of string
  | `TextinputLink of Uri.t
]

let make_textinput (l : [< textinput'] list) =
  let title = match find (function `TextinputTitle _ -> true | _ -> false) l with
    | Some (`TextinputTitle t) -> t
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "textinput")
  in
  let description = match find (function `TextinputDescription _ -> true | _ -> false) l with
    | Some (`TextinputDescription s) -> s
    | _ -> Error.raise_expectation (Error.Tag "description") (Error.Tag "textinput")
  in
  let name = match find (function `TextinputName _ -> true | _ -> false) l with
    | Some (`TextinputName s) -> s
    | _ -> Error.raise_expectation (Error.Tag "name") (Error.Tag "textinput")
  in
  let link = match find (function `TextinputLink _ -> true | _ -> false) l with
    | Some (`TextinputLink u) -> u
    | _ -> Error.raise_expectation (Error.Tag "link") (Error.Tag "textinput")
  in
  ({ title; description; name; link; } : textinput)

let textinput_title_of_xml (tag, datas) =
  try get_leaf datas
  with Error.ExpectedLeaf ->
    Error.raise_expectation Error.Data (Error.Tag "textinput/title")

let textinput_description_of_xml (tag, datas) =
  try get_leaf datas
  with Error.ExpectedLeaf ->
    Error.raise_expectation Error.Data (Error.Tag "textinput/description")

let textinput_name_of_xml (tag, datas) =
  try get_leaf datas
  with Error.ExpectedLeaf ->
    Error.raise_expectation Error.Data (Error.Tag "textinput/name")

let textinput_link_of_xml (tag, datas) =
  try Uri.of_string (get_leaf datas)
  with Error.ExpectedLeaf ->
    Error.raise_expectation Error.Data (Error.Tag "textinput/link")

let textinput_of_xml =
  let data_producer = [
    ("title", (fun ctx a -> `TextinputTitle (textinput_title_of_xml a)));
    ("description", (fun ctx a -> `TextinputDescription (textinput_description_of_xml a)));
    ("name", (fun ctx a -> `TextinputName (textinput_name_of_xml a)));
    ("link", (fun ctx a -> `TextinputLink (textinput_link_of_xml a)));
  ] in
  generate_catcher ~data_producer make_textinput

(* RFC RSS 2.0 -
 * http://cyber.law.harvard.edu/rss/rss.html#ltcategorygtSubelementOfLtitemgt *)

type category' = [
  | `CategoryData of string
  | `CategoryDomain of Uri.t
]

let make_category (l : [< category'] list) =
  let data = match find (function `CategoryData _ -> true | _ -> false) l with
    | Some (`CategoryData s)-> s
    | _ -> Error.raise_expectation Error.Data (Error.Tag "category")
  in let domain = match find (function `CategoryDomain _ -> true | _ -> false) l with
    | Some (`CategoryDomain d) -> Some d
    | _ -> None
  in
  ({ data; domain; } : category )

let category_of_xml =
  let attr_producer = [ ("domain", (fun ctx a -> `CategoryDomain (Uri.of_string a))); ] in
  let leaf_producer ctx data = `CategoryData data in
  generate_catcher ~attr_producer ~leaf_producer make_category

(* RFC RSS 2.0 -
 * http://cyber.law.harvard.edu/rss/rss.html#ltenclosuregtSubelementOfLtitemgt *)

type enclosure' = [
  | `EnclosureURL of Uri.t
  | `EnclosureLength of int
  | `EnclosureMime of string
]

let make_enclosure (l : [< enclosure'] list) =
  let url = match find (function `EnclosureURL _ -> true | _ -> false) l with
    | Some (`EnclosureURL u) -> u
    | _ -> Error.raise_expectation (Error.Attr "url") (Error.Tag "enclosure")
  in
  let length = match find (function `EnclosureLength _ -> true | _ -> false) l with
    | Some (`EnclosureLength l) -> l
    | _ -> Error.raise_expectation (Error.Attr "length") (Error.Tag "enclosure")
  in
  let mime = match find (function `EnclosureMime _ -> true | _ -> false) l with
    | Some (`EnclosureMime m) -> m
    | _ -> Error.raise_expectation (Error.Attr "type") (Error.Tag "enclosure")
  in
  ({ url; length; mime; } : enclosure)

let enclosure_of_xml =
  let attr_producer = [
    ("url", (fun ctx a -> `EnclosureURL (Uri.of_string a)));
    ("length", (fun ctx a -> `EnclosureLength (int_of_string a)));
    ("type", (fun ctx a -> `EnclosureMime a));
  ] in
  generate_catcher ~attr_producer make_enclosure

(* RFC RSS 2.0 -
 * http://cyber.law.harvard.edu/rss/rss.html#ltguidgtSubelementOfLtitemgt *)

type guid' = [
  | `GuidData of Uri.t
  | `GuidPermalink of bool
]

let make_guid (l : [< guid'] list) =
  let data = match find (function `GuidData _ -> true | _ -> false) l with
    | Some (`GuidData u) -> u
    | _ -> Error.raise_expectation Error.Data (Error.Tag "guid")
  in
  let permalink = match find (function `GuidPermalink _ -> true | _ -> false) l with
    | Some (`GuidPermalink b) -> b
    | _ -> true (* cf. RFC *)
  in
  ({ data; permalink; } : guid)

let guid_of_xml =
  let attr_producer = [ ("isPermalink", (fun ctx a -> `GuidPermalink (bool_of_string a))); ] in
  let leaf_producer ctx data = `GuidData (Uri.of_string data) in
  generate_catcher ~attr_producer ~leaf_producer make_guid

(* RFC RSS 2.0 -
 * http://cyber.law.harvard.edu/rss/rss.html#ltsourcegtSubelementOfLtitemgt *)

type source' = [
  | `SourceData of string
  | `SourceURL of Uri.t
]

let make_source (l : [< source'] list) =
  let data = match find (function `SourceData _ -> true | _ -> false) l with
    | Some (`SourceData s) -> s
    | _ -> Error.raise_expectation Error.Data (Error.Tag "source")
  in
  let url = match find (function `SourceURL _ -> true | _ -> false) l with
    | Some (`SourceURL u) -> u
    | _ -> Error.raise_expectation (Error.Attr "url") (Error.Tag "source")
  in
  ({ data; url; } : source)

let source_of_xml =
  let attr_producer = [ ("url", (fun ctx a -> `SourceURL (Uri.of_string a))); ] in
  let leaf_producer ctx data = `SourceData data in
  generate_catcher ~attr_producer ~leaf_producer make_source

(* RFC RSS 2.0 -
 * http://cyber.law.harvard.edu/rss/rss.html#hrelementsOfLtitemgt *)

type item' = [
  | `ItemTitle of string
  | `ItemDescription of string
  | `ItemLink of Uri.t
  | `ItemAuthor of string (* e-mail *)
  | `ItemCategory of category
  | `ItemComments of Uri.t
  | `ItemEnclosure of enclosure
  | `ItemGuid of guid
  | `ItemPubDate of string
  | `ItemSource of source
]

let make_item (l : [< item'] list) =
  let story = match
      find (function `ItemTitle _ -> true | _ -> false) l,
      find (function `ItemDescription _ -> true | _ -> false) l
    with
    | Some (`ItemTitle t), Some (`ItemDescription d) -> All (t, d)
    | Some (`ItemTitle t), _ -> Title t
    | _, Some (`ItemDescription d) -> Description d
    | _, _ -> Error.raise_item_exceptation
  in
  let link = match find (function `ItemLink _ -> true | _ -> false) l with
    | Some (`ItemLink l) -> Some l
    | _ -> None
  in
  let author = match find (function `ItemAuthor _ -> true | _ -> false) l with
    | Some (`ItemAuthor a) -> Some a
    | _ -> None
  in
  let category = match find (function `ItemCategory _ -> true | _ -> false) l with
    | Some (`ItemCategory c) -> Some c
    | _ -> None
  in
  let comments = match find (function `ItemComments _ -> true | _ -> false) l with
    | Some (`ItemComments c) -> Some c
    | _ -> None
  in
  let enclosure = match find (function `ItemEnclosure _ -> true | _ -> false) l with
    | Some (`ItemEnclosure e) -> Some e
    | _ -> None
  in
  let guid = match find (function `ItemGuid _ -> true | _ -> false) l with
    | Some (`ItemGuid g) -> Some g
    | _ -> None
  in
  let pubDate = match find (function `ItemPubDate _ -> true | _ -> false) l with
    | Some (`ItemPubDate p) -> Some p
    | _ -> None
  in
  let source = match find (function `ItemSource _ -> true | _ -> false) l with
    | Some (`ItemSource s) -> Some s
    | _ -> None
  in
  ({ story; link; author; category; comments; enclosure; guid; pubDate; source; } : item)

let item_title_of_xml (tag, datas) =
  try get_leaf datas
  with Error.ExpectedLeaf ->
    Error.raise_expectation Error.Data (Error.Tag "item/title")

let item_description_of_xml (tag, datas) =
  try get_leaf datas
  with Error.ExpectedLeaf ->
    Error.raise_expectation Error.Data (Error.Tag "item/description")

let item_link_of_xml (tag, datas) =
  try Uri.of_string (get_leaf datas)
  with Error.ExpectedLeaf ->
    Error.raise_expectation Error.Data (Error.Tag "item/link")

let item_author_of_xml (tag, datas) =
  try get_leaf datas
  with Error.ExpectedLeaf ->
    Error.raise_expectation Error.Data (Error.Tag "item/author")

let item_comments_of_xml (tag, datas) =
  try Uri.of_string (get_leaf datas)
  with Error.ExpectedLeaf ->
    Error.raise_expectation Error.Data (Error.Tag "item/comments")

let item_pubdate_of_xml (tag, datas) =
  try get_leaf datas
  with Error.ExpectedLeaf ->
    Error.raise_expectation Error.Data (Error.Tag "item/pubDate")

let item_of_xml =
  let data_producer = [
    ("title", (fun ctx a -> `ItemTitle (item_title_of_xml a)));
    ("description", (fun ctx a -> `ItemDescription (item_description_of_xml a)));
    ("link", (fun ctx a -> `ItemLink (item_link_of_xml a)));
    ("author", (fun ctx a -> `ItemAuthor (item_author_of_xml a)));
    ("category", (fun ctx a -> `ItemCategory (category_of_xml a)));
    ("comments", (fun ctx a -> `ItemComments (item_comments_of_xml a)));
    ("enclosure", (fun ctx a -> `ItemEnclosure (enclosure_of_xml a)));
    ("guid", (fun ctx a -> `ItemGuid (guid_of_xml a)));
    ("pubDate", (fun ctx a -> `ItemPubDate (item_pubdate_of_xml a)));
    ("source", (fun ctx a -> `ItemSource (source_of_xml a)));
  ] in
  generate_catcher ~data_producer make_item

(* RFC 2.0 -
 * http://cyber.law.harvard.edu/rss/rss.html#hrelementsOfLtitemgt *)

type channel' = [
  | `ChannelTitle of string
  | `ChannelLink of Uri.t
  | `ChannelDescription of string
  | `ChannelLanguage of string
  | `ChannelCopyright of string
  | `ChannelManagingEditor of string (* e-mail *)
  | `ChannelWebMaster of string (* e-mail *)
  | `ChannelPubDate of string (* date *)
  | `ChannelLastBuildDate of string (* date *)
  | `ChannelCategory of string
  | `ChannelGenerator of string
  | `ChannelDocs of Uri.t
  | `ChannelCloud of cloud
  | `ChannelTTL of int
  | `ChannelImage of image
  | `ChannelRatng of int
  | `ChannelTextinput of textinput
  | `ChannelSkipHours of int
  | `ChannelSkipDays of int
  | `ChannelItem of item
]
