let input = Xmlm.make_input (`Channel stdin)

type tree =
  | Node of Xmlm.tag * tree list
  | Leaf of string

type state =
  | Root
  | Entry

type rel =
  | Alternate
  | Related
  | Self
  | Enclosure
  | Via
  | Link of Uri.t

type type_content =
  | Html
  | Text
  | Xhtml
  | Mime of string

type content = { ty: type_content; src: Uri.t option; }

type author = {
  name: string;
  uri: Uri.t option;
  email: string option;
}

type category = {
  term: string;
  scheme: Uri.t option;
  label: string option;
}

type generator = {
  version: string option;
  uri: Uri.t option;
}

type link = {
  href: Uri.t; (* iri *)
  rel: rel;
  type_media: string option;
  hreflang: string option;
  title: string option;
  length: int option;
}

type source = {
  author: author * author list;
  category: category list;
  contributor: author list;
  generator: (generator * string) option;
  icon: Uri.t option;
  id: Uri.t;
  link: link * link list;
  logo: Uri.t option;
  rights: string option;
  subtitle: string option;
  title: string;
  updated: string option; (* date *)
}

type source' = [
  | `SourceAuthor of author
  | `SourceCategory of category
  | `SourceContributor of author
  | `SourceGenerator of (generator * string)
  | `SourceIcon of Uri.t
  | `SourceId of Uri.t
  | `SourceLink of link
  | `SourceLogo of Uri.t
  | `SourceSubtitle of string
  | `SourceTitle of string
  | `SourceRights of string
  | `SourceUpdated of string
]

type entry = {
  (*
   * si atom:entry ne contient pas atom:author,
   * atom:author <- atom:feed/atom:author
   * sinon erreur
   *)
  author: author * author list;
  category: category list;
  content: (content * string) option;
  contributor: author list;
  id: Uri.t; (* iri *)
  (*
   * si pas de atom:content, doit contenir
   * atom:link avec rel="alternate"
   *
   * combinaison atom:link(rel="alternate"; type; hreflang)
   * doit être unique
   *)
  link: link list;
  published: string option; (* date *)
  rights: string option;
  source: source list;
  (*
   * atom:summary obligatoire si atom:entry contient atom:content avec
   * attribut src ou atom:entry codé en base64 (LOL)
   *)
  summary: string option;
  title: string;
  updated: string; (* date *)
}

type entry' = [
  | `EntryAuthor of author
  | `EntryCategory of category
  | `EntryContributor of author
  | `EntryId of Uri.t
  | `EntryLink of link
  | `EntryPublished of string
  | `EntryRights of string
  | `EntrySource of source
  | `EntryContent of (content * string)
  | `EntrySummary of string
  | `EntryTitle of string
  | `EntryUpdated of string
]

type feed = {
  (*
   * si tout les atom:entry ne contiennent pas atom:author
   * ne respecte pas la RFC
   *)
  author: author list;
  category: category list;
  contributor: author list;
  generator: (generator * string) option;
  icon: Uri.t option;
  id: Uri.t;
  (*
   * combinaison atom:link(rel="alternate"; type; hreflang)
   * doit être unique
   *)
  link: link * link list; (* self link * other link *)
  logo: Uri.t option;
  rights: string option;
  subtitle: string option;
  title: string;
  updated: string;
  entry: entry list;
}

type feed' = [
  | `FeedAuthor of author
  | `FeedCategory of category
  | `FeedContributor of author
  | `FeedGenerator of (generator * string)
  | `FeedIcon of Uri.t
  | `FeedId of Uri.t
  | `FeedLink of link
  | `FeedLogo of Uri.t
  | `FeedRights of string
  | `FeedSubtitle of string
  | `FeedTitle of string
  | `FeedUpdated of string
  | `FeedEntry of entry
]

type context = {
  state: state;
  input: Xmlm.input;
}

type opts_neturl = {
  schemes: (string, Neturl.url_syntax) Hashtbl.t;
  base_syntax: Neturl.url_syntax;
  accept_8bits: bool;
  enable_fragment: bool;
}

(* Exception *)

exception Malformed_URL of string
exception ExpectedLeaf
exception ExpectedAuthorName
exception ExpectedCategoryTerm
exception ExpectedGeneratorContent
exception ExpectedIconURI
exception ExpectedIdURI
exception ExpectedLinkHREF
exception ExpectedLogoURI
exception ExpectedPublishedDate
exception ExpectedRightsData
exception ExpectedOneAuthorSource
exception ExpectedIdSource
exception ExpectedOneLinkSource
exception ExpectedTitleSource
exception ExpectedTitleData
exception ExpectedSubtitleData
exception ExpectedUpdatedData
exception ExpectedOneAuthorEntry
exception ExpectedIdEntry
exception ExpectedTitleEntry
exception ExpectedDataSummary
exception ExpectedUpdatedEntry
exception ExpectedIdFeed
exception ExpectedOneLinkFeed
exception ExpectedTitleFeed
exception ExpectedUpdatedFeed
exception ExpectedNodeRoot

(* Util *)

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

let make_context ?enc ?strip ?ns ?entity input =
  { state = Root; input = Xmlm.make_input input; }

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

let update_ctx_state { state; input; } new_state =
  { state = new_state; input; }

(* XML to string *)

let string_of_name (prefix, name) =
  let buffer = Buffer.create 16 in
  (* Buffer.add_string buffer prefix; *)
  (* Buffer.add_char buffer ':'; *)
  Buffer.add_string buffer name;
  Buffer.contents buffer

let string_of_attr (name, value) =
  let buffer = Buffer.create 16 in
  Buffer.add_string buffer (string_of_name name);
  Buffer.add_string buffer " = ";
  Buffer.add_string buffer value;
  Buffer.contents buffer

let string_of_tag (name, attrs) =
  let buffer = Buffer.create 16 in
  Buffer.add_char buffer '<';
  Buffer.add_string buffer (string_of_name name);
  if List.length attrs = 0  then () else Buffer.add_char buffer ' ';
  (match attrs with
    | [] -> ()
    | x :: r ->
      Buffer.add_string buffer (string_of_attr x);
      List.iter (fun x -> Buffer.add_char buffer ' '; Buffer.add_string buffer (string_of_attr x)) r);
  Buffer.add_char buffer '>';
  Buffer.contents buffer

let string_of_close_tag (name, _) =
  let buffer = Buffer.create 16 in
  Buffer.add_string buffer "</";
  Buffer.add_string buffer (string_of_name name);
  Buffer.add_char buffer '>';
  Buffer.contents buffer

let string_of_xml ctx =
  let el tag datas =
    let buffer = Buffer.create 16
    in
      Buffer.add_string buffer (string_of_tag tag);
      List.iter (fun x -> Buffer.add_string buffer x) datas;
      Buffer.add_string buffer (string_of_close_tag tag);
      Buffer.contents buffer
  in let data str = str
  in let (_, str) = Xmlm.input_doc_tree ~el ~data ctx.input
  in str

(* Produce XML *)

let generate_catcher'
  ?(attr_producer=[])
  ?(data_producer=[])
  ?leaf_producer maker =
  let get_producer name map =
    try Some (List.assoc name map)
    with _ -> None
  in
  let rec catch_attr acc = function
    | attr :: r -> begin match get_producer (get_attr_name attr) attr_producer with
      | Some f -> catch_attr ((f attr) :: acc) r
      | None -> catch_attr acc r end
    | [] -> acc
  in
  let rec catch_datas acc = function
    | (Node (tag, datas)) :: r ->
      begin match get_producer (get_tag_name tag) data_producer with
      | Some f -> catch_datas ((f (tag, datas)) :: acc) r
      | None -> catch_datas acc r end
    | (Leaf str) :: r ->
      begin match leaf_producer with
      | Some f -> catch_datas ((f str) :: acc) r
      | None -> catch_datas acc r end
    | [] -> acc
  in
  let generate (tag, datas) =
    maker (catch_attr (catch_datas [] datas) (get_attrs tag))
  in generate

(* RFC Compliant (or raise error) *)

let make_author (l : [< `AuthorName of string | `AuthorURI of Uri.t | `AuthorEmail of string] list) =
  let name = match find (function `AuthorName _ -> true | _ -> false) l with
    | Some (`AuthorName s) -> s
    | _ -> raise ExpectedAuthorName
  in let uri = match find (function `AuthorURI _ -> true | _ -> false) l with
    | Some (`AuthorURI u) -> Some u
    | _ -> None
  in let email = match find (function `AuthorEmail _ -> true | _ -> false) l with
    | Some (`AuthorEmail e) -> Some e
    | _ -> None
  in ({ name; uri; email; } : author)

let author_of_xml (tag, datas) =
  let rec aux acc = function
    | Node (tag, datas) :: r when tag_is tag "name" && datas_has_leaf datas ->
      aux (`AuthorName (get_leaf datas) :: acc) r
    | Node (tag, datas) :: r when tag_is tag "uri" && datas_has_leaf datas ->
      aux (`AuthorURI (Uri.of_string (get_leaf datas)) :: acc) r
    | Node (tag, datas) :: r when tag_is tag "email" && datas_has_leaf datas ->
      aux (`AuthorEmail (get_leaf datas) :: acc) r
    | _ :: r -> aux acc r
    | [] -> acc
  in make_author (aux [] datas)

(* RFC Compliant (or raise error) *)

let make_category (l : [< `CategoryTerm of string | `CategoryScheme of Uri.t | `CategoryLabel of string] list) =
  let term = match find (function `CategoryTerm _ -> true | _ -> false) l with
    | Some (`CategoryTerm t) -> t
    | _ -> raise ExpectedCategoryTerm
  in let scheme = match find (function `CategoryScheme _ -> true | _ -> false) l with
    | Some (`CategoryScheme u) -> Some u
    | _ -> None
  in let label = match find (function `CategoryLabel _ -> true | _ -> false) l with
    | Some (`CategoryLabel l) -> Some l
    | _ -> None
  in ({ term; scheme; label; } : category)

let category_of_xml (tag, datas) =
  let rec aux acc = function
    | attr :: r when attr_is attr "term" ->
      aux ((`CategoryTerm (get_value attr)) :: acc) r
    | attr :: r when attr_is attr "scheme" ->
      aux ((`CategoryScheme (Uri.of_string (get_value attr))) :: acc) r
    | attr :: r when attr_is attr "label" ->
      aux ((`CategoryLabel (get_value attr)) :: acc) r
    | _ :: r -> aux acc r
    | [] -> acc
  in make_category (aux [] (get_attrs tag))

(* RFC Compliant (or raise error) *)

let make_contributor = make_author
let contributor_of_xml = author_of_xml

(* RFC Compliant (or raise error) *)

let make_generator (l : [< `GeneratorURI of Uri.t | `GeneratorVersion of string | `GeneratorContent of string] list) =
  let content = match find (function `GeneratorContent _ -> true | _ -> false) l with
    | Some ((`GeneratorContent c)) -> c
    | _ -> raise ExpectedGeneratorContent
  in let version = match find (function `GeneratorVersion _ -> true | _ -> false) l with
    | Some ((`GeneratorVersion v)) -> Some v
    | _ -> None
  in let uri = match find (function `GeneratorURI _ -> true | _ -> false) l with
    | Some ((`GeneratorURI u)) -> Some u
    | _ -> None
  in ({ version; uri; } : generator), content

let generator_of_xml (tag, datas) =
  let rec catch_attr acc = function
    | attr :: r when attr_is attr "version" ->
      catch_attr ((`GeneratorVersion (get_value attr)) :: acc) r
    | attr :: r when attr_is attr "uri" ->
      catch_attr ((`GeneratorURI (Uri.of_string (get_value attr))) :: acc) r
    | _ :: r -> catch_attr acc r
    | [] -> acc
  in let catch_data = if datas_has_leaf datas then [`GeneratorContent (get_leaf datas)] else [] in
  make_generator (catch_attr catch_data (get_attrs tag))

(* RFC Compliant (or raise error) *)

let make_icon (l : [< `IconURI of Uri.t] list) =
  let uri = match find (fun (`IconURI _) -> true) l with
    | Some (`IconURI u) -> u
    | _ -> raise ExpectedIconURI
  in uri

let icon_of_xml (tag, datas) =
  let catch_data =
    if datas_has_leaf datas
    then [`IconURI (Uri.of_string (get_leaf datas))]
    else []
  in make_icon catch_data

(* RFC Compliant (or raise error) *)

let make_id (l : [< `IdURI of Uri.t] list) =
  let uri = match find (fun (`IdURI _) -> true) l with
    | Some (`IdURI u) -> u
    | _ -> raise ExpectedIdURI
  in uri

let id_of_xml (tag, datas) =
  let catch_data =
    if datas_has_leaf datas
    then [`IdURI (Uri.of_string (get_leaf datas))]
    else []
  in make_id catch_data

(* RFC Compliant (or raise error) *)

let make_link (l : [< `LinkHREF of Uri.t | `LinkRel of rel | `LinkType of string | `LinkHREFLang of string | `LinkTitle of string | `LinkLength of int] list) =
  let href = match find (function `LinkHREF _ -> true | _ -> false) l with
    | Some (`LinkHREF u) -> u
    | _ -> raise ExpectedLinkHREF
  in let rel = match find (function `LinkRel _ -> true | _ -> false) l with
    | Some (`LinkRel r) -> r
    | _ -> Alternate (* cf. RFC 4287 § 4.2.7.2 *)
  in let type_media = match find (function `LinkType _ -> true | _ -> false) l with
    | Some (`LinkType t) -> Some t
    | _ -> None
  in let hreflang = match find (function `LinkHREFLang _ -> true | _ -> false) l with
    | Some (`LinkHREFLang l) -> Some l
    | _ -> None
  in let title = match find (function `LinkTitle _ -> true | _ -> false) l with
    | Some (`LinkTitle s) -> Some s
    | _ -> None
  in let length = match find (function `LinkLength _ -> true | _ -> false) l with
    | Some (`LinkLength i) -> Some i
    | _ -> None
  in ({ href; rel; type_media; hreflang; title; length; } : link)

let rel_of_string s = match String.lowercase (String.trim s) with
  | "alternate" -> Alternate
  | "related" -> Related
  | "self" -> Self
  | "enclosure" -> Enclosure
  | "via" -> Via
  | uri -> Link (Uri.of_string uri)

let link_of_xml =
  let attr_producer = [
    ("href", (function attr -> `LinkHREF (Uri.of_string (get_value attr))));
    ("rel", (function attr -> `LinkRel (rel_of_string (get_value attr))));
    ("type", (function attr -> `LinkType (get_value attr)));
    ("hreflang", (function attr -> `LinkHREFLang (get_value attr)));
    ("title", (function attr -> `LinkTitle (get_value attr)));
    ("length", (function attr -> `LinkLength (int_of_string (get_value attr))));
  ] in
  generate_catcher' ~attr_producer make_link

(* RFC Compliant (or raise error) *)

let make_logo (l : [< `LogoURI of Uri.t] list) =
  let uri = match find (fun (`LogoURI _) -> true) l with
    | Some (`LogoURI u) -> u
    | _ -> raise ExpectedLogoURI
  in uri

let logo_of_xml =
  let leaf_producer data = `LogoURI (Uri.of_string data) in
  generate_catcher' ~leaf_producer make_logo

(* RFC Compliant (or raise error) *)

let make_published (l : [< `PublishedDate of string] list) =
  let date = match find (fun (`PublishedDate _) -> true) l with
    | Some (`PublishedDate d) -> d
    | _ -> raise ExpectedPublishedDate
  in date

let published_of_xml =
  let attr_producer = [] in
  let leaf_producer data = `PublishedDate data in
  generate_catcher' ~attr_producer ~leaf_producer make_published

(* RFC Compliant (or raise error) *)

let make_rights (l : [< `RightData of string] list) =
  let rights = match find (fun (`RightData _) -> true) l with
    | Some (`RightData d) -> d
    | _ -> raise ExpectedRightsData
  in rights

let rights_of_xml =
  let attr_producer = [] in
  let leaf_producer data = `RightData data in
  generate_catcher' ~attr_producer ~leaf_producer make_rights

(* RFC Compliant (or raise error) *)

let make_title (l : [< `TitleData of string] list) =
  let title = match find (fun (`TitleData _) -> true) l with
    | Some (`TitleData d) -> d
    | _ -> raise ExpectedTitleData
  in title

let title_of_xml =
  let attr_producer = [] in
  let leaf_producer data = `TitleData data in
  generate_catcher' ~attr_producer ~leaf_producer make_title

(* RFC Compliant (or raise error) *)

let make_subtitle (l : [< `SubtitleData of string] list) =
  let subtitle = match find (fun (`SubtitleData _) -> true) l with
    | Some (`SubtitleData d) -> d
    | _ -> raise ExpectedSubtitleData
  in subtitle

let subtitle_of_xml =
  let attr_producer = [] in
  let leaf_producer data = `SubtitleData data in
  generate_catcher' ~attr_producer ~leaf_producer make_subtitle

(* RFC Compliant (or raise error) *)

let make_updated (l : [< `UpdatedData of string] list) =
  let updated = match find (fun (`UpdatedData _) -> true) l with
    | Some (`UpdatedData d) -> d
    | _ -> raise ExpectedUpdatedData
  in updated

let updated_of_xml =
  let attr_producer = [] in
  let leaf_producer data = `UpdatedData data in
  generate_catcher' ~attr_producer ~leaf_producer make_updated

(* RFC Compliant (or raise error) *)

let make_source (l : [< source'] list) =
  let author =
    (function [] -> raise ExpectedOneAuthorSource | x :: r -> x, r)
    (List.fold_left (fun acc -> function `SourceAuthor x -> x :: acc | _ -> acc) [] l)
  in let category = List.fold_left (fun acc -> function `SourceCategory x -> x :: acc | _ -> acc) [] l
  in let contributor = List.fold_left (fun acc -> function `SourceContributor x -> x :: acc | _ -> acc) [] l
  in let generator = match find (function `SourceGenerator _ -> true | _ -> false) l with
    | Some (`SourceGenerator g) -> Some g
    | _ -> None
  in let icon = match find (function `SourceIcon _ -> true | _ -> false) l with
    | Some (`SourceIcon u) -> Some u
    | _ -> None
  in let id = match find (function `SourceId _ -> true | _ -> false) l with
    | Some (`SourceId i) -> i
    | _ -> raise ExpectedIdSource
  in let link =
    (function [] -> raise ExpectedOneLinkSource | x :: r -> (x, r))
    (List.fold_left (fun acc -> function `SourceLink x -> x :: acc | _ -> acc) [] l)
  in let logo = match find (function `SourceLogo _ -> true | _ -> false) l with
    | Some (`SourceLogo u) -> Some u
    | _ -> None
  in let rights = match find (function `SourceRights _ -> true | _ -> false) l with
    | Some (`SourceRights r) -> Some r
    | _ -> None
  in let subtitle = match find (function `SourceSubtitle _ -> true | _ -> false) l with
    | Some (`SourceSubtitle s) -> Some s
    | _ -> None
  in let title = match find (function `SourceTitle _ -> true | _ -> false) l with
    | Some (`SourceTitle s) -> s
    | _ -> raise ExpectedTitleSource
  in let updated = match find (function `SourceUpdated _ -> true | _ -> false) l with
    | Some (`SourceUpdated d) -> Some d
    | _ -> None
  in ({ author; category; contributor; generator; icon; id; link; logo; rights; subtitle; title; updated; } : source)

let source_of_xml =
  let data_producer = [
    ("author", (function a -> `SourceAuthor (author_of_xml a)));
    ("category", (function a -> `SourceCategory (category_of_xml a)));
    ("contributor", (function a -> `SourceContributor (contributor_of_xml a)));
    ("generator", (function a -> `SourceGenerator (generator_of_xml a)));
    ("icon", (function a -> `SourceIcon (icon_of_xml a)));
    ("id", (function a -> `SourceId (id_of_xml a)));
    ("link", (function a -> `SourceLink (link_of_xml a)));
    ("logo", (function a -> `SourceLogo (logo_of_xml a)));
    ("rights", (function a -> `SourceRights (rights_of_xml a)));
    ("subtitle", (function a -> `SourceSubtitle (subtitle_of_xml a)));
    ("title", (function a -> `SourceTitle (title_of_xml a)));
    ("updated", (function a -> `SourceUpdated (updated_of_xml a)));
  ] in
  generate_catcher' ~data_producer make_source

(* RFC Compliant (or raise error) *)

let make_content (l : [< `ContentType of type_content | `ContentSRC of Uri.t | `ContentData of string] list) =
  let ty = match find (function `ContentType _ -> true | _ -> false) l with
    | Some (`ContentType ty) -> ty
    | _ -> Text
  in let src = match find (function `ContentSRC _ -> true | _ -> false) l with
    | Some (`ContentSRC s) -> Some s
    | _ -> None
  in let data = match find (function `ContentData _ -> true | _ -> false) l with
    | Some (`ContentData d) -> d
    | _ -> ""
  in (({ ty; src; } : content), data)

let type_content_of_string s = match String.lowercase (String.trim s) with
  | "html" -> Html
  | "text" -> Text
  | "xhtml" -> Xhtml
  | mime -> Mime mime

let content_of_xml =
  let attr_producer = [
    ("type", (fun attr -> `ContentType (type_content_of_string (get_value attr))));
    ("src", (fun attr -> `ContentSRC (Uri.of_string (get_value attr))));
  ] in let leaf_producer data = `ContentData data in
  generate_catcher' ~attr_producer ~leaf_producer make_content

(* RFC Compliant (or raise error) *)

let make_summary (l : [< `SummaryData of string] list) =
  let data = match find (fun (`SummaryData _) -> true) l with
    | Some (`SummaryData d) -> d
    | _ -> raise ExpectedDataSummary
  in data

let summary_of_xml =
  let leaf_producer data = `SummaryData data in
  generate_catcher' ~leaf_producer make_summary

(* RFC Compliant (or raise error) *)

let make_entry (l : [< entry'] list) =
  let author =
    (function [] -> raise ExpectedOneAuthorEntry | x :: r -> x, r)
    (List.fold_left (fun acc -> function `EntryAuthor x -> x :: acc | _ -> acc) [] l)
  in let category = List.fold_left (fun acc -> function `EntryCategory x -> x :: acc | _ -> acc) [] l
  in let contributor = List.fold_left (fun acc -> function `EntryContributor x -> x :: acc | _ -> acc) [] l
  in let id = match find (function `EntryId _ -> true | _ -> false) l with
    | Some (`EntryId i) -> i
    | _ -> raise ExpectedIdEntry
  in let link = List.fold_left (fun acc -> function `EntryLink x -> x :: acc | _ -> acc) [] l
  in let published = match find (function `EntryPublished _ -> true | _ -> false) l with
    | Some (`EntryPublished s) -> Some s
    | _ -> None
  in let rights = match find (function `EntryRights _ -> true | _ -> false) l with
    | Some (`EntryRights r) -> Some r
    | _ -> None
  in let source = List.fold_left (fun acc -> function `EntrySource x -> x :: acc | _ -> acc) [] l
  in let content = match find (function `EntryContent _ -> true | _ -> false) l with
    | Some (`EntryContent c) -> Some c
    | _ -> None
  in let summary = match find (function `EntrySummary _ -> true | _ -> false) l with
    | Some (`EntrySummary s) -> Some s
    | _ -> None
  in let title = match find (function `EntryTitle _ -> true | _ -> false) l with
    | Some (`EntryTitle t) -> t
    | _ -> raise ExpectedTitleEntry
  in let updated = match find (function `EntryUpdated _ -> true | _ -> false) l with
    | Some (`EntryUpdated u) -> u
    | _ -> raise ExpectedUpdatedEntry
  in ({ author; category; content; contributor; id; link; published; rights; source; summary; title; updated; } : entry)

let entry_of_xml =
  let data_producer = [
    ("author", (fun a -> `EntryAuthor (author_of_xml a)));
    ("category", (fun a -> `EntryCategory (category_of_xml a)));
    ("contributor", (fun a -> `EntryContributor (contributor_of_xml a)));
    ("id", (fun a -> `EntryId (id_of_xml a)));
    ("link", (fun a -> `EntryLink (link_of_xml a)));
    ("published", (fun a -> `EntryPublished (published_of_xml a)));
    ("rights", (fun a -> `EntryRights (rights_of_xml a)));
    ("source", (fun a -> `EntrySource (source_of_xml a)));
    ("content", (fun a -> `EntryContent (content_of_xml a)));
    ("summary", (fun a -> `EntrySummary (summary_of_xml a)));
    ("title", (fun a -> `EntryTitle (title_of_xml a)));
    ("updated", (fun a -> `EntryUpdated (updated_of_xml a)));
  ] in
  generate_catcher' ~data_producer make_entry

(* RFC Compliant (or raise error) *)

let make_feed (l : [< feed'] list) =
  let author = List.fold_left (fun acc -> function `FeedAuthor x -> x :: acc | _ -> acc) [] l
  in let category = List.fold_left (fun acc -> function `FeedCategory x -> x :: acc | _ -> acc) [] l
  in let contributor = List.fold_left (fun acc -> function `FeedContributor x -> x :: acc | _ -> acc) [] l
  in let generator = match find (function `FeedGenerator _ -> true | _ -> false) l with
    | Some (`FeedGenerator g) -> Some g
    | _ -> None
  in let icon = match find (function `FeedIcon _ -> true | _ -> false) l with
    | Some (`FeedIcon i) -> Some i
    | _ -> None
  in let id = match find (function `FeedId _ -> true | _ -> false) l with
    | Some (`FeedId i) -> i
    | _ -> raise ExpectedIdFeed
  in let link =
    (function [] -> raise ExpectedOneLinkFeed | x :: r -> (x, r))
    (List.fold_left (fun acc -> function `FeedLink x -> x :: acc | _ -> acc) [] l)
  in let logo = match find (function `FeedLogo _ -> true | _ -> false) l with
    | Some (`FeedLogo l) -> Some l
    | _ -> None
  in let rights = match find (function `FeedRights _ -> true | _ -> false) l with
    | Some (`FeedRights r) -> Some r
    | _ -> None
  in let subtitle = match find (function `FeedSubtitle _ -> true | _ -> false) l with
    | Some (`FeedSubtitle s) -> Some s
    | _ -> None
  in let title = match find (function `FeedTitle _ -> true | _ -> false) l with
    | Some (`FeedTitle t) -> t
    | _ -> raise ExpectedTitleFeed
  in let updated = match find (function `FeedUpdated _ -> true | _ -> false) l with
    | Some (`FeedUpdated u) -> u
    | _ -> raise ExpectedUpdatedFeed
  in let entry = List.fold_left (fun acc -> function `FeedEntry x -> x :: acc | _ -> acc) [] l
  in ({ author; category; contributor; generator; icon; id; link; logo; rights; subtitle; title; updated; entry; } : feed)

let feed_of_xml =
  let data_producer = [
    ("author", (fun a -> `FeedAuthor (author_of_xml a)));
    ("category", (fun a -> `FeedCategory (category_of_xml a)));
    ("contributor", (fun a -> `FeedContributor (contributor_of_xml a)));
    ("generator", (fun a -> `FeedGenerator (generator_of_xml a)));
    ("icon", (fun a -> `FeedIcon (icon_of_xml a)));
    ("id", (fun a -> `FeedId (id_of_xml a)));
    ("link", (fun a -> `FeedLink (link_of_xml a)));
    ("logo", (fun a -> `FeedLogo (logo_of_xml a)));
    ("rights", (fun a -> `FeedRights (rights_of_xml a)));
    ("subtitle", (fun a -> `FeedSubtitle (subtitle_of_xml a)));
    ("title", (fun a -> `FeedTitle (title_of_xml a)));
    ("updated", (fun a -> `FeedUpdated (updated_of_xml a)));
    ("entry", (fun a -> `FeedEntry (entry_of_xml a)));
  ] in
  generate_catcher' ~data_producer make_feed

let analyze_tree = function
  | Node (tag, datas) -> feed_of_xml (tag, datas)
  | _ -> raise ExpectedNodeRoot

let produce_tree input =
  let el tag datas = Node (tag, datas) in
  let data data = Leaf data in
  let (_, tree) = Xmlm.input_doc_tree ~el ~data input
  in tree

let () = let ctx = make_context (`Channel stdin) in
  let _ = analyze_tree (produce_tree ctx.input) in ()
