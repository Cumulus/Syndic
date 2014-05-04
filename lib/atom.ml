open Common.XML
open Common.Util

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
   * et atom:feed ne contient atom:author
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
  link: link list;
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

module Error = struct
  include Common.Error

  exception Duplicate_Link of ((Uri.t * string * string) * (string * string))

  let raise_duplicate_string { href; type_media; hreflang; _} (type_media', hreflang') =
    let ty = (function Some a -> a | None -> "(none)") type_media in
    let hl = (function Some a -> a | None -> "(none)") hreflang in
    let ty' = (function "" -> "(none)" | s -> s) type_media' in
    let hl' = (function "" -> "(none)" | s -> s) hreflang' in
    raise (Duplicate_Link ((href, ty, hl), (ty', hl')))

  let string_of_duplicate_exception ((uri, ty, hl), (ty', hl')) =
    let buffer = Buffer.create 16 in
    Buffer.add_string buffer "Duplicate link between [href: ";
    Buffer.add_string buffer (Uri.to_string uri);
    Buffer.add_string buffer ", ty: ";
    Buffer.add_string buffer ty;
    Buffer.add_string buffer ", hl: ";
    Buffer.add_string buffer hl;
    Buffer.add_string buffer "] and [ty: ";
    Buffer.add_string buffer ty';
    Buffer.add_string buffer ", hl: ";
    Buffer.add_string buffer hl';
    Buffer.add_string buffer "]";
    Buffer.contents buffer
end

(* RFC Compliant (or raise error) *)

let make_author (l : [< `AuthorName of string | `AuthorURI of Uri.t | `AuthorEmail of string] list) =
  let name = match find (function `AuthorName _ -> true | _ -> false) l with
    | Some (`AuthorName s) -> s
    | _ -> Error.raise_expectation (Error.Tag "name") (Error.Tag "author")
  in let uri = match find (function `AuthorURI _ -> true | _ -> false) l with
    | Some (`AuthorURI u) -> Some u
    | _ -> None
  in let email = match find (function `AuthorEmail _ -> true | _ -> false) l with
    | Some (`AuthorEmail e) -> Some e
    | _ -> None
  in ({ name; uri; email; } : author)

let author_name_of_xml (tag, datas) =
  try get_leaf datas
  with _ -> "" (* mandatory ? *)

let author_uri_of_xml (tag, datas) =
  try Uri.of_string (get_leaf datas)
  with Error.ExpectedLeaf -> Error.raise_expectation Error.Data (Error.Tag "author/uri")

let author_email_of_xml (tag, datas) =
  try get_leaf datas
  with _ -> "" (* mandatory ? *)

let author_of_xml =
  let data_producer = [
    ("name", (fun ctx a -> `AuthorName (author_name_of_xml a)));
    ("uri", (fun ctx a -> `AuthorURI (author_uri_of_xml a)));
    ("email", (fun ctx a -> `AuthorEmail (author_email_of_xml a)));
  ] in
  generate_catcher ~data_producer make_author

(* RFC Compliant (or raise error) *)

let make_category (l : [< `CategoryTerm of string | `CategoryScheme of Uri.t | `CategoryLabel of string] list) =
  let term = match find (function `CategoryTerm _ -> true | _ -> false) l with
    | Some (`CategoryTerm t) -> t
    | _ -> Error.raise_expectation (Error.Attr "term") (Error.Tag "category")
  in let scheme = match find (function `CategoryScheme _ -> true | _ -> false) l with
    | Some (`CategoryScheme u) -> Some u
    | _ -> None
  in let label = match find (function `CategoryLabel _ -> true | _ -> false) l with
    | Some (`CategoryLabel l) -> Some l
    | _ -> None
  in ({ term; scheme; label; } : category)

let category_of_xml =
  let attr_producer = [
    ("term", (fun ctx a -> `CategoryTerm a));
    ("scheme", (fun ctx a -> `CategoryScheme (Uri.of_string a)));
    ("label", (fun ctx a -> `CategoryLabel a))
  ] in
  generate_catcher ~attr_producer make_category

(* RFC Compliant (or raise error) *)

let make_contributor = make_author
let contributor_of_xml = author_of_xml

(* RFC Compliant (or raise error) *)

let make_generator (l : [< `GeneratorURI of Uri.t | `GeneratorVersion of string | `GeneratorContent of string] list) =
  let content = match find (function `GeneratorContent _ -> true | _ -> false) l with
    | Some ((`GeneratorContent c)) -> c
    | _ -> Error.raise_expectation Error.Data (Error.Tag "generator")
  in let version = match find (function `GeneratorVersion _ -> true | _ -> false) l with
    | Some ((`GeneratorVersion v)) -> Some v
    | _ -> None
  in let uri = match find (function `GeneratorURI _ -> true | _ -> false) l with
    | Some ((`GeneratorURI u)) -> Some u
    | _ -> None
  in ({ version; uri; } : generator), content

let generator_of_xml =
  let attr_producer = [
    ("version", (fun ctx a -> `GeneratorVersion a));
    ("uri", (fun ctx a -> `GeneratorURI (Uri.of_string a)));
  ] in
  generate_catcher ~attr_producer make_generator

(* RFC Compliant (or raise error) *)

let make_icon (l : [< `IconURI of Uri.t] list) =
  let uri = match find (fun (`IconURI _) -> true) l with
    | Some (`IconURI u) -> u
    | _ -> Error.raise_expectation Error.Data (Error.Tag "icon")
  in uri

let icon_of_xml =
  let leaf_producer ctx data = `IconURI (Uri.of_string data)
  in generate_catcher ~leaf_producer make_icon

(* RFC Compliant (or raise error) *)

let make_id (l : [< `IdURI of Uri.t] list) =
  let uri = match find (fun (`IdURI _) -> true) l with
    | Some (`IdURI u) -> u
    | _ -> Error.raise_expectation Error.Data (Error.Tag "id")
  in uri

let id_of_xml =
  let leaf_producer ctx data = `IdURI (Uri.of_string data)
  in generate_catcher ~leaf_producer make_id

(* RFC Compliant (or raise error) *)

let make_link (l : [< `LinkHREF of Uri.t | `LinkRel of rel | `LinkType of string | `LinkHREFLang of string | `LinkTitle of string | `LinkLength of int] list) =
  let href = match find (function `LinkHREF _ -> true | _ -> false) l with
    | Some (`LinkHREF u) -> u
    | _ -> Error.raise_expectation (Error.Attr "href") (Error.Tag "link")
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
  | uri -> Link (Uri.of_string uri) (* RFC 4287 § 4.2.7.2 *)

let link_of_xml =
  let attr_producer = [
    ("href", (fun ctx a -> `LinkHREF (Uri.of_string a)));
    ("rel", (fun ctx a -> `LinkRel (rel_of_string a)));
    ("type", (fun ctx a -> `LinkType a));
    ("hreflang", (fun ctx a -> `LinkHREFLang a));
    ("title", (fun ctx a -> `LinkTitle a));
    ("length", (fun ctx a -> `LinkLength (int_of_string a)));
  ] in
  generate_catcher ~attr_producer make_link

(* RFC Compliant (or raise error) *)

let make_logo (l : [< `LogoURI of Uri.t] list) =
  let uri = match find (fun (`LogoURI _) -> true) l with
    | Some (`LogoURI u) -> u
    | _ -> Error.raise_expectation Error.Data (Error.Tag "logo")
  in uri

let logo_of_xml =
  let leaf_producer ctx data = `LogoURI (Uri.of_string data) in
  generate_catcher ~leaf_producer make_logo

(* RFC Compliant (or raise error) *)

let make_published (l : [< `PublishedDate of string] list) =
  let date = match find (fun (`PublishedDate _) -> true) l with
    | Some (`PublishedDate d) -> d
    | _ -> Error.raise_expectation Error.Data (Error.Tag "published")
  in date

let published_of_xml =
  let attr_producer = [] in
  let leaf_producer ctx data = `PublishedDate data in
  generate_catcher ~attr_producer ~leaf_producer make_published

(* RFC Compliant (or raise error) *)

let make_rights (l : [< `RightData of string] list) =
  let rights = match find (fun (`RightData _) -> true) l with
    | Some (`RightData d) -> d
    | _ -> Error.raise_expectation Error.Data (Error.Tag "rights")
  in rights

let rights_of_xml =
  let attr_producer = [] in
  let leaf_producer ctx data = `RightData data in
  generate_catcher ~attr_producer ~leaf_producer make_rights

(* RFC Compliant (or raise error) *)

let make_title (l : [< `TitleData of string] list) =
  let title = match find (fun (`TitleData _) -> true) l with
    | Some (`TitleData d) -> d
    | _ -> Error.raise_expectation Error.Data (Error.Tag "title")
  in title

let title_of_xml =
  let attr_producer = [] in
  let leaf_producer ctx data = `TitleData data in
  generate_catcher ~attr_producer ~leaf_producer make_title

(* RFC Compliant (or raise error) *)

let make_subtitle (l : [< `SubtitleData of string] list) =
  let subtitle = match find (fun (`SubtitleData _) -> true) l with
    | Some (`SubtitleData d) -> d
    | _ -> Error.raise_expectation Error.Data (Error.Tag "subtitle")
  in subtitle

let subtitle_of_xml =
  let attr_producer = [] in
  let leaf_producer ctx data = `SubtitleData data in
  generate_catcher ~attr_producer ~leaf_producer make_subtitle

(* RFC Compliant (or raise error) *)

let make_updated (l : [< `UpdatedData of string] list) =
  let updated = match find (fun (`UpdatedData _) -> true) l with
    | Some (`UpdatedData d) -> d
    | _ -> Error.raise_expectation Error.Data (Error.Tag "updated")
  in updated

let updated_of_xml =
  let attr_producer = [] in
  let leaf_producer ctx data = `UpdatedData data in
  generate_catcher ~attr_producer ~leaf_producer make_updated

(* RFC Compliant (or raise error) *)

let make_source (l : [< source'] list) =
  let author =
    (function [] -> Error.raise_expectation (Error.Tag "author") (Error.Tag "source") | x :: r -> x, r)
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
    | _ -> Error.raise_expectation (Error.Tag "id") (Error.Tag "source")
  in let link =
    (function [] -> Error.raise_expectation (Error.Tag "link") (Error.Tag "source") | x :: r -> (x, r))
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
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "source")
  in let updated = match find (function `SourceUpdated _ -> true | _ -> false) l with
    | Some (`SourceUpdated d) -> Some d
    | _ -> None
  in ({ author; category; contributor; generator; icon; id; link; logo; rights; subtitle; title; updated; } : source)

let source_of_xml =
  let data_producer = [
    ("author", (fun ctx a -> `SourceAuthor (author_of_xml a)));
    ("category", (fun ctx a -> `SourceCategory (category_of_xml a)));
    ("contributor", (fun ctx a -> `SourceContributor (contributor_of_xml a)));
    ("generator", (fun ctx a -> `SourceGenerator (generator_of_xml a)));
    ("icon", (fun ctx a -> `SourceIcon (icon_of_xml a)));
    ("id", (fun ctx a -> `SourceId (id_of_xml a)));
    ("link", (fun ctx a -> `SourceLink (link_of_xml a)));
    ("logo", (fun ctx a -> `SourceLogo (logo_of_xml a)));
    ("rights", (fun ctx a -> `SourceRights (rights_of_xml a)));
    ("subtitle", (fun ctx a -> `SourceSubtitle (subtitle_of_xml a)));
    ("title", (fun ctx a -> `SourceTitle (title_of_xml a)));
    ("updated", (fun ctx a -> `SourceUpdated (updated_of_xml a)));
  ] in
  generate_catcher ~data_producer make_source

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
    ("type", (fun ctx a -> `ContentType (type_content_of_string a)));
    ("src", (fun ctx a -> `ContentSRC (Uri.of_string a)));
  ] in let leaf_producer ctx data = `ContentData data in
  generate_catcher ~attr_producer ~leaf_producer make_content

(* RFC Compliant (or raise error) *)

let make_summary (l : [< `SummaryData of string] list) =
  let data = match find (fun (`SummaryData _) -> true) l with
    | Some (`SummaryData d) -> d
    | _ -> Error.raise_expectation Error.Data (Error.Tag "summary")
  in data

let summary_of_xml =
  let leaf_producer ctx data = `SummaryData data in
  generate_catcher ~leaf_producer make_summary

(* RFC Compliant (or raise error) *)

module LinkOrder
  : Set.OrderedType with type t = string * string =
struct
  type t = string * string
  let compare (a : t) (b : t) = match compare (fst a) (fst b) with
    | 0 -> compare (snd a) (snd b)
    | n -> n
end

module LinkSet = Set.Make(LinkOrder)

let uniq_link_alternate (l : link list) =
  let rec aux acc = function
    | [] -> l
    | ({ rel; type_media = Some ty; hreflang = Some hl; _ } as x) :: r when rel = Alternate ->
      if LinkSet.mem (ty, hl) acc
      then Error.raise_duplicate_string x (LinkSet.find (ty, hl) acc)
      else aux (LinkSet.add (ty, hl) acc) r
    | ({ rel; type_media = None; hreflang = Some hl; _ } as x) :: r when rel = Alternate ->
      if LinkSet.mem ("", hl) acc
      then Error.raise_duplicate_string x (LinkSet.find ("", hl) acc)
      else aux (LinkSet.add ("", hl) acc) r
    | ({ rel; type_media = Some ty; hreflang = None; _ } as x) :: r when rel = Alternate ->
      if LinkSet.mem (ty, "") acc
      then Error.raise_duplicate_string x (LinkSet.find (ty, "") acc)
      else aux (LinkSet.add (ty, "") acc) r
    | ({ rel; type_media = None; hreflang = None; _ } as x) :: r when rel = Alternate ->
      if LinkSet.mem ("", "") acc
      then Error.raise_duplicate_string x (LinkSet.find ("", "") acc)
      else aux (LinkSet.add ("", "") acc) r
    | x :: r -> aux acc r
  in aux LinkSet.empty l

let make_entry (feed : [< feed'] list) (l : [< entry'] list) =
  let feed_author = match find (function `FeedAuthor _ -> true | _ -> false) feed with
    | Some (`FeedAuthor a) -> Some a
    | _ -> None
  in let author =
    (* default author is feed/author, cf. RFC 4287 § 4.1.2 *)
    (function
      | None, [] ->
        Error.raise_expectation
          (Error.Tag "author")
          (Error.Tag "entry")
      | Some a, [] -> a, []
      | _, x :: r -> x, r)
      (feed_author, List.fold_left (fun acc -> function `EntryAuthor x -> x :: acc | _ -> acc) [] l)
  in let category = List.fold_left (fun acc -> function `EntryCategory x -> x :: acc | _ -> acc) [] l
  in let contributor = List.fold_left (fun acc -> function `EntryContributor x -> x :: acc | _ -> acc) [] l
  in let id = match find (function `EntryId _ -> true | _ -> false) l with
    | Some (`EntryId i) -> i
    | _ -> Error.raise_expectation (Error.Tag "id") (Error.Tag "entry")
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
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "entry")
  in let updated = match find (function `EntryUpdated _ -> true | _ -> false) l with
    | Some (`EntryUpdated u) -> u
    | _ -> Error.raise_expectation (Error.Tag "updated") (Error.Tag "entry")
  in ({ author; category; content; contributor; id; link = uniq_link_alternate link; published; rights; source; summary; title; updated; } : entry)

let entry_of_xml feed =
  let data_producer = [
    ("author", (fun ctx a -> `EntryAuthor (author_of_xml a)));
    ("category", (fun ctx a -> `EntryCategory (category_of_xml a)));
    ("contributor", (fun ctx a -> `EntryContributor (contributor_of_xml a)));
    ("id", (fun ctx a -> `EntryId (id_of_xml a)));
    ("link", (fun ctx a -> `EntryLink (link_of_xml a)));
    ("published", (fun ctx a -> `EntryPublished (published_of_xml a)));
    ("rights", (fun ctx a -> `EntryRights (rights_of_xml a)));
    ("source", (fun ctx a -> `EntrySource (source_of_xml a)));
    ("content", (fun ctx a -> `EntryContent (content_of_xml a)));
    ("summary", (fun ctx a -> `EntrySummary (summary_of_xml a)));
    ("title", (fun ctx a -> `EntryTitle (title_of_xml a)));
    ("updated", (fun ctx a -> `EntryUpdated (updated_of_xml a)));
  ] in
  generate_catcher ~data_producer (make_entry feed)

(* RFC Compliant (or raise error) *)

let make_feed (l : [< feed'] list) =
  let author = List.fold_left (fun acc -> function `FeedAuthor x -> x :: acc | _ -> acc) [] l
  in let category = List.fold_left (fun acc -> function `FeedCategory x -> x :: acc | _ -> acc) [] l
  in let contributor = List.fold_left (fun acc -> function `FeedContributor x -> x :: acc | _ -> acc) [] l
  in let link = List.fold_left (fun acc -> function `FeedLink x -> x :: acc | _ -> acc) [] l
  in let generator = match find (function `FeedGenerator _ -> true | _ -> false) l with
    | Some (`FeedGenerator g) -> Some g
    | _ -> None
  in let icon = match find (function `FeedIcon _ -> true | _ -> false) l with
    | Some (`FeedIcon i) -> Some i
    | _ -> None
  in let id = match find (function `FeedId _ -> true | _ -> false) l with
    | Some (`FeedId i) -> i
    | _ -> Error.raise_expectation (Error.Tag "id") (Error.Tag "feed")
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
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "feed")
  in let updated = match find (function `FeedUpdated _ -> true | _ -> false) l with
    | Some (`FeedUpdated u) -> u
    | _ -> Error.raise_expectation (Error.Tag "updated") (Error.Tag "feed")
  in let entry = List.fold_left (fun acc -> function `FeedEntry x -> x :: acc | _ -> acc) [] l
  in ({ author; category; contributor; generator; icon; id; link; logo; rights; subtitle; title; updated; entry; } : feed)

let feed_of_xml =
  let data_producer = [
    ("author", (fun ctx a -> `FeedAuthor (author_of_xml a)));
    ("category", (fun ctx a -> `FeedCategory (category_of_xml a)));
    ("contributor", (fun ctx a -> `FeedContributor (contributor_of_xml a)));
    ("generator", (fun ctx a -> `FeedGenerator (generator_of_xml a)));
    ("icon", (fun ctx a -> `FeedIcon (icon_of_xml a)));
    ("id", (fun ctx a -> `FeedId (id_of_xml a)));
    ("link", (fun ctx a -> `FeedLink (link_of_xml a)));
    ("logo", (fun ctx a -> `FeedLogo (logo_of_xml a)));
    ("rights", (fun ctx a -> `FeedRights (rights_of_xml a)));
    ("subtitle", (fun ctx a -> `FeedSubtitle (subtitle_of_xml a)));
    ("title", (fun ctx a -> `FeedTitle (title_of_xml a)));
    ("updated", (fun ctx a -> `FeedUpdated (updated_of_xml a)));
    ("entry", (fun ctx a -> `FeedEntry (entry_of_xml ctx a)));
  ] in
  generate_catcher ~data_producer make_feed

let analyze input =
  let el tag datas = Node (tag, datas) in
  let data data = Leaf data in
  let (_, tree) = Xmlm.input_doc_tree ~el ~data input in
  let aux = function
    | Node (tag, datas) when tag_is tag "feed" -> feed_of_xml (tag, datas)
    | _ -> Error.raise_expectation (Error.Tag "feed") Error.Root
  in aux tree
