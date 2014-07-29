open Common.XML
open Common.Util

type rel =
  | Alternate
  | Related
  | Self
  | Enclosure
  | Via
  | Link of Uri.t

(* See RFC 4287 § 3.1.1
 * Text constructs MAY have a "type" attribute.  When present, the value
 * MUST be one of [Text], [Html], or [Xhtml].  If the "type" attribute
 * is not provided, Atom Processors MUST behave as though it were
 * present with a value of "text".  Unlike the atom:content element
 * defined in Section 4.1.3, MIME media types [MIMEREG] MUST NOT be used
 * as values for the "type" attribute on Text constructs.
 *)

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

(** {C See RFC 4287 § 3.2}
  * A Person construct is an element that describes a person,
  * corporation, or similar entity (hereafter, 'person').
  *
  * atomPersonConstruct =
  *    atomCommonAttributes,
  *    (element atom:name { text } {% \equiv %} [`Name]
  *     & element atom:uri { atomUri }? {% \equiv %} [`URI]
  *     & element atom:email { atomEmailAddress }? {% \equiv %} [`Email]
  *     & extensionElement* )
  *
  * This specification assigns no significance to the order of appearance
  * of the child elements in a Person construct.  Person constructs allow
  * extension Metadata elements (see Section 6.4).
  *
  * {C See RFC 4287 § 4.2.1}
  * The "atom:author" element is a Person construct that indicates the
  * author of the entry or feed.
  *
  * atomAuthor = element atom:author { atomPersonConstruct }
  *
  * If an atom:entry element does not contain atom:author elements, then
  * the atom:author elements of the contained atom:source element are
  * considered to apply.  In an Atom Feed Document, the atom:author
  * elements of the containing atom:feed element are considered to apply
  * to the entry if there are no atom:author elements in the locations
  * described above.
  *)

type author' = [
  | `Name of string
  | `URI of Uri.t
  | `Email of string
]

let make_author (l : [< author'] list) =
  (** element atom:name { text } *)
  let name = match find (function `Name _ -> true | _ -> false) l with
    | Some (`Name s) -> s
    | _ -> Error.raise_expectation (Error.Tag "name") (Error.Tag "author")
  in
  (** element atom:uri { atomUri }? *)
  let uri = match find (function `URI _ -> true | _ -> false) l with
    | Some (`URI u) -> Some u
    | _ -> None
  in
  (** element atom:email { atomEmailAddress }? *)
  let email = match find (function `Email _ -> true | _ -> false) l with
    | Some (`Email e) -> Some e
    | _ -> None
  in
  ({ name; uri; email; } : author)

let author_name_of_xml (tag, datas) =
  try get_leaf datas
  with Error.Expected_Leaf -> "" (* mandatory ? *)

let author_uri_of_xml (tag, datas) =
  try Uri.of_string (get_leaf datas)
  with Error.Expected_Leaf ->
    Error.raise_expectation Error.Data (Error.Tag "author/uri")

let author_email_of_xml (tag, datas) =
  try get_leaf datas
  with Error.Expected_Leaf -> "" (* mandatory ? *)

(** Safe generator, Unsafe generator *)

let author_of_xml, author_of_xml' =
  let data_producer = [
    ("name", (fun ctx a -> `Name (author_name_of_xml a)));
    ("uri", (fun ctx a -> `URI (author_uri_of_xml a)));
    ("email", (fun ctx a -> `Email (author_email_of_xml a)));
  ] in
  generate_catcher ~data_producer make_author,
  generate_catcher ~data_producer (fun x -> x)


(** {C See RFC 4287 § 4.2.2 }
  * The "atom:category" element conveys information about a category
  * associated with an entry or feed.  This specification assigns no
  * meaning to the content (if any) of this element.
  *
  * atomCategory =
  *    element atom:category {
  *       atomCommonAttributes,
  *       attribute term { text }, {% \equiv %} [`Term]
  *       attribute scheme { atomUri }?, {% \equiv %} [`Scheme]
  *       attribute label { text }?, {% \equiv %} [`Label]
  *       undefinedContent
  *    }
  *
  * {C See RFC 4287 § 4.2.2.1 }
  * The "term" attribute is a string that identifies the category to
  * which the entry or feed belongs.  Category elements MUST have a
  * "term" attribute.
  *
  * {C See RFC 4287 § 4.2.2.2 }
  * The "scheme" attribute is an IRI that identifies a categorization
  * scheme.  Category elements MAY have a "scheme" attribute.
  *
  * {C See RFC 4287 § 4.2.2.3 }
  * The "label" attribute provides a human-readable label for display in
  * end-user applications.  The content of the "label" attribute is
  * Language-Sensitive.  Entities such as "&amp;" and "&lt;" represent
  * their corresponding characters ("&" and "<", respectively), not
  * markup.  Category elements MAY have a "label" attribute.
  *)

type category' = [
  | `Term of string
  | `Scheme of Uri.t
  | `Label of string
]

let make_category (l : [< category'] list) =
  (** attribute term { text } *)
  let term = match find (function `Term _ -> true | _ -> false) l with
    | Some (`Term t) -> t
    | _ -> Error.raise_expectation (Error.Attr "term") (Error.Tag "category")
  in
  (** attribute scheme { atomUri }? *)
  let scheme =
    match find (function `Scheme _ -> true | _ -> false) l with
    | Some (`Scheme u) -> Some u
    | _ -> None
  in
  (** attribute label { text }? *)
  let label = match find (function `Label _ -> true | _ -> false) l with
    | Some (`Label l) -> Some l
    | _ -> None
  in
  ({ term; scheme; label; } : category)

(** Safe generator, Unsafe generator *)

let category_of_xml, category_of_xml' =
  let attr_producer = [
    ("term", (fun ctx a -> `Term a));
    ("scheme", (fun ctx a -> `Scheme (Uri.of_string a)));
    ("label", (fun ctx a -> `Label a))
  ] in
  generate_catcher ~attr_producer make_category,
  generate_catcher ~attr_producer (fun x -> x)

(** {C See RFC 4287 § 4.2.3 }
  * The "atom:contributor" element is a Person construct that indicates a
  * person or other entity who contributed to the entry or feed.
  *
  * atomContributor = element atom:contributor { atomPersonConstruct }
  *)

let make_contributor = make_author
let contributor_of_xml = author_of_xml
let contributor_of_xml' = author_of_xml'

(** {C See RFC 4287 § 4.2.4 }
 * The "atom:generator" element's content identifies the agent used to
 * generate a feed, for debugging and other purposes.
 *
 * atomGenerator = element atom:generator {
 *    atomCommonAttributes,
 *    attribute uri { atomUri }?, {% \equiv %} [`URI]
 *    attribute version { text }?, {% \equiv %} [`Version]
 *    text {% \equiv %} [`Content]
 * }
 *
 * The content of this element, when present, MUST be a string that is a
 * human-readable name for the generating agent.  Entities such as
 * "&amp;" and "&lt;" represent their corresponding characters ("&" and
 * "<" respectively), not markup.
 *
 * The atom:generator element MAY have a "uri" attribute whose value
 * MUST be an IRI reference [RFC3987].  When dereferenced, the resulting
 * URI (mapped from an IRI, if necessary) SHOULD produce a
 * representation that is relevant to that agent.
 *
 * The atom:generator element MAY have a "version" attribute that
 * indicates the version of the generating agent.
 *)

type generator' = [
  | `URI of Uri.t
  | `Version of string
  | `Content of string
]

let make_generator (l : [< generator'] list) =
  (** text *)
  let content = match find (function `Content _ -> true | _ -> false) l with
    | Some ((`Content c)) -> c
    | _ -> Error.raise_expectation Error.Data (Error.Tag "generator")
  in
  (** attribute version { text }? *)
  let version = match find (function `Version _ -> true | _ -> false) l with
    | Some ((`Version v)) -> Some v
    | _ -> None
  in
  (** attribute uri { atomUri }? *)
  let uri = match find (function `URI _ -> true | _ -> false) l with
    | Some ((`URI u)) -> Some u
    | _ -> None
  in ({ version; uri; } : generator), content

(** Safe generator, Unsafe generator *)

let generator_of_xml, generator_of_xml' =
  let attr_producer = [
    ("version", (fun ctx a -> `Version a));
    ("uri", (fun ctx a -> `URI (Uri.of_string a)));
  ] in
  let leaf_producer ctx data = `Content data in
  generate_catcher ~attr_producer ~leaf_producer make_generator,
  generate_catcher ~attr_producer ~leaf_producer (fun x -> x)

(** {C See RFC 4287 § 4.2.5 }
  * The "atom:icon" element's content is an IRI reference [RFC3987] that
  * identifies an image that provides iconic visual identification for a
  * feed.
  *
  * atomIcon = element atom:icon {
  *    atomCommonAttributes,
  *    (atomUri) {% \equiv %} [`URI]
  * }
  *
  * The image SHOULD have an aspect ratio of one (horizontal) to one
  * (vertical) and SHOULD be suitable for presentation at a small size.
  *)

type icon' = [ `URI of Uri.t ]

let make_icon (l : [< icon'] list) =
  (** (atomUri) *)
  let uri = match find (fun (`URI _) -> true) l with
    | Some (`URI u) -> u
    | _ -> Error.raise_expectation Error.Data (Error.Tag "icon")
  in uri

let icon_of_xml, icon_of_xml' =
  let leaf_producer ctx data = `URI (Uri.of_string data) in
  generate_catcher ~leaf_producer make_icon,
  generate_catcher ~leaf_producer (fun x -> x)

(** {C See RFC 4287 § 4.2.6 }
  * The "atom:id" element conveys a permanent, universally unique
  * identifier for an entry or feed.
  *
  * atomId = element atom:id {
  *    atomCommonAttributes,
  *    (atomUri) {% \equiv %} [`URI]
  * }
  *
  * Its content MUST be an IRI, as defined by [RFC3987].  Note that the
  * definition of "IRI" excludes relative references.  Though the IRI
  * might use a dereferencable scheme, Atom Processors MUST NOT assume it
  * can be dereferenced.
  *)

type id' = [ `URI of Uri.t ]

let make_id (l : [< id'] list) =
  (** (atomUri) *)
  let uri = match find (fun (`URI _) -> true) l with
    | Some (`URI u) -> u
    | _ -> Error.raise_expectation Error.Data (Error.Tag "id")
  in uri

let id_of_xml, id_of_xml' =
  let leaf_producer ctx data = `URI (Uri.of_string data) in
  generate_catcher ~leaf_producer make_id,
  generate_catcher ~leaf_producer (fun x -> x)

(* RFC Compliant (or raise error) *)

type link' = [
  | `LinkHREF of Uri.t
  | `LinkRel of rel
  | `LinkType of string
  | `LinkHREFLang of string
  | `LinkTitle of string
  | `LinkLength of int
]

let make_link (l : [< link'] list) =
  let href = match find (function `LinkHREF _ -> true | _ -> false) l with
    | Some (`LinkHREF u) -> u
    | _ -> Error.raise_expectation (Error.Attr "href") (Error.Tag "link")
  in
  let rel = match find (function `LinkRel _ -> true | _ -> false) l with
    | Some (`LinkRel r) -> r
    | _ -> Alternate (* cf. RFC 4287 § 4.2.7.2 *)
  in
  let type_media = match find (function `LinkType _ -> true | _ -> false) l with
    | Some (`LinkType t) -> Some t
    | _ -> None
  in
  let hreflang = match find (function `LinkHREFLang _ -> true | _ -> false) l with
    | Some (`LinkHREFLang l) -> Some l
    | _ -> None
  in
  let title = match find (function `LinkTitle _ -> true | _ -> false) l with
    | Some (`LinkTitle s) -> Some s
    | _ -> None
  in
  let length = match find (function `LinkLength _ -> true | _ -> false) l with
    | Some (`LinkLength i) -> Some i
    | _ -> None
  in
  ({ href; rel; type_media; hreflang; title; length; } : link)

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

type logo' = [
  | `LogoURI of Uri.t
]

let make_logo (l : [< logo'] list) =
  let uri = match find (fun (`LogoURI _) -> true) l with
    | Some (`LogoURI u) -> u
    | _ -> Error.raise_expectation Error.Data (Error.Tag "logo")
  in uri

let logo_of_xml =
  let leaf_producer ctx data = `LogoURI (Uri.of_string data) in
  generate_catcher ~leaf_producer make_logo

(* RFC Compliant (or raise error) *)

type published' = [
  | `PublishedDate of string
]

let make_published (l : [< published'] list) =
  let date = match find (fun (`PublishedDate _) -> true) l with
    | Some (`PublishedDate d) -> d
    | _ -> Error.raise_expectation Error.Data (Error.Tag "published")
  in date

let published_of_xml =
  let leaf_producer ctx data = `PublishedDate data in
  generate_catcher ~leaf_producer make_published

(* RFC Compliant (or raise error) *)

type rights' = [
  | `RightData of string
]

let make_rights (l : [< rights'] list) =
  let rights = match find (fun (`RightData _) -> true) l with
    | Some (`RightData d) -> d
    | _ -> Error.raise_expectation Error.Data (Error.Tag "rights")
  in rights

let rights_of_xml =
  let leaf_producer ctx data = `RightData data in
  generate_catcher ~leaf_producer make_rights

(* RFC Compliant (or raise error) *)

type title' = [
  | `TitleData of string
]

let make_title (l : [< title'] list) =
  let title = match find (fun (`TitleData _) -> true) l with
    | Some (`TitleData d) -> d
    | _ -> Error.raise_expectation Error.Data (Error.Tag "title")
  in title

let title_of_xml =
  let leaf_producer ctx data = `TitleData data in
  generate_catcher ~leaf_producer make_title

(* RFC Compliant (or raise error) *)

type subtitle' = [
  | `SubtitleData of string
]

let make_subtitle (l : [< subtitle'] list) =
  let subtitle = match find (fun (`SubtitleData _) -> true) l with
    | Some (`SubtitleData d) -> d
    | _ -> Error.raise_expectation Error.Data (Error.Tag "subtitle")
  in subtitle

let subtitle_of_xml =
  let leaf_producer ctx data = `SubtitleData data in
  generate_catcher ~leaf_producer make_subtitle

(* RFC Compliant (or raise error) *)

type updated' = [
  | `UpdatedData of string
]

let make_updated (l : [< updated'] list) =
  let updated = match find (fun (`UpdatedData _) -> true) l with
    | Some (`UpdatedData d) -> d
    | _ -> Error.raise_expectation Error.Data (Error.Tag "updated")
  in updated

let updated_of_xml =
  let leaf_producer ctx data = `UpdatedData data in
  generate_catcher ~leaf_producer make_updated

(* RFC Compliant (or raise error) *)

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

let make_source (l : [< source'] list) =
  let author =
    (function [] -> Error.raise_expectation (Error.Tag "author") (Error.Tag "source") | x :: r -> x, r)
      (List.fold_left (fun acc -> function `SourceAuthor x -> x :: acc | _ -> acc) [] l)
  in
  let category = List.fold_left (fun acc -> function `SourceCategory x -> x :: acc | _ -> acc) [] l in
  let contributor = List.fold_left (fun acc -> function `SourceContributor x -> x :: acc | _ -> acc) [] l in
  let generator = match find (function `SourceGenerator _ -> true | _ -> false) l with
    | Some (`SourceGenerator g) -> Some g
    | _ -> None
  in
  let icon = match find (function `SourceIcon _ -> true | _ -> false) l with
    | Some (`SourceIcon u) -> Some u
    | _ -> None
  in
  let id = match find (function `SourceId _ -> true | _ -> false) l with
    | Some (`SourceId i) -> i
    | _ -> Error.raise_expectation (Error.Tag "id") (Error.Tag "source")
  in
  let link =
    (function [] -> Error.raise_expectation (Error.Tag "link") (Error.Tag "source") | x :: r -> (x, r))
      (List.fold_left (fun acc -> function `SourceLink x -> x :: acc | _ -> acc) [] l)
  in
  let logo = match find (function `SourceLogo _ -> true | _ -> false) l with
    | Some (`SourceLogo u) -> Some u
    | _ -> None
  in
  let rights = match find (function `SourceRights _ -> true | _ -> false) l with
    | Some (`SourceRights r) -> Some r
    | _ -> None
  in
  let subtitle = match find (function `SourceSubtitle _ -> true | _ -> false) l with
    | Some (`SourceSubtitle s) -> Some s
    | _ -> None
  in
  let title = match find (function `SourceTitle _ -> true | _ -> false) l with
    | Some (`SourceTitle s) -> s
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "source")
  in
  let updated = match find (function `SourceUpdated _ -> true | _ -> false) l with
    | Some (`SourceUpdated d) -> Some d
    | _ -> None
  in
  ({ author; category; contributor; generator; icon; id; link; logo; rights; subtitle; title; updated; } : source)

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

type content' = [
  | `ContentType of type_content
  | `ContentSRC of Uri.t
  | `ContentData of string
]

let make_content (l : [< content'] list) =
  let ty = match find (function `ContentType _ -> true | _ -> false) l with
    | Some (`ContentType ty) -> ty
    | _ -> Text
  in
  let src = match find (function `ContentSRC _ -> true | _ -> false) l with
    | Some (`ContentSRC s) -> Some s
    | _ -> None
  in
  let data = match find (function `ContentData _ -> true | _ -> false) l with
    | Some (`ContentData d) -> d
    | _ -> ""
  in
  (({ ty; src; } : content), data)

let type_content_of_string s = match String.lowercase (String.trim s) with
  | "html" -> Html
  | "text" -> Text
  | "xhtml" -> Xhtml
  | mime -> Mime mime

let content_of_xml =
  let attr_producer = [
    ("type", (fun ctx a -> `ContentType (type_content_of_string a)));
    ("src", (fun ctx a -> `ContentSRC (Uri.of_string a)));
  ] in
  let leaf_producer ctx data = `ContentData data in
  generate_catcher ~attr_producer ~leaf_producer make_content

(* RFC Compliant (or raise error) *)

type summary' = [
  | `SummaryData of string
]

let make_summary (l : [< summary'] list) =
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
  in let contributor = List.fold_left (fun acc -> function `EntryContributor x -> x :: acc | _ -> acc) [] l in
  let id = match find (function `EntryId _ -> true | _ -> false) l with
    | Some (`EntryId i) -> i
    | _ -> Error.raise_expectation (Error.Tag "id") (Error.Tag "entry")
  in let link = List.fold_left (fun acc -> function `EntryLink x -> x :: acc | _ -> acc) [] l in
  let published = match find (function `EntryPublished _ -> true | _ -> false) l with
    | Some (`EntryPublished s) -> Some s
    | _ -> None
  in
  let rights = match find (function `EntryRights _ -> true | _ -> false) l with
    | Some (`EntryRights r) -> Some r
    | _ -> None
  in let source = List.fold_left (fun acc -> function `EntrySource x -> x :: acc | _ -> acc) [] l in
  let content = match find (function `EntryContent _ -> true | _ -> false) l with
    | Some (`EntryContent c) -> Some c
    | _ -> None
  in
  let summary = match find (function `EntrySummary _ -> true | _ -> false) l with
    | Some (`EntrySummary s) -> Some s
    | _ -> None
  in
  let title = match find (function `EntryTitle _ -> true | _ -> false) l with
    | Some (`EntryTitle t) -> t
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "entry")
  in
  let updated = match find (function `EntryUpdated _ -> true | _ -> false) l with
    | Some (`EntryUpdated u) -> u
    | _ -> Error.raise_expectation (Error.Tag "updated") (Error.Tag "entry")
  in
  ({ author; category; content; contributor; id; link = uniq_link_alternate link; published; rights; source; summary; title; updated; } : entry)

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
  let author = List.fold_left (fun acc -> function `FeedAuthor x -> x :: acc | _ -> acc) [] l in
  let category = List.fold_left (fun acc -> function `FeedCategory x -> x :: acc | _ -> acc) [] l in
  let contributor = List.fold_left (fun acc -> function `FeedContributor x -> x :: acc | _ -> acc) [] l in
  let link = List.fold_left (fun acc -> function `FeedLink x -> x :: acc | _ -> acc) [] l in
  let generator = match find (function `FeedGenerator _ -> true | _ -> false) l with
    | Some (`FeedGenerator g) -> Some g
    | _ -> None
  in
  let icon = match find (function `FeedIcon _ -> true | _ -> false) l with
    | Some (`FeedIcon i) -> Some i
    | _ -> None
  in
  let id = match find (function `FeedId _ -> true | _ -> false) l with
    | Some (`FeedId i) -> i
    | _ -> Error.raise_expectation (Error.Tag "id") (Error.Tag "feed")
  in
  let logo = match find (function `FeedLogo _ -> true | _ -> false) l with
    | Some (`FeedLogo l) -> Some l
    | _ -> None
  in
  let rights = match find (function `FeedRights _ -> true | _ -> false) l with
    | Some (`FeedRights r) -> Some r
    | _ -> None
  in
  let subtitle = match find (function `FeedSubtitle _ -> true | _ -> false) l with
    | Some (`FeedSubtitle s) -> Some s
    | _ -> None
  in
  let title = match find (function `FeedTitle _ -> true | _ -> false) l with
    | Some (`FeedTitle t) -> t
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "feed")
  in
  let updated = match find (function `FeedUpdated _ -> true | _ -> false) l with
    | Some (`FeedUpdated u) -> u
    | _ -> Error.raise_expectation (Error.Tag "updated") (Error.Tag "feed")
  in
  let entry = List.fold_left (fun acc -> function `FeedEntry x -> x :: acc | _ -> acc) [] l in
  ({ author; category; contributor; generator; icon; id; link; logo; rights; subtitle; title; updated; entry; } : feed)

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
