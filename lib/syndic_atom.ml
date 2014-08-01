open Syndic_common.XML
open Syndic_common.Util

type author =
  {
    name: string;
    uri: Uri.t option;
    email: string option;
  }

type author' = [
  | `Name of string
  | `URI of Uri.t
  | `Email of string
]

let make_author (l : [< author'] list) =
  (* element atom:name { text } *)
  let name = match find (function `Name _ -> true | _ -> false) l with
    | Some (`Name s) -> s
    | _ -> Syndic_common.Error.raise_expectation
             (Syndic_common.Error.Tag "name")
             (Syndic_common.Error.Tag "author")
  in
  (* element atom:uri { atomUri }? *)
  let uri = match find (function `URI _ -> true | _ -> false) l with
    | Some (`URI u) -> Some u
    | _ -> None
  in
  (* element atom:email { atomEmailAddress }? *)
  let email = match find (function `Email _ -> true | _ -> false) l with
    | Some (`Email e) -> Some e
    | _ -> None
  in
  ({ name; uri; email; } : author)

let author_name_of_xml (tag, datas) =
  try get_leaf datas
  with Syndic_common.Error.Expected_Leaf -> "" (* mandatory ? *)

let author_uri_of_xml (tag, datas) =
  try Uri.of_string (get_leaf datas)
  with Syndic_common.Error.Expected_Leaf ->
    Syndic_common.Error.raise_expectation
      Syndic_common.Error.Data
      (Syndic_common.Error.Tag "author/uri")

let author_email_of_xml (tag, datas) =
  try get_leaf datas
  with Syndic_common.Error.Expected_Leaf -> "" (* mandatory ? *)

let author_of_xml =
  let data_producer = [
    ("name", (fun ctx a -> `Name (author_name_of_xml a)));
    ("uri", (fun ctx a -> `URI (author_uri_of_xml a)));
    ("email", (fun ctx a -> `Email (author_email_of_xml a)));
  ] in
  generate_catcher ~data_producer make_author

let author_of_xml' =
  let data_producer = [
    ("name", (fun ctx -> dummy_of_xml ~ctor:(fun a -> `Name a)));
    ("uri", (fun ctx -> dummy_of_xml ~ctor:(fun a -> `URI a)));
    ("email", (fun ctx -> dummy_of_xml ~ctor:(fun a -> `Email a)));
  ] in
  generate_catcher ~data_producer (fun x -> x)

type category =
  {
    term: string;
    scheme: Uri.t option;
    label: string option;
  }

type category' = [
  | `Term of string
  | `Scheme of string
  | `Label of string
]

let make_category (l : [< category'] list) =
  (* attribute term { text } *)
  let term = match find (function `Term _ -> true | _ -> false) l with
    | Some (`Term t) -> t
    | _ -> Syndic_common.Error.raise_expectation
             (Syndic_common.Error.Attr "term")
             (Syndic_common.Error.Tag "category")
  in
  (* attribute scheme { atomUri }? *)
  let scheme =
    match find (function `Scheme _ -> true | _ -> false) l with
    | Some (`Scheme u) -> Some (Uri.of_string u)
    | _ -> None
  in
  (* attribute label { text }? *)
  let label = match find (function `Label _ -> true | _ -> false) l with
    | Some (`Label l) -> Some l
    | _ -> None
  in
  ({ term; scheme; label; } : category)

let category_of_xml, category_of_xml' =
  let attr_producer = [
    ("term", (fun ctx a -> `Term a));
    ("scheme", (fun ctx a -> `Scheme a));
    ("label", (fun ctx a -> `Label a))
  ] in
  generate_catcher ~attr_producer make_category,
  generate_catcher ~attr_producer (fun x -> x)

let make_contributor = make_author
let contributor_of_xml = author_of_xml
let contributor_of_xml' = author_of_xml'

type generator =
  {
    version: string option;
    uri: Uri.t option;
    content: string;
  }

type generator' = [
  | `URI of string
  | `Version of string
  | `Content of string
]

let make_generator (l : [< generator'] list) =
  (* text *)
  let content = match find (function `Content _ -> true | _ -> false) l with
    | Some ((`Content c)) -> c
    | _ -> Syndic_common.Error.raise_expectation
             Syndic_common.Error.Data
             (Syndic_common.Error.Tag "generator")
  in
  (* attribute version { text }? *)
  let version = match find (function `Version _ -> true | _ -> false) l with
    | Some ((`Version v)) -> Some v
    | _ -> None
  in
  (* attribute uri { atomUri }? *)
  let uri = match find (function `URI _ -> true | _ -> false) l with
    | Some ((`URI u)) -> Some (Uri.of_string u)
    | _ -> None
  in ({ version; uri; content; } : generator)

let generator_of_xml, generator_of_xml' =
  let attr_producer = [
    ("version", (fun ctx a -> `Version a));
    ("uri", (fun ctx a -> `URI a));
  ] in
  let leaf_producer ctx data = `Content data in
  generate_catcher ~attr_producer ~leaf_producer make_generator,
  generate_catcher ~attr_producer ~leaf_producer (fun x -> x)

type icon = Uri.t
type icon' = [ `URI of string ]

let make_icon (l : [< icon'] list) =
  (** (atomUri) *)
  let uri = match find (fun (`URI _) -> true) l with
    | Some (`URI u) -> (Uri.of_string u)
    | _ -> Syndic_common.Error.raise_expectation
             Syndic_common.Error.Data
             (Syndic_common.Error.Tag "icon")
  in uri

let icon_of_xml, icon_of_xml' =
  let leaf_producer ctx data = `URI data in
  generate_catcher ~leaf_producer make_icon,
  generate_catcher ~leaf_producer (fun x -> x)

type id = Uri.t
type id' = [ `URI of string ]

let make_id (l : [< id'] list) =
  (* (atomUri) *)
  let uri = match find (fun (`URI _) -> true) l with
    | Some (`URI u) -> (Uri.of_string u)
    | _ -> Syndic_common.Error.raise_expectation
             Syndic_common.Error.Data
             (Syndic_common.Error.Tag "id")
  in uri

let id_of_xml, id_of_xml' =
  let leaf_producer ctx data = `URI data in
  generate_catcher ~leaf_producer make_id,
  generate_catcher ~leaf_producer (fun x -> x)

type rel =
  | Alternate
  | Related
  | Self
  | Enclosure
  | Via
  | Link of Uri.t

type link =
  {
    href: Uri.t;
    rel: rel;
    type_media: string option;
    hreflang: string option;
    title: string option;
    length: int option;
  }

type link' = [
  | `HREF of string
  | `Rel of string
  | `Type of string
  | `HREFLang of string
  | `Title of string
  | `Length of string
]

let rel_of_string s = match String.lowercase (String.trim s) with
  | "alternate" -> Alternate
  | "related" -> Related
  | "self" -> Self
  | "enclosure" -> Enclosure
  | "via" -> Via
  | uri -> Link (Uri.of_string uri) (* RFC 4287 § 4.2.7.2 *)

let make_link (l : [< link'] list) =
  (* attribute href { atomUri } *)
  let href = match find (function `HREF _ -> true | _ -> false) l with
    | Some (`HREF u) -> (Uri.of_string u)
    | _ -> Syndic_common.Error.raise_expectation
             (Syndic_common.Error.Attr "href")
             (Syndic_common.Error.Tag "link")
  in
  (* attribute rel { atomNCName | atomUri }? *)
  let rel = match find (function `Rel _ -> true | _ -> false) l with
    | Some (`Rel r) -> rel_of_string r
    | _ -> Alternate (* cf. RFC 4287 § 4.2.7.2 *)
  in
  (* attribute type { atomMediaType }? *)
  let type_media = match find (function `Type _ -> true | _ -> false) l with
    | Some (`Type t) -> Some t
    | _ -> None
  in
  (* attribute hreflang { atomLanguageTag }? *)
  let hreflang =
    match find (function `HREFLang _ -> true | _ -> false) l with
    | Some (`HREFLang l) -> Some l
    | _ -> None
  in
  (* attribute title { text }? *)
  let title = match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title s) -> Some s
    | _ -> None
  in
  (* attribute length { text }? *)
  let length = match find (function `Length _ -> true | _ -> false) l with
    | Some (`Length i) -> Some (int_of_string i)
    | _ -> None
  in
  ({ href; rel; type_media; hreflang; title; length; } : link)

let link_of_xml, link_of_xml' =
  let attr_producer = [
    ("href", (fun ctx a -> `HREF a));
    ("rel", (fun ctx a -> `Rel a));
    ("type", (fun ctx a -> `Type a));
    ("hreflang", (fun ctx a -> `HREFLang a));
    ("title", (fun ctx a -> `Title a));
    ("length", (fun ctx a -> `Length a));
  ] in
  generate_catcher ~attr_producer make_link,
  generate_catcher ~attr_producer (fun x -> x)

type logo = Uri.t
type logo' = [ `URI of string ]

let make_logo (l : [< logo'] list) =
  (* (atomUri) *)
  let uri = match find (fun (`URI _) -> true) l with
    | Some (`URI u) -> (Uri.of_string u)
    | _ -> Syndic_common.Error.raise_expectation
             Syndic_common.Error.Data
             (Syndic_common.Error.Tag "logo")
  in uri

let logo_of_xml, logo_of_xml' =
  let leaf_producer ctx data = `URI data in
  generate_catcher ~leaf_producer make_logo,
  generate_catcher ~leaf_producer (fun x -> x)

type published = Netdate.t
type published' = [ `Date of string ]

let make_published (l : [< published'] list) =
  (* atom:published { atomDateConstruct } *)
  let date = match find (fun (`Date _) -> true) l with
    | Some (`Date d) -> Netdate.parse d
    | _ -> Syndic_common.Error.raise_expectation
             Syndic_common.Error.Data
             (Syndic_common.Error.Tag "published")
  in date

let published_of_xml, published_of_xml' =
  let leaf_producer ctx data = `Date data in
  generate_catcher ~leaf_producer make_published,
  generate_catcher ~leaf_producer (fun x -> x)


type rights = string
type rights' = [ `Data of string ]

let make_rights (l : [< rights'] list) =
  (* element atom:rights { atomTextConstruct } *)
  let rights = match find (fun (`Data _) -> true) l with
    | Some (`Data d) -> d
    | _ -> Syndic_common.Error.raise_expectation
             Syndic_common.Error.Data
             (Syndic_common.Error.Tag "rights")
  in rights

let rights_of_xml, rights_of_xml' =
  let leaf_producer ctx data = `Data data in
  generate_catcher ~leaf_producer make_rights,
  generate_catcher ~leaf_producer (fun x -> x)

type title = string
type title' = [ `Data of string ]

let make_title (l : [< title'] list) =
  (* element atom:title { atomTextConstruct } *)
  let title = match find (fun (`Data _) -> true) l with
    | Some (`Data d) -> d
    | _ -> Syndic_common.Error.raise_expectation
             Syndic_common.Error.Data
             (Syndic_common.Error.Tag "title")
  in title

let title_of_xml, title_of_xml' =
  let leaf_producer ctx data = `Data data in
  generate_catcher ~leaf_producer make_title,
  generate_catcher ~leaf_producer (fun x -> x)

type subtitle = string
type subtitle' = [ `Data of string ]

let make_subtitle (l : [< subtitle'] list) =
  let subtitle = match find (fun (`Data _) -> true) l with
    | Some (`Data d) -> d
    | _ -> Syndic_common.Error.raise_expectation
             Syndic_common.Error.Data
             (Syndic_common.Error.Tag "subtitle")
  in subtitle

let subtitle_of_xml, subtitle_of_xml' =
  let leaf_producer ctx data = `Data data in
  generate_catcher ~leaf_producer make_subtitle,
  generate_catcher ~leaf_producer (fun x -> x)

type updated = Netdate.t
type updated' = [ `Date of string ]

let make_updated (l : [< updated'] list) =
  (* atom:updated { atomDateConstruct } *)
  let updated = match find (fun (`Date _) -> true) l with
    | Some (`Date d) -> Netdate.parse d
    | _ -> Syndic_common.Error.raise_expectation
             Syndic_common.Error.Data
             (Syndic_common.Error.Tag "updated")
  in updated

let updated_of_xml, updated_of_xml' =
  let leaf_producer ctx data = `Date data in
  generate_catcher ~leaf_producer make_updated,
  generate_catcher ~leaf_producer (fun x -> x)

type source =
  {
    authors: author * author list;
    categories: category list;
    contributors: author list;
    generator: generator option;
    icon: icon option;
    id: id;
    links: link * link list;
    logo: logo option;
    rights: rights option;
    subtitle: subtitle option;
    title: title;
    updated: updated option;
  }

type source' = [
  | `Author of author
  | `Category of category
  | `Contributor of author
  | `Generator of generator
  | `Icon of icon
  | `ID of id
  | `Link of link
  | `Logo of logo
  | `Subtitle of subtitle
  | `Title of title
  | `Rights of rights
  | `Updated of updated
]

let make_source (l : [< source'] list) =
  (* atomAuthor* *)
  let authors =
    (function
      | [] -> Syndic_common.Error.raise_expectation
                (Syndic_common.Error.Tag "author")
                (Syndic_common.Error.Tag "source")
      | x :: r -> x, r)
      (List.fold_left
         (fun acc -> function `Author x -> x :: acc | _ -> acc)
         [] l)
  in
  (* atomCategory* *)
  let categories =
    List.fold_left
      (fun acc -> function `Category x -> x :: acc | _ -> acc)
      [] l in
  (* atomContributor* *)
  let contributors =
    List.fold_left
      (fun acc -> function `Contributor x -> x :: acc | _ -> acc)
      [] l in
  (* atomGenerator? *)
  let generator =
    match find (function `Generator _ -> true | _ -> false) l with
    | Some (`Generator g) -> Some g
    | _ -> None
  in
  (* atomIcon? *)
  let icon = match find (function `Icon _ -> true | _ -> false) l with
    | Some (`Icon u) -> Some u
    | _ -> None
  in
  (* atomId? *)
  let id = match find (function `ID _ -> true | _ -> false) l with
    | Some (`ID i) -> i
    | _ -> Syndic_common.Error.raise_expectation
             (Syndic_common.Error.Tag "id")
             (Syndic_common.Error.Tag "source")
  in
  (* atomLink* *)
  let links =
    (function
      | [] -> Syndic_common.Error.raise_expectation
                (Syndic_common.Error.Tag "link")
                (Syndic_common.Error.Tag "source")
      | x :: r -> (x, r))
      (List.fold_left (fun acc -> function `Link x -> x :: acc | _ -> acc) [] l)
  in
  (* atomLogo? *)
  let logo = match find (function `Logo _ -> true | _ -> false) l with
    | Some (`Logo u) -> Some u
    | _ -> None
  in
  (* atomRights? *)
  let rights = match find (function `Rights _ -> true | _ -> false) l with
    | Some (`Rights r) -> Some r
    | _ -> None
  in
  (* atomSubtitle? *)
  let subtitle = match find (function `Subtitle _ -> true | _ -> false) l with
    | Some (`Subtitle s) -> Some s
    | _ -> None
  in
  (* atomTitle? *)
  let title = match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title s) -> s
    | _ -> Syndic_common.Error.raise_expectation
             (Syndic_common.Error.Tag "title")
             (Syndic_common.Error.Tag "source")
  in
  (* atomUpdated? *)
  let updated = match find (function `Updated _ -> true | _ -> false) l with
    | Some (`Updated d) -> Some d
    | _ -> None
  in
  ({ authors;
     categories;
     contributors;
     generator;
     icon;
     id;
     links;
     logo;
     rights;
     subtitle;
     title;
     updated; } : source)

let source_of_xml =
  let data_producer = [
    ("author", (fun ctx a -> `Author (author_of_xml a)));
    ("category", (fun ctx a -> `Category (category_of_xml a)));
    ("contributor", (fun ctx a -> `Contributor (contributor_of_xml a)));
    ("generator", (fun ctx a -> `Generator (generator_of_xml a)));
    ("icon", (fun ctx a -> `Icon (icon_of_xml a)));
    ("id", (fun ctx a -> `ID (id_of_xml a)));
    ("link", (fun ctx a -> `Link (link_of_xml a)));
    ("logo", (fun ctx a -> `Logo (logo_of_xml a)));
    ("rights", (fun ctx a -> `Rights (rights_of_xml a)));
    ("subtitle", (fun ctx a -> `Subtitle (subtitle_of_xml a)));
    ("title", (fun ctx a -> `Title (title_of_xml a)));
    ("updated", (fun ctx a -> `Updated (updated_of_xml a)));
  ] in
  generate_catcher ~data_producer make_source

let source_of_xml' =
  let data_producer = [
    ("author", (fun ctx a -> `Author (author_of_xml' a)));
    ("category", (fun ctx a -> `Category (category_of_xml' a)));
    ("contributor", (fun ctx a -> `Contributor (contributor_of_xml' a)));
    ("generator", (fun ctx a -> `Generator (generator_of_xml' a)));
    ("icon", (fun ctx a -> `Icon (icon_of_xml' a)));
    ("id", (fun ctx a -> `ID (id_of_xml' a)));
    ("link", (fun ctx a -> `Link (link_of_xml' a)));
    ("logo", (fun ctx a -> `Logo (logo_of_xml' a)));
    ("rights", (fun ctx a -> `Rights (rights_of_xml' a)));
    ("subtitle", (fun ctx a -> `Subtitle (subtitle_of_xml' a)));
    ("title", (fun ctx a -> `Title (title_of_xml' a)));
    ("updated", (fun ctx a -> `Updated (updated_of_xml' a)));
  ] in
  generate_catcher ~data_producer (fun x -> x)

type type_content =
  | Html
  | Text
  | Xhtml
  | Mime of string

let type_content_of_string s = match String.lowercase (String.trim s) with
  | "html" -> Html
  | "text" -> Text
  | "xhtml" -> Xhtml
  | mime -> Mime mime

type content =
  {
    ty : type_content;
    src : Uri.t option;
    data : string;
  }

type content' = [
  | `Type of string
  | `SRC of string
  | `Data of string
]

(* TODO: see RFC *)

let make_content (l : [< content'] list) =
  (* attribute type { "text" | "html" }?
   *  | attribute type { "xhtml" }
   *  | attribute type { atomMediaType }? *)
  let ty = match find (function `Type _ -> true | _ -> false) l with
    | Some (`Type ty) -> type_content_of_string ty
    | _ -> Text
  in
  (* attribute src { atomUri }
   *  | none *)
  let src = match find (function `SRC _ -> true | _ -> false) l with
    | Some (`SRC s) -> Some (Uri.of_string s)
    | _ -> None
  in
  (* (text)*
   *  | xhtmlDiv
   *  | (text|anyElement)*
   *  | none *)
  let data = match find (function `Data _ -> true | _ -> false) l with
    | Some (`Data d) -> d
    | _ -> ""
  in
  ({ ty; src; data; } : content)

let content_of_xml, content_of_xml' =
  let attr_producer = [
    ("type", (fun ctx a -> `Type a));
    ("src", (fun ctx a -> `SRC a));
  ] in
  let leaf_producer ctx data = `Data data in
  generate_catcher ~attr_producer ~leaf_producer make_content,
  generate_catcher ~attr_producer ~leaf_producer (fun x -> x)

type summary = string
type summary' = [ `Data of string ]

let make_summary (l : [< summary'] list) =
  (* element atom:summaru { atomTextConstruct } *)
  let data = match find (fun (`Data _) -> true) l with
    | Some (`Data d) -> d
    | _ -> Syndic_common.Error.raise_expectation
             Syndic_common.Error.Data
             (Syndic_common.Error.Tag "summary")
  in data

let summary_of_xml, summary_of_xml' =
  let leaf_producer ctx data = `Data data in
  generate_catcher ~leaf_producer make_summary,
  generate_catcher ~leaf_producer (fun x -> x)

type entry =
  {
    authors: author * author list;
    categories: category list;
    content: content option;
    contributors: author list;
    id: id;
    links: link list;
    published: published option;
    rights: rights option;
    sources: source list;
    summary: summary option;
    title: title;
    updated: updated;
  }

type entry' = [
  | `Author of author
  | `Category of category
  | `Contributor of author
  | `ID of id
  | `Link of link
  | `Published of published
  | `Rights of rights
  | `Source of source
  | `Content of content
  | `Summary of summary
  | `Title of title
  | `Updated of updated
]

module Error = struct
  include Syndic_common.Error

  exception Duplicate_Link of ((Uri.t * string * string) * (string * string))

  let raise_duplicate_link
      { href; type_media; hreflang; _}
      (type_media', hreflang') =
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

    | ({ rel; type_media = Some ty; hreflang = Some hl; _ } as x) :: r
      when rel = Alternate ->
      if LinkSet.mem (ty, hl) acc
      then Error.raise_duplicate_link x (LinkSet.find (ty, hl) acc)
      else aux (LinkSet.add (ty, hl) acc) r

    | ({ rel; type_media = None; hreflang = Some hl; _ } as x) :: r
      when rel = Alternate ->
      if LinkSet.mem ("", hl) acc
      then Error.raise_duplicate_link x (LinkSet.find ("", hl) acc)
      else aux (LinkSet.add ("", hl) acc) r

    | ({ rel; type_media = Some ty; hreflang = None; _ } as x) :: r
      when rel = Alternate ->
      if LinkSet.mem (ty, "") acc
      then Error.raise_duplicate_link x (LinkSet.find (ty, "") acc)
      else aux (LinkSet.add (ty, "") acc) r

    | ({ rel; type_media = None; hreflang = None; _ } as x) :: r
      when rel = Alternate ->
      if LinkSet.mem ("", "") acc
      then Error.raise_duplicate_link x (LinkSet.find ("", "") acc)
      else aux (LinkSet.add ("", "") acc) r

    | x :: r -> aux acc r
  in aux LinkSet.empty l

type feed' = [
  | `Author of author
  | `Category of category
  | `Contributor of author
  | `Generator of generator
  | `Icon of icon
  | `ID of id
  | `Link of link
  | `Logo of logo
  | `Rights of rights
  | `Subtitle of subtitle
  | `Title of title
  | `Updated of updated
  | `Entry of entry
]

let make_entry (feed : [< feed'] list) (l : [< entry'] list) =
  let feed_author =
    match find (function `Author _ -> true | _ -> false) feed with
    | Some (`Author a) -> Some a
    | _ -> None
    (* (atomAuthor* *)
  in let authors =
    (* default author is feed/author, see RFC 4287 § 4.1.2 *)
    (function
      | None, [] ->
        Error.raise_expectation
          (Error.Tag "author")
          (Error.Tag "entry")
      | Some a, [] -> a, []
      | _, x :: r -> x, r)
      (feed_author,
       List.fold_left
         (fun acc -> function `Author x -> x :: acc | _ -> acc)
         [] l)
      (* atomCategory* *)
  in let categories = List.fold_left
      (fun acc -> function `Category x -> x :: acc | _ -> acc) [] l
      (* atomContributor* *)
  in let contributors = List.fold_left
      (fun acc -> function `Contributor x -> x :: acc | _ -> acc) [] l in
  (* atomId *)
  let id = match find (function `ID _ -> true | _ -> false) l with
    | Some (`ID i) -> i
    | _ -> Error.raise_expectation (Error.Tag "id") (Error.Tag "entry")
    (* atomLink* *)
  in let links = List.fold_left
      (fun acc -> function `Link x -> x :: acc | _ -> acc) [] l in
  (* atomPublished? *)
  let published = match find (function `Published _ -> true | _ -> false) l with
    | Some (`Published s) -> Some s
    | _ -> None
  in
  (* atomRights? *)
  let rights = match find (function `Rights _ -> true | _ -> false) l with
    | Some (`Rights r) -> Some r
    | _ -> None
    (* atomSource? *)
  in let sources = List.fold_left
      (fun acc -> function `Source x -> x :: acc | _ -> acc) [] l in
  (* atomContent? *)
  let content = match find (function `Content _ -> true | _ -> false) l with
    | Some (`Content c) -> Some c
    | _ -> None
  in
  (* atomSummary? *)
  let summary = match find (function `Summary _ -> true | _ -> false) l with
    | Some (`Summary s) -> Some s
    | _ -> None
  in
  (* atomTitle *)
  let title = match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title t) -> t
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "entry")
  in
  (* atomUpdated *)
  let updated = match find (function `Updated _ -> true | _ -> false) l with
    | Some (`Updated u) -> u
    | _ -> Error.raise_expectation (Error.Tag "updated") (Error.Tag "entry")
  in
  ({ authors;
     categories;
     content;
     contributors;
     id;
     links = uniq_link_alternate links;
     published;
     rights;
     sources;
     summary;
     title;
     updated; } : entry)

let entry_of_xml feed =
  let data_producer = [
    ("author", (fun ctx a -> `Author (author_of_xml a)));
    ("category", (fun ctx a -> `Category (category_of_xml a)));
    ("contributor", (fun ctx a -> `Contributor (contributor_of_xml a)));
    ("id", (fun ctx a -> `ID (id_of_xml a)));
    ("link", (fun ctx a -> `Link (link_of_xml a)));
    ("published", (fun ctx a -> `Published (published_of_xml a)));
    ("rights", (fun ctx a -> `Rights (rights_of_xml a)));
    ("source", (fun ctx a -> `Source (source_of_xml a)));
    ("content", (fun ctx a -> `Content (content_of_xml a)));
    ("summary", (fun ctx a -> `Summary (summary_of_xml a)));
    ("title", (fun ctx a -> `Title (title_of_xml a)));
    ("updated", (fun ctx a -> `Updated (updated_of_xml a)));
  ] in
  generate_catcher ~data_producer (make_entry feed)

let entry_of_xml' =
  let data_producer = [
    ("author", (fun ctx a -> `Author (author_of_xml' a)));
    ("category", (fun ctx a -> `Category (category_of_xml' a)));
    ("contributor", (fun ctx a -> `Contributor (contributor_of_xml' a)));
    ("id", (fun ctx a -> `ID (id_of_xml' a)));
    ("link", (fun ctx a -> `Link (link_of_xml' a)));
    ("published", (fun ctx a -> `Published (published_of_xml' a)));
    ("rights", (fun ctx a -> `Rights (rights_of_xml' a)));
    ("source", (fun ctx a -> `Source (source_of_xml' a)));
    ("content", (fun ctx a -> `Content (content_of_xml' a)));
    ("summary", (fun ctx a -> `Summary (summary_of_xml' a)));
    ("title", (fun ctx a -> `Title (title_of_xml' a)));
    ("updated", (fun ctx a -> `Updated (updated_of_xml' a)));
  ] in
  generate_catcher ~data_producer (fun x -> x)

type feed =
  {
    authors: author list;
    categories: category list;
    contributors: author list;
    generator: generator option;
    icon: icon option;
    id: id;
    links: link list;
    logo: logo option;
    rights: rights option;
    subtitle: subtitle option;
    title: title;
    updated: updated;
    entries: entry list;
  }

let make_feed (l : [< feed'] list) =
  (* atomAuthor* *)
  let authors = List.fold_left
      (fun acc -> function `Author x -> x :: acc | _ -> acc) [] l in
  (* atomCategory* *)
  let categories = List.fold_left
      (fun acc -> function `Category x -> x :: acc | _ -> acc) [] l in
  (* atomContributor* *)
  let contributors = List.fold_left
      (fun acc -> function `Contributor x -> x :: acc | _ -> acc) [] l in
  (* atomLink* *)
  let links = List.fold_left
      (fun acc -> function `Link x -> x :: acc | _ -> acc) [] l in
  (* atomGenerator? *)
  let generator = match find (function `Generator _ -> true | _ -> false) l with
    | Some (`Generator g) -> Some g
    | _ -> None
  in
  (* atomIcon? *)
  let icon = match find (function `Icon _ -> true | _ -> false) l with
    | Some (`Icon i) -> Some i
    | _ -> None
  in
  (* atomId *)
  let id = match find (function `ID _ -> true | _ -> false) l with
    | Some (`ID i) -> i
    | _ -> Error.raise_expectation (Error.Tag "id") (Error.Tag "feed")
  in
  (* atomLogo? *)
  let logo = match find (function `Logo _ -> true | _ -> false) l with
    | Some (`Logo l) -> Some l
    | _ -> None
  in
  (* atomRights? *)
  let rights = match find (function `Rights _ -> true | _ -> false) l with
    | Some (`Rights r) -> Some r
    | _ -> None
  in
  (* atomSubtitle? *)
  let subtitle = match find (function `Subtitle _ -> true | _ -> false) l with
    | Some (`Subtitle s) -> Some s
    | _ -> None
  in
  (* atomTitle *)
  let title = match find (function `Title _ -> true | _ -> false) l with
    | Some (`Title t) -> t
    | _ -> Error.raise_expectation (Error.Tag "title") (Error.Tag "feed")
  in
  (* atomUpdated *)
  let updated = match find (function `Updated _ -> true | _ -> false) l with
    | Some (`Updated u) -> u
    | _ -> Error.raise_expectation (Error.Tag "updated") (Error.Tag "feed")
  in
  (* atomEntry* *)
  let entries = List.fold_left
      (fun acc -> function `Entry x -> x :: acc | _ -> acc) [] l in
  ({ authors;
     categories;
     contributors;
     generator;
     icon;
     id;
     links;
     logo;
     rights;
     subtitle;
     title;
     updated;
     entries; } : feed)

let feed_of_xml =
  let data_producer = [
    ("author", (fun ctx a -> `Author (author_of_xml a)));
    ("category", (fun ctx a -> `Category (category_of_xml a)));
    ("contributor", (fun ctx a -> `Contributor (contributor_of_xml a)));
    ("generator", (fun ctx a -> `Generator (generator_of_xml a)));
    ("icon", (fun ctx a -> `Icon (icon_of_xml a)));
    ("id", (fun ctx a -> `ID (id_of_xml a)));
    ("link", (fun ctx a -> `Link (link_of_xml a)));
    ("logo", (fun ctx a -> `Logo (logo_of_xml a)));
    ("rights", (fun ctx a -> `Rights (rights_of_xml a)));
    ("subtitle", (fun ctx a -> `Subtitle (subtitle_of_xml a)));
    ("title", (fun ctx a -> `Title (title_of_xml a)));
    ("updated", (fun ctx a -> `Updated (updated_of_xml a)));
    ("entry", (fun ctx a -> `Entry (entry_of_xml ctx a)));
  ] in
  generate_catcher ~data_producer make_feed

let feed_of_xml' =
  let data_producer = [
    ("author", (fun ctx a -> `Author (author_of_xml' a)));
    ("category", (fun ctx a -> `Category (category_of_xml' a)));
    ("contributor", (fun ctx a -> `Contributor (contributor_of_xml' a)));
    ("generator", (fun ctx a -> `Generator (generator_of_xml' a)));
    ("icon", (fun ctx a -> `Icon (icon_of_xml' a)));
    ("id", (fun ctx a -> `ID (id_of_xml' a)));
    ("link", (fun ctx a -> `Link (link_of_xml' a)));
    ("logo", (fun ctx a -> `Logo (logo_of_xml' a)));
    ("rights", (fun ctx a -> `Rights (rights_of_xml' a)));
    ("subtitle", (fun ctx a -> `Subtitle (subtitle_of_xml' a)));
    ("title", (fun ctx a -> `Title (title_of_xml' a)));
    ("updated", (fun ctx a -> `Updated (updated_of_xml' a)));
    ("entry", (fun ctx a -> `Entry (entry_of_xml' a)));
  ] in
  generate_catcher ~data_producer (fun x -> x)

let analyze input =
  let el tag datas = Node (tag, datas) in
  let data data = Leaf data in
  let (_, tree) = Xmlm.input_doc_tree ~el ~data input in
  let aux = function
    | Node (tag, datas) when tag_is tag "feed" -> feed_of_xml (tag, datas)
    | _ -> Error.raise_expectation (Error.Tag "feed") Error.Root
  in aux tree

let unsafe input =
  let el tag datas = Node (tag, datas) in
  let data data = Leaf data in
  let (_, tree) = Xmlm.input_doc_tree ~el ~data input in
  let aux = function
    | Node (tag, datas) when tag_is tag "feed" ->
      `Feed (feed_of_xml' (tag, datas))
    | _ -> `Feed []
  in aux tree
