open Syndic_common.XML
open Syndic_common.Util
module XML = Syndic_xml
module Error = Syndic_error

module Date = struct
  open CalendarLib
  open Printf
  open Scanf

  (* RFC3339 date *)
  let of_string s =
    let make_date year month day h m s z =
      let date = Calendar.Date.make year month day in
      let t = Calendar.Time.(make h m (Second.from_float s)) in
      if z = "" || z.[0] = 'Z' then
        Calendar.(create date t)
      else
        let tz =
          let open Calendar.Time in
          sscanf z "%i:%i" (fun h m -> Period.make h m (Second.from_int 0)) in
        Calendar.(create date (Time.add t tz))
    in
    (* Sometimes, the seconds have a decimal point
       See https://forge.ocamlcore.org/tracker/index.php?func=detail&aid=1414&group_id=83&atid=418 *)
    try sscanf s "%i-%i-%iT%i:%i:%f%s" make_date
    with Scanf.Scan_failure _ ->
      invalid_arg(sprintf "Syndic.Atom.Date.of_string: cannot parse %S" s)
end

let namespaces = [ "http://www.w3.org/2005/Atom" ]

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

(* The actual XML content is supposed to be inside a <div> which is NOT
   part of the content. *)
(* FIXME: beware for output! Must pust the <div> back (with namespace ?) *)
let rec get_xml_content xml0 = function
  | XML.Data (_, s) :: tl ->
    if only_whitespace s then get_xml_content xml0 tl
    else xml0 (* unexpected *)
  | XML.Node (pos, tag, data) :: tl when tag_is tag "div" ->
     let is_space =
       List.for_all (function XML.Data (_, s) -> only_whitespace s
                            | _ -> false) tl in
     if is_space then data else xml0
  | _ -> xml0

let no_namespace = Some ""
let rm_namespace _ = no_namespace

(* For HTML, the spec says the whole content needs to be escaped
   http://tools.ietf.org/html/rfc4287#section-3.1.1.2 (some feeds use
   <![CDATA[ ]]>) so a single data item should be present.
   If not, assume the HTML was properly parsed and convert it back
   to a string as it should. *)
let get_html_content html =
  match html with
  | [XML.Data (_, d)] -> d
  | h ->
     (* It is likely that, when the HTML was parsed, the Atom
        namespace was applied.  Remove it. *)
     String.concat "" (List.map (XML.to_string ~ns_prefix:rm_namespace) h)

type text_construct =
  | Text of string
  | Html of string
  | Xhtml of Syndic_xml.t list

let text_construct_of_xml
    ((pos, (tag, attr), data) : Xmlm.pos * Xmlm.tag * t list) =
  match find (fun a -> attr_is a "type") attr with
  | Some(_, "html") -> Html(get_html_content data)
  | Some(_, "application/xhtml+xml")
  | Some(_, "xhtml") -> Xhtml(get_xml_content data data)
  | _ -> Text(get_leaf data)


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

let make_author datas (l : [< author'] list) =
  (* element atom:name { text } *)
  let name = match find (function `Name _ -> true | _ -> false) l with
    | Some (`Name s) -> s
    | _ ->
       (* The spec mandates that <author><name>name</name></author>
          but severay feed just do <author>name</author> *)
       get_leaf datas in
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

let author_name_of_xml (pos, tag, datas) =
  try get_leaf datas
  with Not_found -> "" (* mandatory ? *)

let author_uri_of_xml (pos, tag, datas) =
  try Uri.of_string (get_leaf datas)
  with Not_found -> raise (Error.Error (pos,
                            "The content of <uri> MUST be \
                             a non-empty string"))

let author_email_of_xml (pos, tag, datas) =
  try get_leaf datas
  with Not_found -> "" (* mandatory ? *)

(* {[  atomAuthor = element atom:author { atomPersonConstruct } ]}
   where

    atomPersonConstruct =
        atomCommonAttributes,
        (element atom:name { text }
         & element atom:uri { atomUri }?
         & element atom:email { atomEmailAddress }?
         & extensionElement * )

   This specification assigns no significance to the order of
   appearance of the child elements in a Person construct.  *)
let author_of_xml =
  let data_producer = [
    ("name", (fun ctx a -> `Name (author_name_of_xml a)));
    ("uri", (fun ctx a -> `URI (author_uri_of_xml a)));
    ("email", (fun ctx a -> `Email (author_email_of_xml a)));
  ] in
  fun ((_, _, datas) as xml) ->
  generate_catcher ~namespaces ~data_producer (make_author datas) xml

let author_of_xml' =
  let data_producer = [
    ("name", (fun ctx -> dummy_of_xml ~ctor:(fun a -> `Name a)));
    ("uri", (fun ctx -> dummy_of_xml ~ctor:(fun a -> `URI a)));
    ("email", (fun ctx -> dummy_of_xml ~ctor:(fun a -> `Email a)));
  ] in
  generate_catcher ~namespaces ~data_producer (fun x -> x)

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

let make_category ~pos (l : [< category'] list) =
  (* attribute term { text } *)
  let term = match find (function `Term _ -> true | _ -> false) l with
    | Some (`Term t) -> t
    | _ ->
      raise (Error.Error (pos,
                            "Category elements MUST have a 'term' \
                             attribute"))
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


(* atomCategory =
     element atom:category {
        atomCommonAttributes,
        attribute term { text },
        attribute scheme { atomUri }?,
        attribute label { text }?,
        undefinedContent
     }
 *)
let category_of_xml, category_of_xml' =
  let attr_producer = [
    ("term", (fun ctx pos a -> `Term a));
    ("scheme", (fun ctx pos a -> `Scheme a));
    ("label", (fun ctx pos a -> `Label a))
  ] in
  (fun ((pos, _, _) as xml) ->
     generate_catcher ~attr_producer (make_category ~pos) xml),
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

let make_generator ~pos (l : [< generator'] list) =
  (* text *)
  let content = match find (function `Content _ -> true | _ -> false) l with
    | Some ((`Content c)) -> c
    | _ -> raise (Error.Error (pos,
                            "The content of <generator> MUST be \
                             a non-empty string"))
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

(* atomGenerator = element atom:generator {
      atomCommonAttributes,
      attribute uri { atomUri }?,
      attribute version { text }?,
      text
    }
 *)
let generator_of_xml, generator_of_xml' =
  let attr_producer = [
    ("version", (fun ctx pos a -> `Version a));
    ("uri", (fun ctx pos a -> `URI a));
  ] in
  let leaf_producer ctx pos data = `Content data in
  (fun ((pos, _, _) as xml) ->
     generate_catcher ~attr_producer ~leaf_producer (make_generator ~pos) xml),
  generate_catcher ~attr_producer ~leaf_producer (fun x -> x)

type icon = Uri.t
type icon' = [ `URI of string ]

let make_icon ~pos (l : [< icon'] list) =
  (** (atomUri) *)
  let uri = match find (fun (`URI _) -> true) l with
    | Some (`URI u) -> (Uri.of_string u)
    | _ -> raise (Error.Error (pos,
                            "The content of <icon> MUST be \
                             a non-empty string"))
  in uri

(* atomIcon = element atom:icon {
      atomCommonAttributes,
    }
 *)
let icon_of_xml, icon_of_xml' =
  let leaf_producer ctx pos data = `URI data in
  (fun ((pos, _, _) as xml) ->
     generate_catcher ~leaf_producer (make_icon ~pos) xml),
  generate_catcher ~leaf_producer (fun x -> x)

type id = Uri.t
type id' = [ `URI of string ]

let make_id ~pos (l : [< id'] list) =
  (* (atomUri) *)
  let uri = match find (fun (`URI _) -> true) l with
    | Some (`URI u) -> (Uri.of_string u)
    | _ -> raise (Error.Error (pos,
                            "The content of <id> MUST be \
                             a non-empty string"))
  in uri

(* atomId = element atom:id {
      atomCommonAttributes,
      (atomUri)
    }
 *)
let id_of_xml, id_of_xml' =
  let leaf_producer ctx pos data = `URI data in
  (fun ((pos, _, _) as xml) ->
     generate_catcher ~leaf_producer (make_id ~pos) xml),
  generate_catcher ~leaf_producer (fun x -> x)

let rel_of_string s = match String.lowercase (String.trim s) with
  | "alternate" -> Alternate
  | "related" -> Related
  | "self" -> Self
  | "enclosure" -> Enclosure
  | "via" -> Via
  | uri -> Link (Uri.of_string uri) (* RFC 4287 § 4.2.7.2 *)

let make_link ~pos (l : [< link'] list) =
  (* attribute href { atomUri } *)
  let href = match find (function `HREF _ -> true | _ -> false) l with
    | Some (`HREF u) -> (Uri.of_string u)
    | _ ->
      raise (Error.Error (pos,
                            "Link elements MUST have a 'href' \
                             attribute"))
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

(* atomLink =
    element atom:link {
        atomCommonAttributes,
        attribute href { atomUri },
        attribute rel { atomNCName | atomUri }?,
        attribute type { atomMediaType }?,
        attribute hreflang { atomLanguageTag }?,
        attribute title { text }?,
        attribute length { text }?,
        undefinedContent
  }
 *)
let link_of_xml, link_of_xml' =
  let attr_producer = [
    ("href", (fun ctx pos a -> `HREF a));
    ("rel", (fun ctx pos a -> `Rel a));
    ("type", (fun ctx pos a -> `Type a));
    ("hreflang", (fun ctx pos a -> `HREFLang a));
    ("title", (fun ctx pos a -> `Title a));
    ("length", (fun ctx pos a -> `Length a));
  ] in
  (fun ((pos, _, _) as xml) ->
     generate_catcher ~attr_producer (make_link ~pos) xml),
  generate_catcher ~attr_producer (fun x -> x)

type logo = Uri.t
type logo' = [ `URI of string ]

let make_logo ~pos (l : [< logo'] list) =
  (* (atomUri) *)
  let uri = match find (fun (`URI _) -> true) l with
    | Some (`URI u) -> (Uri.of_string u)
    | _ -> raise (Error.Error (pos,
                            "The content of <logo> MUST be \
                             a non-empty string"))
  in uri

(* atomLogo = element atom:logo {
      atomCommonAttributes,
      (atomUri)
    }
 *)
let logo_of_xml, logo_of_xml' =
  let leaf_producer ctx pos data = `URI data in
  (fun ((pos, _, _) as xml) ->
     generate_catcher ~leaf_producer (make_logo ~pos) xml),
  generate_catcher ~leaf_producer (fun x -> x)

type published = CalendarLib.Calendar.t
type published' = [ `Date of string ]

let make_published ~pos (l : [< published'] list) =
  (* atom:published { atomDateConstruct } *)
  let date = match find (fun (`Date _) -> true) l with
    | Some (`Date d) -> Date.of_string d
    | _ -> raise (Error.Error (pos,
                            "The content of <published> MUST be \
                             a non-empty string"))
  in date

(* atomPublished = element atom:published { atomDateConstruct } *)
let published_of_xml, published_of_xml' =
  let leaf_producer ctx pos data = `Date data in
  (fun ((pos, _, _) as xml) ->
    generate_catcher ~leaf_producer (make_published ~pos) xml),
  generate_catcher ~leaf_producer (fun x -> x)

type rights = text_construct
type rights' = [ `Data of Syndic_xml.t list ]

let rights_of_xml = text_construct_of_xml

(* atomRights = element atom:rights { atomTextConstruct } *)
let rights_of_xml' ((pos, (tag, attr), data) : Xmlm.pos * Xmlm.tag * t list) =
  `Data data

type title = text_construct
type title' = [ `Data of Syndic_xml.t list ]

let title_of_xml = text_construct_of_xml

(* atomTitle = element atom:title { atomTextConstruct } *)
let title_of_xml' ((pos, (tag, attr), data) : Xmlm.pos * Xmlm.tag * t list) =
  `Data data

type subtitle = text_construct
type subtitle' = [ `Data of Syndic_xml.t list ]

let subtitle_of_xml = text_construct_of_xml

(* atomSubtitle = element atom:subtitle { atomTextConstruct } *)
let subtitle_of_xml' ((pos, (tag, attr), data) : Xmlm.pos * Xmlm.tag * t list) =
  `Data data

type updated = CalendarLib.Calendar.t
type updated' = [ `Date of string ]

let make_updated ~pos (l : [< updated'] list) =
  (* atom:updated { atomDateConstruct } *)
  let updated = match find (fun (`Date _) -> true) l with
    | Some (`Date d) -> Date.of_string d
    | _ -> raise (Error.Error (pos,
                            "The content of <updated> MUST be \
                             a non-empty string"))
  in updated

(* atomUpdated = element atom:updated { atomDateConstruct } *)
let updated_of_xml, updated_of_xml' =
  let leaf_producer ctx pos data = `Date data in
  (fun ((pos, _, _) as xml) ->
     generate_catcher ~leaf_producer (make_updated ~pos) xml),
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

let make_source ~pos ~entry_authors (l : [< source'] list) =
  (* atomAuthor* *)
  let authors =
    List.fold_left
      (fun acc -> function `Author x -> x :: acc | _ -> acc) [] l in
  let authors = match authors, entry_authors with
    | x :: r, _ -> x, r
    | [], x :: r -> x, r
    | [], [] ->
      raise (Error.Error (pos,
                            "<source> elements MUST contains one or more \
                             <author> elements"))
      (* XXX: no see this rule in RFC *)
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
    | _ -> raise (Error.Error (pos,
                            "<source> elements MUST contains exactly one \
                             <id> elements"))
  in
  (* atomLink* *)
  let links =
    (function
      | [] -> raise (Error.Error (pos,
                            "<source> elements MUST contains one or more \
                             <link> elements"))
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
    | _ -> raise (Error.Error (pos,
                            "<source> elements MUST contains exactly one \
                             <title> elements"))
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

(* atomSource =
    element atom:source {
        atomCommonAttributes,
        (atomAuthor*
         & atomCategory*
         & atomContributor*
         & atomGenerator?
         & atomIcon?
         & atomId?
         & atomLink*
         & atomLogo?
         & atomRights?
         & atomSubtitle?
         & atomTitle?
         & atomUpdated?
         & extensionElement * )
      }
 *)
let source_of_xml ((pos, _, _) as xml)=
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
  fun ~entry_authors ->
  generate_catcher
    ~namespaces
    ~data_producer
    (make_source ~pos ~entry_authors) xml

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
  generate_catcher ~namespaces ~data_producer (fun x -> x)

type mime = string

type content =
  | Text of string
  | Html of string
  | Xhtml of Syndic_xml.t list
  | Mime of mime * string
  | Src of mime option * Uri.t

type content' = [
  | `Type of string
  | `SRC of string
  | `Data of Syndic_xml.t list
]

(*  atomInlineTextContent =
      element atom:content {
          atomCommonAttributes,
          attribute type { "text" | "html" }?,
          (text)*
    }

    atomInlineXHTMLContent =
      element atom:content {
          atomCommonAttributes,
          attribute type { "xhtml" },
          xhtmlDiv
    }

    atomInlineOtherContent =
      element atom:content {
          atomCommonAttributes,
          attribute type { atomMediaType }?,
          (text|anyElement)*
    }

    atomOutOfLineContent =
      element atom:content {
          atomCommonAttributes,
          attribute type { atomMediaType }?,
          attribute src { atomUri },
          empty
    }

    atomContent = atomInlineTextContent
    | atomInlineXHTMLContent
    | atomInlineOtherContent
    | atomOutOfLineContent
 *)
let content_of_xml
    ((pos, (tag, attr), data) : Xmlm.pos * Xmlm.tag * t list) : content =
  (* MIME ::= attribute type { "text" | "html" }?
              | attribute type { "xhtml" }
              | attribute type { atomMediaType }? *)
  (* attribute src { atomUri } | none
     If src s present, [data] MUST be empty. *)
  match find (fun a -> attr_is a "src") attr with
  | Some (_, src) ->
     let mime = match find (fun a -> attr_is a "type") attr with
       | Some(_, ty) -> Some ty
       | None -> None in
     Src(mime, Uri.of_string src)
  | None ->
     (* (text)*
      *  | xhtmlDiv
      *  | (text|anyElement)*
      *  | none *)
     match find (fun a -> attr_is a "type") attr with
     | Some (_, "text") | None -> Text(get_leaf data)
     | Some (_, "html") -> Html(get_html_content data)
     | Some (_, "xhtml") -> Xhtml(get_xml_content data data)
     | Some (_, mime) -> Mime(mime, get_leaf data)

let content_of_xml' ((pos, (tag, attr), data) : Xmlm.pos * Xmlm.tag * t list) =
  let l = match find (fun a -> attr_is a "src") attr with
    | Some(_, src) -> [`SRC src]
    | None -> [] in
  let l = match find (fun a -> attr_is a "type") attr with
    | Some(_, ty) -> `Type ty :: l
    | None -> l in
  `Data data :: l


type summary = text_construct
type summary' = [ `Data of Syndic_xml.t list ]

(* atomSummary = element atom:summary { atomTextConstruct } *)
let summary_of_xml = text_construct_of_xml

let summary_of_xml' ((pos, (tag, attr), data) : Xmlm.pos * Xmlm.tag * t list) =
  `Data data

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

module LinkOrder
  : Set.OrderedType with type t = string * string =
struct
  type t = string * string
  let compare (a : t) (b : t) = match compare (fst a) (fst b) with
    | 0 -> compare (snd a) (snd b)
    | n -> n
end

module LinkSet = Set.Make(LinkOrder)

let uniq_link_alternate ~pos (l : link list) =
  let string_of_duplicate_link
      { href; type_media; hreflang; _ }
      (type_media', hreflang') =
    let ty = (function Some a -> a | None -> "(none)") type_media in
    let hl = (function Some a -> a | None -> "(none)") hreflang in
    let ty' = (function "" -> "(none)" | s -> s) type_media' in
    let hl' = (function "" -> "(none)" | s -> s) hreflang' in
    Printf.sprintf
      "Duplicate link between \
       <link href=\"%s\" hreflang=\"%s\" type=\"%s\" ..> and \
       <link hreflang=\"%s\" type=\"%s\" ..>"
      (Uri.to_string href)
      hl ty hl' ty'
  in
  let raise_error link link' =
    raise (Error.Error (pos,  (string_of_duplicate_link link link')))
  in
  let rec aux acc = function
    | [] -> l

    | ({ rel; type_media = Some ty; hreflang = Some hl; _ } as x) :: r
      when rel = Alternate ->
      if LinkSet.mem (ty, hl) acc
      then raise_error x (LinkSet.find (ty, hl) acc)
      else aux (LinkSet.add (ty, hl) acc) r

    | ({ rel; type_media = None; hreflang = Some hl; _ } as x) :: r
      when rel = Alternate ->
      if LinkSet.mem ("", hl) acc
      then raise_error x (LinkSet.find ("", hl) acc)
      else aux (LinkSet.add ("", hl) acc) r

    | ({ rel; type_media = Some ty; hreflang = None; _ } as x) :: r
      when rel = Alternate ->
      if LinkSet.mem (ty, "") acc
      then raise_error x (LinkSet.find (ty, "") acc)
      else aux (LinkSet.add (ty, "") acc) r

    | ({ rel; type_media = None; hreflang = None; _ } as x) :: r
      when rel = Alternate ->
      if LinkSet.mem ("", "") acc
      then raise_error x (LinkSet.find ("", "") acc)
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


let make_entry ~pos ~(feed_authors: author list) l =
  let authors =
    List.fold_left
      (fun acc -> function `Author x -> x :: acc | _ -> acc) [] l in
  let authors = match authors with
    (* default author is feed/author, see RFC 4287 § 4.1.2 *)
    | [] -> feed_authors
    | _ -> authors in
  (* atomSource? (pass the authors known so far) *)
  let sources = List.fold_left
                  (fun acc -> function `Source x -> x :: acc | _ -> acc) [] l in
  let sources = List.map (source_of_xml ~entry_authors:authors) sources in
  let authors = match authors, sources with
    | a0 :: a, _ -> a0, a
    | [], s :: src ->
       (* Collect authors given in [sources] *)
       let a0, a1 = s.authors in
       let a2 =
         List.map (fun (s: source) -> let a1, a = s.authors in a1 :: a) src in
       a0, List.concat (a1 :: a2)
    | [], [] ->
      raise (Error.Error (pos,
                            "<entry> elements MUST contains one or more \
                             <author> elements or <feed> elements MUST \
                             contains one or more <author> elements"))
  (* atomCategory* *)
  in let categories = List.fold_left
      (fun acc -> function `Category x -> x :: acc | _ -> acc) [] l
      (* atomContributor* *)
  in let contributors = List.fold_left
      (fun acc -> function `Contributor x -> x :: acc | _ -> acc) [] l in
  (* atomId *)
  let id = match find (function `ID _ -> true | _ -> false) l with
    | Some (`ID i) -> i
    | _ -> raise (Error.Error (pos,
                            "<entry> elements MUST contains exactly one \
                             <id> elements"))
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
    | _ -> None in
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
    | _ -> raise (Error.Error (pos,
                            "<entry> elements MUST contains exactly one \
                             <title> elements"))
  in
  (* atomUpdated *)
  let updated = match find (function `Updated _ -> true | _ -> false) l with
    | Some (`Updated u) -> u
    | _ -> raise (Error.Error (pos,
                            "<entry> elements MUST contains exactly one \
                             <updated> elements"))
  in
  ({ authors;
     categories;
     content;
     contributors;
     id;
     links = uniq_link_alternate ~pos links;
     published;
     rights;
     sources;
     summary;
     title;
     updated; } : entry)

(* atomEntry =
     element atom:entry {
        atomCommonAttributes,
        (atomAuthor*
         & atomCategory*
         & atomContent?
         & atomContributor*
         & atomId
         & atomLink*
         & atomPublished?
         & atomRights?
         & atomSource?
         & atomSummary?
         & atomTitle
         & atomUpdated
         & extensionElement * )
      }
 *)
let entry_of_xml ((pos, _, _) as xml)=
  let data_producer = [
    ("author", (fun ctx a -> `Author (author_of_xml a)));
    ("category", (fun ctx a -> `Category (category_of_xml a)));
    ("contributor", (fun ctx a -> `Contributor (contributor_of_xml a)));
    ("id", (fun ctx a -> `ID (id_of_xml a)));
    ("link", (fun ctx a -> `Link (link_of_xml a)));
    ("published", (fun ctx a -> `Published (published_of_xml a)));
    ("rights", (fun ctx a -> `Rights (rights_of_xml a)));
    ("source", (fun ctx a -> `Source a));
    ("content", (fun ctx a -> `Content (content_of_xml a)));
    ("summary", (fun ctx a -> `Summary (summary_of_xml a)));
    ("title", (fun ctx a -> `Title (title_of_xml a)));
    ("updated", (fun ctx a -> `Updated (updated_of_xml a)));
  ] in
  fun ~feed_authors ->
  generate_catcher
    ~namespaces
    ~data_producer
    (make_entry ~pos ~feed_authors) xml

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
  generate_catcher ~namespaces ~data_producer (fun x -> x)

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

let make_feed ~pos (l : _ list) =
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
    | _ -> raise (Error.Error (pos,
                            "<feed> elements MUST contains exactly one \
                             <id> elements"))
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
    | _ -> raise (Error.Error (pos,
                            "<feed> elements MUST contains exactly one \
                             <title> elements"))
  in
  (* atomUpdated *)
  let updated = match find (function `Updated _ -> true | _ -> false) l with
    | Some (`Updated u) -> u
    | _ -> raise (Error.Error (pos,
                            "<feed> elements MUST contains exactly one \
                             <updated> elements"))
  in
  (* atomEntry* *)
  let entries =
    List.fold_left
      (fun acc -> function `Entry x ->
         entry_of_xml ~feed_authors:authors x :: acc
                         | _ -> acc) [] l in
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

(* atomFeed =
     element atom:feed {
        atomCommonAttributes,
        (atomAuthor*
         & atomCategory*
         & atomContributor*
         & atomGenerator?
         & atomIcon?
         & atomId
         & atomLink*
         & atomLogo?
         & atomRights?
         & atomSubtitle?
         & atomTitle
         & atomUpdated
         & extensionElement * ),
        atomEntry*
      }
 *)
let feed_of_xml ((pos, _, _) as xml) =
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
    ("entry", (fun ctx a -> `Entry a));
  ] in
  generate_catcher ~namespaces ~data_producer (make_feed ~pos) xml

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
  generate_catcher ~namespaces ~data_producer (fun x -> x)

let parse input =
  match XML.of_xmlm input |> snd with
  | XML.Node (pos, tag, datas) when tag_is tag "feed" ->
    feed_of_xml (pos, tag, datas)
  | _ -> raise (Error.Error ((0, 0),
                         "document MUST contains exactly one \
                          <feed> element"))
(* FIXME: the spec says that an entry can appear as the top-level element *)

let unsafe input =
  match XML.of_xmlm input |> snd with
  | XML.Node (pos, tag, datas) when tag_is tag "feed" ->
    `Feed (feed_of_xml' (pos, tag, datas))
  | _ -> `Feed []
