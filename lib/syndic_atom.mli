(** [Syndic.Atom]: {{: http://tools.ietf.org/html/rfc4287} RFC 4287} compliant
    Atom parser. *)

module Error : module type of Syndic_error

(** {2 Structure of Atom document} *)

(** A {{:http://tools.ietf.org/html/rfc4287#section-3.1}text construct}. It
    contains human-readable text, usually in small quantities. The content of
    Text constructs is Language-Sensitive.

    Since the constructors [Text], [Html] or [Xhtml] are shadowed by those of
    the same name in the definition of {!type:content}, you may need a type
    annotation to disambiguate the two. *)
type text_construct =
  | Text of string  (** [Text(content)] *)
  | Html of Uri.t option * string
      (** [Html(xmlbase, content)] where the content is left unparsed. *)
  | Xhtml of Uri.t option * Syndic_xml.t list  (** [Xhtml(xmlbase, content)] *)

(** Describes a person, corporation, or similar entity (hereafter, 'person')
    that indicates the author of the entry or feed. {{:
    http://tools.ietf.org/html/rfc4287#section-3.2} See RFC 4287 § 3.2}.
    Person constructs allow extension Metadata elements (see {{:
    http://tools.ietf.org/html/rfc4287#section-6.4}Section 6.4}).

    They are used for authors
    ({{:http://tools.ietf.org/html/rfc4287#section-4.2.1} See RFC 4287
    § 4.2.1}) and contributors
    ({{:http://tools.ietf.org/html/rfc4287#section-4.2.3} See RFC 4287
    § 4.2.3}) *)
type author = {name: string; uri: Uri.t option; email: string option}

val author : ?uri:Uri.t -> ?email:string -> string -> author

(** The [category] element conveys information about a category associated with
    an entry or feed. This specification assigns no meaning to the content (if
    any) of this element. {{:http://tools.ietf.org/html/rfc4287#section-4.2.2}
    See RFC 4287 § 4.2.2}.

    - [term] is a string that identifies the category to which the entry or
    feed belongs. {{: http://tools.ietf.org/html/rfc4287#section-4.2.2.2} See
    RFC 4287 § 4.2.2.2} - [scheme], if present, is an IRI that identifies a
    categorization scheme. {{:
    http://tools.ietf.org/html/rfc4287#section-4.2.2.3} See RFC 4287 §
    4.2.2.3} - [label], if present, is a human-readable label for display in
    end-user applications. The content of the "label" attribute is
    Language-Sensitive. {{: http://tools.ietf.org/html/rfc4287#section-4.2.2.1}
    See RFC 4287 § 4.2.2.1} *)
type category = {term: string; scheme: Uri.t option; label: string option}

val category : ?scheme:Uri.t -> ?label:string -> string -> category

(** The [generator] element's content identifies the agent used to generate a
    feed, for debugging and other purposes. - [content] is a human-readable
    name for the generating agent. - [uri], if present, SHOULD produce a
    representation that is relevant to that agent. - [version], if present,
    indicates the version of the generating agent.

    See {{: http://tools.ietf.org/html/rfc4287#section-4.2.4}RFC 4287 §
    4.2.4}. *)
type generator = {version: string option; uri: Uri.t option; content: string}

val generator : ?uri:Uri.t -> ?version:string -> string -> generator

(** The [icon] element's content is an IRI reference [RFC3987] that identifies
    an image that provides iconic visual identification for a feed.

    The image SHOULD have an aspect ratio of one (horizontal) to one (vertical)
    and SHOULD be suitable for presentation at a small size.

    {{:http://tools.ietf.org/html/rfc4287#section-4.2.5} See RFC 4287 § 4.2.5} *)
type icon = Uri.t

(** The [id] element conveys a permanent, universally unique identifier for an
    entry or feed.

    Its content MUST be an IRI, as defined by [RFC3987]. Note that the
    definition of "IRI" excludes relative references. Though the IRI might use
    a dereferencable scheme, Atom Processors MUST NOT assume it can be
    dereferenced.

    There is more information in the RFC but they are not necessary here, at
    least, they can not be checked here.

    {{: http://tools.ietf.org/html/rfc4287#section-4.2.6} See RFC 4287 § 4.2.6
    } *)
type id = Uri.t

(** Indicates the link relation type. See {{:
    http://tools.ietf.org/html/rfc4287#section-4.2.7.2} RFC 4287 § 4.2.7.2}. *)
type rel =
  | Alternate
      (** Signifies that the URI in the value of the link [href] field
          identifies an alternate version of the resource described by the
          containing element. *)
  | Related
      (** Signifies that the URI in the value of the link [href] field
          identifies a resource related to the resource described by the
          containing element. *)
  | Self
      (** Signifies that the URI in the value of the link [href] field
          identifies a resource equivalent to the containing element. *)
  | Enclosure
      (** Signifies that the IRI in the value of the link [href] field
          identifies a related resource that is potentially large in size and
          might require special handling. When [Enclosure] is specified, the
          length attribute SHOULD be provided. *)
  | Via
      (** Signifies that the IRI in the value of the link [href] field
          identifies a resource that is the source of the information provided
          in the containing element. *)
  | Link of Uri.t
      (** The URI MUST be non-empty and match either the "isegment-nz-nc" or
          the "IRI" production in {{:http://tools.ietf.org/html/rfc3987}
          RFC3987}. Note that use of a relative reference other than a simple
          name is not allowed. *)

(** [link] defines a reference from an entry or feed to a Web resource. See {{:
    http://tools.ietf.org/html/rfc4287#section-4.2.7} RFC 4287 § 4.2.7}.

    - [href] contains the link's IRI. The value MUST be a IRI reference,
    {{:http://tools.ietf.org/html/rfc3987} RFC3987}. See {{:
    http://tools.ietf.org/html/rfc4287#section-4.2.7.1} RFC 4287 § 4.2.7.1}. -
    [type_media] is an advisory media type: it is a hint about the type of the
    representation that is expected to be returned when the value of the href
    attribute is dereferenced. Note that the type attribute does not override
    the actual media type returned with the representation. The value of
    [type_media], if given, MUST conform to the syntax of a MIME media type,
    {{:http://tools.ietf.org/html/rfc4287#ref-MIMEREG} MIMEREG}. See {{:
    http://tools.ietf.org/html/rfc4287#section-4.2.7.3} RFC 4287 § 4.2.7.3}. -
    [hreflang] describes the language of the resource pointed to by the href
    attribute. When used together with the [rel=Alternate], it implies a
    translated version of the entry. The value of [hreflang] MUST be a language
    tag, {{:http://tools.ietf.org/html/rfc3066} RFC3066}. See {{:
    http://tools.ietf.org/html/rfc4287#section-4.2.7.4} RFC 4287 § 4.2.7.4}. -
    [title] conveys human-readable information about the link. The content of
    the "title" attribute is Language-Sensitive. The value [""] means that no
    title is provided. See {{:
    http://tools.ietf.org/html/rfc4287#section-4.2.7.5} RFC 4287 § 4.2.7.5}. -
    [length] indicates an advisory length of the linked content in octets; it
    is a hint about the content length of the representation returned when the
    IRI in the href attribute is mapped to a URI and dereferenced. Note that
    the length attribute does not override the actual content length of the
    representation as reported by the underlying protocol. See {{:
    http://tools.ietf.org/html/rfc4287#section-4.2.7.6} RFC 4287 § 4.2.7.6}. *)
type link =
  { href: Uri.t
  ; rel: rel
  ; type_media: string option
  ; hreflang: string option
  ; title: string
  ; length: int option }

val link :
     ?type_media:string
  -> ?hreflang:string
  -> ?title:string
  -> ?length:int
  -> ?rel:rel
  -> Uri.t
  -> link
(** [link uri] creates a link element.

    @param rel The [rel] attribute of the link. It defaults to [Alternate]
    since {{:http://tools.ietf.org/html/rfc4287#section-4.2.7.2} RFC 4287 §
    4.2.7.2} says that {i if the "rel" attribute is not present, the link
    element MUST be interpreted as if the link relation type is "alternate".}

    The other optional arguments all default to [None] (i.e., not specified). *)

(** [logo] is an IRI reference [RFC3987] that identifies an image that provides
    visual identification for a feed.

    The image SHOULD have an aspect ratio of 2 (horizontal) to 1 (vertical).

    {{: http://tools.ietf.org/html/rfc4287#section-4.2.8} See RFC 4287 §
    4.2.8} *)
type logo = Uri.t

(** [published] is a Date construct indicating an instant in time associated
    with an event early in the life cycle of the entry.

    Typically, [published] will be associated with the initial creation or
    first availability of the resource.

    {{: http://tools.ietf.org/html/rfc4287#section-4.2.9} See RFC 4287 §
    4.2.9} *)
type published = Syndic_date.t

(** [rights] is a Text construct that conveys information about rights held in
    and over an entry or feed. The [rights] element SHOULD NOT be used to
    convey machine-readable licensing information.

    If an atom:entry element does not contain an atom:rights element, then the
    atom:rights element of the containing atom:feed element, if present, is
    considered to apply to the entry.

    See {{: http://tools.ietf.org/html/rfc4287#section-4.2.10} RFC 4287 §
    4.2.10 } *)
type rights = text_construct

(** [title] is a Text construct that conveys a human-readable title for an
    entry or feed. {{: http://tools.ietf.org/html/rfc4287#section-4.2.14} See
    RFC 4287 § 4.2.14 } *)
type title = text_construct

(** [subtitle] is a Text construct that conveys a human-readable description or
    subtitle for a feed. {{: http://tools.ietf.org/html/rfc4287#section-4.2.12}
    See RFC 4287 § 4.2.12 } *)
type subtitle = text_construct

(** [updated] is a Date construct indicating the most recent instant in time
    when an entry or feed was modified in a way the publisher considers
    significant. Therefore, not all modifications necessarily result in a
    changed [updated] value.

    Publishers MAY change the value of this element over time.

    {{: http://tools.ietf.org/html/rfc4287#section-4.2.15} See RFC 4287 §
    4.2.15 } *)
type updated = Syndic_date.t

(** If an {!entry} is copied from one feed into another feed, then the source
    {!feed}'s metadata (all child elements of atom:feed other than the
    atom:entry elements) MAY be preserved within the copied entry by adding an
    atom:source child element, if it is not already present in the entry, and
    including some or all of the source feed's Metadata elements as the
    atom:source element's children. Such metadata SHOULD be preserved if the
    source atom:feed contains any of the child elements atom:author,
    atom:contributor, atom:rights, or atom:category and those child elements
    are not present in the source atom:entry.

    {{: http://tools.ietf.org/html/rfc4287#section-4.2.11} See RFC 4287 §
    4.2.11 }

    The atom:source element is designed to allow the aggregation of entries
    from different feeds while retaining information about an entry's source
    feed. For this reason, Atom Processors that are performing such aggregation
    SHOULD include at least the required feed-level Metadata fields ([id],
    [title], and [updated]) in the [source] element.

    {{: http://tools.ietf.org/html/rfc4287#section-4.1.2} See RFC 4287 § 4.1.2
    for more details.} *)
type source =
  { authors: author list
  ; categories: category list
  ; contributors: author list
        (** {{: http://tools.ietf.org/html/rfc4287#section-4.2.3} See RFC 4287
            § 4.2.3 } *)
  ; generator: generator option
  ; icon: icon option
  ; id: id
  ; links: link list
  ; logo: logo option
  ; rights: rights option
  ; subtitle: subtitle option
  ; title: title
  ; updated: updated option }

val source :
     ?categories:category list
  -> ?contributors:author list
  -> ?generator:generator
  -> ?icon:icon
  -> ?links:link list
  -> ?logo:logo
  -> ?rights:rights
  -> ?subtitle:subtitle
  -> ?updated:updated
  -> authors:author list
  -> id:id
  -> title
  -> source

(** A MIME type that conform to the syntax of a MIME media type, but MUST NOT
    be a composite type (see Section 4.2.6 of [MIMEREG]).

    {{: http://tools.ietf.org/html/rfc4287#section-4.1.3.1} See RFC 4287 §
    4.1.3.1 } *)
type mime = string

(** [content] either contains or links to the content of the entry. The value
    of [content] is Language-Sensitive. {{:
    http://tools.ietf.org/html/rfc4287#section-4.1.3} See RFC 4287 § 4.1.3}

    - [Text], [Html], [Xhtml] or [Mime] means that the content was part of the
    document and is provided as an argument. The first argument to [Html] and
    [Xhtml] is the possible xml:base value.
    {{:http://tools.ietf.org/html/rfc4287#section-3.1.1} See RFC 4287 § 3.1.1}
    - [Src(m, iri)] means that the content is to be found at [iri] and has MIME
    type [m]. Atom Processors MAY use the IRI to retrieve the content and MAY
    choose to ignore remote content or to present it in a different manner than
    local content. The value of [m] is advisory; that is to say, when the
    corresponding URI (mapped from an IRI, if necessary) is dereferenced, if
    the server providing that content also provides a media type, the
    server-provided media type is authoritative. See {{:
    http://tools.ietf.org/html/rfc4287#section-4.1.3.2} RFC 4287 § 4.1.3.2} *)
type content =
  | Text of string
  | Html of Uri.t option * string
  | Xhtml of Uri.t option * Syndic_xml.t list
  | Mime of mime * string
  | Src of mime option * Uri.t

(** [summary] is a Text construct that conveys a short summary, abstract, or
    excerpt of an entry.

    It is not advisable for [summary] to duplicate {!title} or {!content}
    because Atom Processors might assume there is a useful summary when there
    is none.

    {{: http://tools.ietf.org/html/rfc4287#section-4.2.13} See RFC 4287 §
    4.2.13 } *)
type summary = text_construct

(** [entry] represents an individual entry, acting as a container for metadata
    and data associated with the entry. This element can appear as a child of
    the atom:feed element, or it can appear as the document (i.e., top-level)
    element of a stand-alone Atom Entry Document.

    The specification mandates that each entry contains an author unless it
    contains some sources or the feed contains an author element. This library
    ensures that the authors are properly dispatched to all locations.

    The following child elements are defined by this specification (note that
    it requires the presence of some of these elements):

    - if [content = None], then [links] MUST contain at least one element with
    a rel attribute value of [Alternate]. - There MUST NOT be more than one
    element of [links] with a rel attribute value of [Alternate] that has the
    same combination of type and hreflang attribute values. - There MAY be
    additional elements of [links] beyond those described above. - There MUST
    be an [summary] in either of the following cases: {ul {- the atom:entry
    contains an atom:content that has a "src" attribute (and is thus empty).}
    {- the atom:entry contains content that is encoded in Base64; i.e., the
    "type" attribute of atom:content is a MIME media type [MIMEREG], but is not
    an XML media type [RFC3023], does not begin with "text/", and does not end
    with "/xml" or "+xml".}}

    {{: http://tools.ietf.org/html/rfc4287#section-4.1.2} See RFC 4287 §
    4.1.2} *)
type entry =
  { authors: author * author list
  ; categories: category list
  ; content: content option
  ; contributors: author list
  ; id: id
  ; links: link list
  ; published: published option
  ; rights: rights option
  ; source: source option
  ; summary: summary option
  ; title: title
  ; updated: updated }

val entry :
     ?categories:category list
  -> ?content:content
  -> ?contributors:author list
  -> ?links:link list
  -> ?published:published
  -> ?rights:rights
  -> ?source:source
  -> ?summary:summary
  -> id:id
  -> authors:author * author list
  -> title:title
  -> updated:updated
  -> unit
  -> entry

(** [feed] is the document (i.e., top-level) element of an Atom Feed Document,
    acting as a container for metadata and data associated with the feed. Its
    element children consist of metadata elements followed by zero or more
    atom:entry child elements.

    - one of the [links] SHOULD have a [rel] attribute value of [Self]. This is
    the preferred URI for retrieving Atom Feed Documents representing this Atom
    feed. - There MUST NOT be more than one element of [links] with a rel
    attribute value of [Alternate] that has the same combination of type and
    hreflang attribute values. - There may be additional elements in [links]
    beyond those described above.

    If multiple {!entry} elements with the same {!id} value appear in an Atom
    Feed Document, they represent the same entry. Their {!updated} timestamps
    SHOULD be different. If an Atom Feed Document contains multiple entries
    with the same {!id}, Atom Processors MAY choose to display all of them or
    some subset of them. One typical behavior would be to display only the
    entry with the latest {!updated} timestamp.

    {{: http://tools.ietf.org/html/rfc4287#section-4.1.1} See RFC 4287 §
    4.1.1} *)
type feed =
  { authors: author list
  ; categories: category list
  ; contributors: author list
  ; generator: generator option
  ; icon: icon option
  ; id: id
  ; links: link list
  ; logo: logo option
  ; rights: rights option
  ; subtitle: subtitle option
  ; title: title
  ; updated: updated
  ; entries: entry list }

val feed :
     ?authors:author list
  -> ?categories:category list
  -> ?contributors:author list
  -> ?generator:generator
  -> ?icon:icon
  -> ?links:link list
  -> ?logo:logo
  -> ?rights:rights
  -> ?subtitle:subtitle
  -> id:id
  -> title:title
  -> updated:updated
  -> entry list
  -> feed

(** {2 Input and output} *)

val parse : ?self:Uri.t -> ?xmlbase:Uri.t -> Xmlm.input -> feed
(** [parse xml] returns the feed corresponding to [xml]. Beware that [xml] is
    mutable, so when the parsing fails, one has to create a new copy of [xml]
    to use it with another function. If you retrieve [xml] from a URL, you
    should use that URL as [~xmlbase].

    Raise [Error.Expected], [Expected_Data] or [Error.Duplicate_Link] if [xml]
    is not a valid Atom document.

    @param xmlbase default xml:base to resolve relative URLs (of course
    xml:base attributes in the XML Atom document take precedence over this).
    See {{:http://www.w3.org/TR/xmlbase/}XML Base}.

    @param self the URI from where the current feed was retrieved. Giving this
    information will add an entry to [links] with [rel = Self] unless one
    already exists. *)

val read : ?self:Uri.t -> ?xmlbase:Uri.t -> string -> feed
(** [read fname] reads the file name [fname] and parses it. For the optional
    parameters, see {!parse}. *)

val to_xml : feed -> Syndic_xml.t
(** [to_xml f] converts the feed [f] to an XML tree. *)

val output : feed -> Xmlm.dest -> unit
(** [output f dest] writes the XML tree of the feed [f] to [dest]. *)

val write : feed -> string -> unit
(** [write f fname] writes the XML tree of the feed [f] to the file named
    [fname]. *)

(** {2 Convenience functions} *)

val ascending : entry -> entry -> int
(** Compare entries so that older dates are smaller. The date of the entry is
    taken from the [published] field, if available, or otherwise [updated] is
    used. *)

val descending : entry -> entry -> int
(** Compare entries so that more recent dates are smaller. The date of the
    entry is taken from the [published] field, if available, or otherwise
    [updated] is used. *)

val aggregate :
     ?self:Uri.t
  -> ?id:id
  -> ?updated:updated
  -> ?subtitle:subtitle
  -> ?title:text_construct
  -> ?sort:[`Newest_first | `Oldest_first | `None]
  -> ?n:int
  -> feed list
  -> feed
(** [aggregate feeds] returns a single feed containing all the posts in
    [feeds]. In order to track the origin of each post in the aggrated feed, it
    is recommended that each feed in [feeds] possesses a link with
    [rel = Self] so that the [source] added to each entry contains a link to
    the original feed. If an entry contains a [source], il will {i not} be
    overwritten.

    @param self The preferred URI for retrieving this aggregayed Atom Feed.
    While not mandatory, it is good practice to set this.

    @param id the universally unique identifier for the aggregated feed. If it
    is not provided a URN is built from the [feeds] IDs. @param sort whether to
    sort the entries of the final feed. The default is [`Newest_first] because
    it is generally desired. @param n number of entries of the (sorted)
    aggregated feed to return. *)

val set_self_link : feed -> ?hreflang:string -> ?length:int -> Uri.t -> feed
(** [set_self feed url] add or replace the URI in the self link of the feed.
    You can also set the [hreflang] and [length] of the self link. *)

val get_self_link : feed -> link option
(** [get_self feed] return the self link of the feed, if any is present. *)

val set_main_author : feed -> author -> feed
(** [set_main_author feed author] will add [author] in front of the list of
    authors of the [feed] (if an author with the same name already exists, the
    optional information are merged, the ones in [author] taking precedence).
    Also remove all empty authors (name = "" and no URI, no email) and replace
    them with [author] if no author is left and an authors is mandatory. *)

(**/**)

(** An URI is given by (xmlbase, uri). The value of [xmlbase], if not [None],
    gives the base URI against which [uri] must be resolved if it is relative. *)
type uri = Uri.t option * string

type person = [`Email of string | `Name of string | `URI of uri] list

val unsafe :
     ?xmlbase:Uri.t
  -> Xmlm.input
  -> [> `Feed of [> `Author of person
                 | `Category of [> `Label of string
                                | `Scheme of string
                                | `Term of string ]
                                list
                 | `Contributor of person
                 | `Entry of [> `Author of person
                             | `Category of [> `Label of string
                                            | `Scheme of string
                                            | `Term of string ]
                                            list
                             | `Content of [> `Data of Syndic_xml.t list
                                           | `SRC of string
                                           | `Type of string ]
                                           list
                             | `Contributor of person
                             | `ID of string list
                             | `Link of [> `HREF of string
                                        | `HREFLang of string
                                        | `Length of string
                                        | `Rel of string
                                        | `Title of string
                                        | `Type of string ]
                                        list
                             | `Published of [> `Date of string] list
                             | `Rights of Syndic_xml.t list
                             | `Source of [> `Author of person
                                          | `Category of [> `Label of string
                                                         | `Scheme of string
                                                         | `Term of string ]
                                                         list
                                          | `Contributor of person
                                          | `Generator of [> `Content of string
                                                          | `URI of uri
                                                          | `Version of string
                                                          ]
                                                          list
                                          | `ID of string list
                                          | `Icon of [> `URI of uri] list
                                          | `Link of [> `HREF of string
                                                     | `HREFLang of string
                                                     | `Length of string
                                                     | `Rel of string
                                                     | `Title of string
                                                     | `Type of string ]
                                                     list
                                          | `Logo of [> `URI of uri] list
                                          | `Rights of Syndic_xml.t list
                                          | `Subtitle of Syndic_xml.t list
                                          | `Title of Syndic_xml.t list
                                          | `Updated of [> `Date of string]
                                                        list ]
                                          list
                             | `Summary of Syndic_xml.t list
                             | `Title of Syndic_xml.t list
                             | `Updated of [> `Date of string] list ]
                             list
                 | `Generator of [> `Content of string
                                 | `URI of uri
                                 | `Version of string ]
                                 list
                 | `ID of string list
                 | `Icon of [> `URI of uri] list
                 | `Link of [> `HREF of string
                            | `HREFLang of string
                            | `Length of string
                            | `Rel of string
                            | `Title of string
                            | `Type of string ]
                            list
                 | `Logo of [> `URI of uri] list
                 | `Rights of Syndic_xml.t list
                 | `Subtitle of Syndic_xml.t list
                 | `Title of Syndic_xml.t list
                 | `Updated of [> `Date of string] list ]
                 list ]
(** Analysis without verification, enjoy ! *)
