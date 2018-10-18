(** [Syndic.Rss1]: compliant with {{: http://web.resource.org/rss/1.0/spec} RSS
    1.0}. *)

module Error : module type of Syndic_error

(** A descriptive title for the channel, image, item and textinput. See RSS 1.0
    {{: http://web.resource.org/rss/1.0/spec#s5.3.1} § 5.3.1}, {{:
    http://web.resource.org/rss/1.0/spec#s5.4.1} § 5.4.1}, {{:
    http://web.resource.org/rss/1.0/spec#s5.5.1} § 5.5.1}, and {{:
    http://web.resource.org/rss/1.0/spec#s5.6.1} § 5.6.1}.

    {[ Syntax: <title>{title}</title> Requirement: Required for all Model:
    (#PCDATA) (Suggested) Maximum Length: 40 (characters) for channel, image
    and textinput (Suggested) Maximum Length: 100 for item ]} *)
type title = string

(** The text input field's (variable) name. {{:
    http://web.resource.org/rss/1.0/spec#s5.6.3} See RSS 1.0 § 5.6.3}.

    {[ Syntax: <name>{textinput_varname}</name> Requirement: Required if
    textinput Model: (#PCDATA) (Suggested) Maximum Length: 500 ]} *)
type name = string

(** This can be - a brief description of the channel's content, function,
    source, etc. {{: http://web.resource.org/rss/1.0/spec#s5.3.3} See RSS 1.0
    § 5.3.3}; - or a brief description/abstract of the item. {{:
    http://web.resource.org/rss/1.0/spec#s5.5.3} See RSS 1.0 § 5.5.3}; - or a
    brief description of the textinput field's purpose. For example: "Subscribe
    to our newsletter for..." or "Search our site's archive of..." {{:
    http://web.resource.org/rss/1.0/spec#s5.6.2} See RSS 1.0 § 5.6.2}.

    {[ Syntax: <description>{description}</description> Requirement: Required
    only for channel and textinput Model: (#PCDATA) (Suggested) Maximum Length:
    500 for channel and item (Suggested) Maximum Length: 100 for textinput ]} *)
type description = string

(** Establishes an RDF association between the optional image element [5.4] and
    this particular RSS channel. The rdf:resource's {image_uri} must be the
    same as the image element's rdf:about {image_uri}. {{:
    http://web.resource.org/rss/1.0/spec#s5.3.4} See RSS 1.0 § 5.3.4}

    {[ Syntax: <image rdf:resource="{image_uri}" /> Requirement: Required only
    if image element present Model: Empty ]} *)
type channel_image = Uri.t

(** The URL of the image to used in the "src" attribute of the channel's image
    tag when rendered as HTML. {{: http://web.resource.org/rss/1.0/spec#s5.4.2}
    See RSS 1.0 § 5.4.2}

    {[ Syntax: <url>{image_url}</url> Requirement: Required if the image
    element is present Model: (#PCDATA) (Suggested) Maximum Length: 500 ]} *)
type url = Uri.t

(** This can be - The URL to which an HTML rendering of the channel title will
    link, commonly the parent site's home or news page. {{:
    http://web.resource.org/rss/1.0/spec#s5.3.2} See RSS 1.0 § 5.3.2} - Or the
    URL to which an HTML rendering of the channel image will link. This, as
    with the channel's title link, is commonly the parent site's home or news
    page. {{: http://web.resource.org/rss/1.0/spec#s5.4.3} See RSS 1.0 §
    5.4.3} - Or the item's URL. {{:
    http://web.resource.org/rss/1.0/spec#s5.5.2} See RSS 1.0 § 5.5.2} - Or the
    URL to which a textinput submission will be directed (using GET). {{:
    http://web.resource.org/rss/1.0/spec#s5.6.4} See RSS 1.0 § 5.6.4}

    {[ Syntax: <link>{link}</link> Requirement: Required for all Model:
    (#PCDATA) (Suggested) Maximum Length: 500 ]} *)
type link = Uri.t

(** An RDF table of contents, associating the document's items [5.5] with this
    particular RSS channel. Each item's rdf:resource {item_uri} must be the
    same as the associated item element's rdf:about {item_uri}.

    An RDF Seq (sequence) is used to contain all the items rather than an RDF
    Bag to denote item order for rendering and reconstruction.

    Note that items appearing in the document but not as members of the channel
    level items sequence are likely to be discarded by RDF parsers.

    {{: http://web.resource.org/rss/1.0/spec#s5.3.5} See RSS 1.0 § 5.3.5}

    {[ Syntax: <items><rdf:Seq><rdf:li resource="{item_uri}" /> ...
    </rdf:Seq></items> Requirement: Required ]} *)
type items = Uri.t list

(** Establishes an RDF association between the optional textinput element [5.6]
    and this particular RSS channel. The {textinput_uri} rdf:resource must be
    the same as the textinput element's rdf:about {textinput_uri}.

    {{: http://web.resource.org/rss/1.0/spec#s5.3.6} See RSS 1.0 § 5.3.6}

    {[ Syntax: <textinput rdf:resource="{textinput_uri}" /> Requirement:
    Required only if texinput element present Model: Empty ]} *)
type channel_textinput = Uri.t

(** The channel element contains metadata describing the channel itself,
    including a title, brief description, and URL link to the described
    resource (the channel provider's home page, for instance). The \{resource\}
    URL of the channel element's rdf:about attribute must be unique with
    respect to any other rdf:about attributes in the RSS document and is a URI
    which identifies the channel. Most commonly, this is either the URL of the
    homepage being described or a URL where the RSS file can be found.

    {{: http://web.resource.org/rss/1.0/spec#s5.3} See RSS 1.0 § 5.3}

    {[ Syntax: <channel rdf:about="{resource}"> Requirement: Required Required
    Attribute(s): rdf:about Model: (title, link, description, image?, items,
    textinput?) ]} *)
type channel =
  { about: Uri.t  (** must be unique *)
  ; title: title
  ; link: link
  ; description: description
  ; image: channel_image option
  ; items: items
  ; textinput: channel_textinput option }

(** An image to be associated with an HTML rendering of the channel. This image
    should be of a format supported by the majority of Web browsers. While the
    later 0.91 specification allowed for a width of 1–144 and height of
    1–400, convention (and the 0.9 specification) dictate 88×31.

    {{: http://web.resource.org/rss/1.0/spec#s5.4} See RSS 1.0 § 5.4}

    {[ Syntax: <image rdf:about="{image_uri}"> Requirement: Optional; if
    present, must also be present in channel element [5.3.4] Required
    Attribute(s): rdf:about Model: (title, url, link) ]} *)
type image = {about: Uri.t; title: title; url: url; link: link}

(** While commonly a news headline, with RSS 1.0's modular extensibility, this
    can be just about anything: discussion posting, job listing, software patch
    -- any object with a URI. There may be a minimum of one item per RSS
    document. While RSS 1.0 does not enforce an upper limit, for backward
    compatibility with RSS 0.9 and 0.91, a maximum of fifteen items is
    recommended.

    [about] must be unique with respect to any other rdf:about attributes in
    the RSS document and is a URI which identifies the item. The value of
    [about] should be identical to the value of the [link], if possible.

    {{: http://web.resource.org/rss/1.0/spec#s5.5} See RSS 1.0 § 5.5}

    {[ Syntax: <item rdf:about="{item_uri}"> Requirement: >= 1 Recommendation
    (for backward compatibility with 0.9x): 1-15 Required Attribute(s):
    rdf:about Model: (title, link, description?) ]} *)
type item =
  {about: Uri.t; title: title; link: link; description: description option}

(** The textinput element affords a method for submitting form data to an
    arbitrary URL — usually located at the parent website. The form processor
    at the receiving end only is assumed to handle the HTTP GET method.

    The field is typically used as a search box or subscription form — among
    others. While this is of some use when RSS documents are rendered as
    channels (see MNN) and accompanied by human readable title and description,
    the ambiguity in automatic determination of meaning of this overloaded
    element renders it otherwise not particularly useful. RSS 1.0 therefore
    suggests either deprecation or augmentation with some form of resource
    discovery of this element in future versions while maintaining it for
    backward compatiblity with RSS 0.9.

    [about] must be unique with respect to any other rdf:about attributes in
    the RSS document and is a URI which identifies the textinput. [about]
    should be identical to the value of the [link], if possible.

    {{: http://web.resource.org/rss/1.0/spec#s5.6} See RSS 1.0 § 5.6 }

    {[ Syntax: <textinput rdf:about="{textinput_uri}"> Requirement: Optional;
    if present, must also be present in channel element [5.3.6] Required
    Attribute(s): rdf:about Model: (title, description, name, link) ]} *)
type textinput =
  {about: Uri.t; title: title; description: description; name: name; link: link}

(** The outermost level in every RSS 1.0 compliant document is the RDF element.
    The opening RDF tag assocaties the rdf: namespace prefix with the RDF
    syntax schema and establishes the RSS 1.0 schema as the default namespace
    for the document.

    While any valid namespace prefix may be used, document creators are advised
    to consider "rdf:" normative. Those wishing to be strictly
    backward-compatible with RSS 0.9 must use "rdf:".

    {{: http://web.resource.org/rss/1.0/spec#s5.2} See RSS 1.0 § 5.2}

    {[ Syntax: <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns="http://purl.org/rss/1.0/"> Requirement: Required exactly as shown,
    aside from any additional namespace declarations Model: (channel, image?,
    item+, textinput?) ]} *)
type rdf =
  { channel: channel
  ; image: image option
  ; item: item list
  ; textinput: textinput option }

val parse : ?xmlbase:Uri.t -> Xmlm.input -> rdf
(** [parse xml] returns the RDF corresponding to [xml].

    @raise Error.raise_expectation if [xml] is not a valid RSS1 document.

    @param xmlbase the base URI against which relative URIs in the XML RSS1
    document are resolved. It is superseded by xml:base present in the document
    (if any). *)

val read : ?xmlbase:Uri.t -> string -> rdf
(** [read fname] reads the file name [fname] and parses it. For the optional
    parameters, see {!parse}. *)

(**/**)

(** An URI is given by (xmlbase, uri). The value of [xmlbase], if not [None],
    gives the base URI against which [uri] must be resolved if it is relative. *)
type uri = Uri.t option * string

val unsafe :
     ?xmlbase:Uri.t
  -> Xmlm.input
  -> [> `RDF of [> `Channel of [> `About of uri
                               | `Description of string list
                               | `Image of [> `URI of uri] list
                               | `Items of [> `Seq of [> `Li of [> `URI of uri]
                                                                list ]
                                                      list ]
                                           list
                               | `Link of [> `URI of uri] list
                               | `TextInput of [> `URI of uri] list
                               | `Title of string list ]
                               list
                | `Image of [> `About of uri
                            | `Link of [> `URI of uri] list
                            | `Title of string list
                            | `URL of [> `URI of uri] list ]
                            list
                | `Item of [> `About of uri
                           | `Description of string list
                           | `Link of [> `URI of uri] list
                           | `Title of string list ]
                           list
                | `TextInput of [> `About of uri
                                | `Description of string list
                                | `Link of [> `URI of uri] list
                                | `Name of string list
                                | `Title of string list ]
                                list ]
                list ]
(** Analysis without verification, enjoy ! *)
