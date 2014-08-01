(** [Syndic.Atom]: {{: http://tools.ietf.org/html/rfc4287} RFC 4287}
    compliant. *)

module Error : sig
  include Syndic_error.T

  exception Duplicate_Link of ((Uri.t * string * string) * (string * string))

  val string_of_duplicate_exception :
    (Uri.t * string * string) * (string * string) -> string
end

(**

{{: http://tools.ietf.org/html/rfc4287#section-3.2} See RFC 4287 § 3.2}

A Person construct  is  an  element  that  describes a person,  corporation,  or
similar entity (hereafter, 'person').

{[
  atomPersonConstruct =
    atomCommonAttributes,
    (element atom:name { text }
     & element atom:uri { atomUri }?
     & element atom:email { atomEmailAddress }?
     & extensionElement * )
]}

This specification  assigns no significance to  the order  of appearance  of the
child elements in a Person construct. Person constructs allow extension Metadata
elements (see Section 6.4).

{{: http://tools.ietf.org/html/rfc4287#section-4.2.1} See RFC 4287 § 4.2.1}

The "atom:author" element is a Person construct that indicates the author of the
entry or feed.

{[ atomAuthor = element atom:author { atomPersonConstruct } ]}

If  an  atom:entry element  does  not  contain  atom:author  elements,  then the
atom:author elements of  the  contained  atom:source  element  are considered to
apply.  In an  Atom Feed Document,  the  atom:author elements of  the containing
atom:feed  element  are  considered  to apply  to  the  entry  if  there  are no
atom:author elements in the locations described above.

*)
type author =
  {
    name : string;
    uri : Uri.t option;
    email : string option;
  }

(**

{{: http://tools.ietf.org/html/rfc4287#section-4.2.2} See RFC 4287 § 4.2.2 }

The "atom:category" element conveys information about a category associated with
an entry or feed.  This specification assigns no meaning to the content (if any)
of this element.

{[
  atomCategory =
    element atom:category {
        atomCommonAttributes,
        attribute term { text },
        attribute scheme { atomUri }?,
        attribute label { text }?,
        undefinedContent
      }
]}

{{: http://tools.ietf.org/html/rfc4287#section-4.2.2.1} See RFC 4287 § 4.2.2.1 }

The "term" attribute is a string that identifies the category to which the entry
or feed belongs. Category elements MUST have a "term" attribute.

{{: http://tools.ietf.org/html/rfc4287#section-4.2.2.2} See RFC 4287 § 4.2.2.2 }

The  "scheme" attribute  is an  IRI  that  identifies  a  categorization scheme.
Category elements MAY have a "scheme" attribute.

{{: http://tools.ietf.org/html/rfc4287#section-4.2.2.3} See RFC 4287 § 4.2.2.3 }

The "label"  attribute provides a  human-readable label for  display in end-user
applications.  The  content  of  the  "label"  attribute  is Language-Sensitive.
Entities such  as "&amp;"  and "&lt;"  represent their  corresponding characters
("&" and "<",  respectively),  not markup.  Category elements MAY have a "label"
attribute.

*)
type category =
  {
    term : string;
    scheme : Uri.t option;
    label : string option;
  }

(**

{{: http://tools.ietf.org/html/rfc4287#section-4.2.4} See RFC 4287 § 4.2.4 }

The "atom:generator" element's  content identifies the agent used  to generate a
feed, for debugging and other purposes.

{[
  atomGenerator = element atom:generator {
      atomCommonAttributes,
      attribute uri { atomUri }?,
      attribute version { text }?,
      text
    }
]}

The  content  of  this element,  when  present,  MUST  be  a  string  that  is a
human-readable name for  the  generating  agent.  Entities  such  as "&amp;" and
"&lt;" represent their corresponding characters ("&" and "<" respectively),  not
markup.

The atom:generator element MAY have a "uri" attribute whose value MUST be an IRI
reference [RFC3987].  When dereferenced,  the resulting URI (mapped from an IRI,
if necessary) SHOULD produce a representation that is relevant to that agent.

The atom:generator  element MAY  have a "version"  attribute that  indicates the
version of the generating agent.

*)
type generator =
  {
    version : string option;
    uri : Uri.t option;
    content : string;
  }

(**

{{: http://tools.ietf.org/html/rfc4287#section-4.2.5} See RFC 4287 § 4.2.5 }

The "atom:icon" element's content is  an IRI reference [RFC3987] that identifies
an image that provides iconic visual identification for a feed.

{[
  atomIcon = element atom:icon {
      atomCommonAttributes,
    }
]}

The image SHOULD have an aspect ratio  of one (horizontal) to one (vertical) and
SHOULD be suitable for presentation at a small size.

*)
type icon = Uri.t

(**

{{: http://tools.ietf.org/html/rfc4287#section-4.2.6} See RFC 4287 § 4.2.6 }

The "atom:id" element conveys a permanent,  universally unique identifier for an
entry or feed.

{[
  atomId = element atom:id {
      atomCommonAttributes,
      (atomUri)
    }
]}

Its content MUST be an IRI, as defined by [RFC3987]. Note that the definition of
"IRI" excludes relative  references.  Though the IRI might  use a dereferencable
scheme, Atom Processors MUST NOT assume it can be dereferenced.

There is more information in the RFC but they are not necessary here,  at least,
they can not be checked here.

*)
type id = Uri.t

(**

{{: http://tools.ietf.org/html/rfc4287#section-4.2.7.2} See RFC 4287 § 4.2.7.2 }

atom:link elements MAY  have a "rel" attribute that  indicates the link relation
type.  {b If the  "rel"  attribute  is  not  present,  the  link element MUST be
interpreted as if the link relation type is "alternate".}

{b The value of "rel" MUST be a  string that is non-empty and matches either the
"isegment-nz-nc" or the  "IRI"  production  in  [RFC3987].}  Note  that use of a
relative reference other than a simple name is not allowed.

*)
type rel =
  | Alternate
  | Related
  | Self
  | Enclosure
  | Via
  | Link of Uri.t

(**

{{: http://tools.ietf.org/html/rfc4287#section-4.2.7} See RFC 4287 § 4.2.7 }

The "atom:link"  element defines  a reference  from an  entry or  feed to  a Web
resource.  This specification assigns no meaning to the content (if any) of this
element.

{[
  atomLink =
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
]}

{{: http://tools.ietf.org/html/rfc4287#section-4.2.7.1} See RFC 4287 § 4.2.7.1 }

The "href" attribute  contains the link's IRI.  atom:link elements  MUST have an
href attribute, whose value MUST be a IRI reference [RFC3987].

{{: http://tools.ietf.org/html/rfc4287#section-4.2.7.3} See RFC 4287 § 4.2.7.3 }

On the link element,  the "type" attribute's value is an advisory media type: it
is a hint about  the type of the representation that is  expected to be returned
when the value  of  the  href  attribute  is  dereferenced.  Note  that the type
attribute  does   not  override  the   actual  media  type   returned  with  the
representation.  Link elements  MAY  have  a  type  attribute,  whose value MUST
conform to the syntax of a MIME media type [MIMEREG].

{{: http://tools.ietf.org/html/rfc4287#section-4.2.7.4} See RFC 4287 § 4.2.7.4 }

The  "hreflang"  attribute's content  describes  the  language  of  the resource
pointed to by the href  attribute.  When used together with the rel="alternate",
it implies a translated version of the entry. Link elements MAY have an hreflang
attribute, whose value MUST be a language tag [RFC3066].

{{: http://tools.ietf.org/html/rfc4287#section-4.2.7.5} See RFC 4287 § 4.2.7.5 }

The "title"  attribute conveys human-readable  information about  the link.  The
content of the "title" attribute is Language-Sensitive. Entities such as "&amp;"
and "&lt;" represent their corresponding characters ("&" and "<", respectively),
not markup. Link elements MAY have a title attribute.

{{: http://tools.ietf.org/html/rfc4287#section-4.2.7.6} See RFC 4287 § 4.2.7.6 }

The "length"  attribute indicates an advisory  length of  the linked  content in
octets;  it is  a hint about the  content length of  the representation returned
when the IRI  in the href attribute  is mapped to a  URI and dereferenced.  Note
that the  length attribute does  not override the  actual content length  of the
representation as reported by the underlying protocol.  Link elements MAY have a
length attribute.

*)
type link =
  {
    href : Uri.t;
    rel : rel;
    type_media : string option;
    hreflang : string option;
    title : string option;
    length : int option;
  }

(**

{{: http://tools.ietf.org/html/rfc4287#section-4.2.8} See RFC 4287 § 4.2.8 }

The "atom:logo" element's content is  an IRI reference [RFC3987] that identifies
an image that provides visual identification for a feed.

{[
  atomLogo = element atom:logo {
      atomCommonAttributes,
      (atomUri)
    }
]}

The image SHOULD have an aspect ratio of 2 (horizontal) to 1 (vertical).

*)
type logo = Uri.t

(**

{{: http://tools.ietf.org/html/rfc4287#section-4.2.9} See RFC 4287 § 4.2.9 }

The "atom:published" element  is a Date construct indicating  an instant in time
associated with an event early in the life cycle of the entry.

{[ atomPublished = element atom:published { atomDateConstruct } ]}

Typically,  atom:published will be associated with the initial creation or first
availability of the resource.

*)
type published = CalendarLib.Calendar.t

(**

{{: http://tools.ietf.org/html/rfc4287#section-4.2.10} See RFC 4287 § 4.2.10 }

The "atom:rights"  element is  a Text construct  that conveys  information about
rights held in and over an entry or feed.

{[ atomRights = element atom:rights { atomTextConstruct } ]}

The atom:rights element SHOULD NOT  be used to convey machine-readable licensing
information.

If  an atom:entry  element does  not contain  an atom:rights  element,  then the
atom:rights  element  of  the  containing  atom:feed  element,  if  present,  is
considered to apply to the entry.

*)
type rights = string

(**

{{: http://tools.ietf.org/html/rfc4287#section-4.2.14} See RFC 4287 § 4.2.14 }

The "atom:title"  element is  a Text  construct that  conveys a  human- readable
title for an entry or feed.

{[ atomTitle = element atom:title { atomTextConstruct } ]}

*)
type title = string

(**

{{: http://tools.ietf.org/html/rfc4287#section-4.2.12} See RFC 4287 § 4.2.12 }

The "atom:subtitle" element  is a Text construct that  conveys a human- readable
description or subtitle for a feed.

{[ atomSubtitle = element atom:subtitle { atomTextConstruct } ]}

*)
type subtitle = string

(**

{{: http://tools.ietf.org/html/rfc4287#section-4.2.15} See RFC 4287 § 4.2.15 }

The  "atom:updated" element  is a  Date  construct  indicating  the  most recent
instant  in time  when an  entry or  feed was  modified in  a way  the publisher
considers significant.  Therefore, not all modifications necessarily result in a
changed atom:updated value.

{[ atomUpdated = element atom:updated { atomDateConstruct } ]}

Publishers MAY change the value of this element over time.

*)
type updated = CalendarLib.Calendar.t

(**

{{: http://tools.ietf.org/html/rfc4287#section-4.2.11} See RFC 4287 § 4.2.11 }

If an  atom:entry is copied  from one feed  into another feed,  then  the source
atom:feed's metadata (all child elements  of atom:feed other than the atom:entry
elements) MAY  be preserved  within the  copied entry  by adding  an atom:source
child element,  if it is not already present in the entry, and including some or
all  of  the  source  feed's  Metadata  elements  as  the  atom:source element's
children. Such metadata SHOULD be preserved if the source atom:feed contains any
of   the  child   elements  atom:author,   atom:contributor,   atom:rights,   or
atom:category and those child elements are not present in the source atom:entry.

{[
  atomSource =
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
]}

The atom:source  element is designed to  allow the  aggregation of  entries from
different feeds while  retaining information about an  entry's source feed.  For
this reason, Atom Processors that are performing such aggregation SHOULD include
at least  the required feed-level  Metadata elements (atom:id,  atom:title,  and
atom:updated) in the atom:source element.

{{: http://tools.ietf.org/html/rfc4287#section-4.1.2} See RFC 4287 § 4.1.2 for
more details.}

*)
type source =
  {
    authors: author * author list;
    categories: category list;
    contributors: author list; (**
                                  {{: http://tools.ietf.org/html/rfc4287#section-4.2.3} See RFC 4287 § 4.2.3 } *)
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

(**

{{: http://tools.ietf.org/html/rfc4287#section-3.1.1} See RFC 4287 § 3.1.1 }

Text constructs MAY have a "type" attribute. When present, the value MUST be one
of [Text],  [Html],  or [Xhtml].  If the "type" attribute is not provided,  Atom
Processors MUST behave as though it were present with a value of "text".  Unlike
the atom:content element  defined in Section 4.1.3,  MIME  media types [MIMEREG]
MUST NOT be used as values for the "type" attribute on Text constructs.

{{: http://tools.ietf.org/html/rfc4287#section-4.1.3.1} See RFC 4287 § 4.1.3.1 }

On the  atom:content element,  the value of  the "type" attribute MAY  be one of
"text",  "html",  or "xhtml".  Failing that,  it MUST conform to the syntax of a
MIME  media type,  but  MUST NOT  be a  composite  type  (see  Section  4.2.6 of
[MIMEREG]).  If neither  the type attribute  nor the src  attribute is provided,
Atom Processors  MUST behave as  though the type  attribute were present  with a
value of "text".

*)
type type_content =
  | Html
  | Text
  | Xhtml
  | Mime of string

(**

{{: http://tools.ietf.org/html/rfc4287#section-4.1.3} See RFC 4287 § 4.1.3 }

The "atom:content" element either contains or links to the content of the entry.
The content of atom:content is Language-Sensitive.

{[
  atomInlineTextContent =
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
]}

{{: http://tools.ietf.org/html/rfc4287#section-4.1.3.2} See RFC 4287 § 4.1.3.2 }

atom:content MAY have  a "src" attribute,  whose value MUST  be an IRI reference
[RFC3987].  If the "src" attribute is present, atom:content MUST be empty.  Atom
Processors MAY  use the IRI  to retrieve the  content and  MAY choose  to ignore
remote content or to present it in a different manner than local content.

If the "src" attribute is present,  the  "type" attribute SHOULD be provided and
MUST be a MIME media type [MIMEREG], rather than "text", "html", or "xhtml". The
value is advisory;  that is to say,  when  the corresponding URI (mapped from an
IRI,  if necessary) is dereferenced,  if the  server providing that content also
provides a media type, the server-provided media type is authoritative.

*)
type content =
  {
    ty : type_content;
    src : Uri.t option;
    data : string;
  }

(**

{{: http://tools.ietf.org/html/rfc4287#section-4.2.13} See RFC 4287 § 4.2.13 }

The "atom:summary"  element is a Text  construct that  conveys a  short summary,
abstract, or excerpt of an entry.

{[ atomSummary = element atom:summary { atomTextConstruct } ]}

It is  not advisable  for the  atom:summary element  to duplicate  atom:title or
atom:content because Atom Processors might assume there is a useful summary when
there is none.

*)
type summary = string

(**

{{: http://tools.ietf.org/html/rfc4287#section-4.1.2} See RFC 4287 § 4.1.2 }

The "atom:entry" element represents an  individual entry,  acting as a container
for metadata and  data associated with the entry.  This element  can appear as a
child  of the  atom:feed element,  or  it  can  appear  as  the  document (i.e.,
top-level) element of a stand-alone Atom Entry Document.

{[
  atomEntry =
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
]}

This specification  assigns no significance to  the order  of appearance  of the
child elements of atom:entry.

The following  child elements are defined  by this  specification (note  that it
requires the presence of some of these elements):

{ul
{- {b atom:entry elements MUST contain one or more atom:author elements,  unless
the atom:entry contains  an  atom:source  element  that  contains an atom:author
element  or,  in  an Atom  Feed  Document,  the  atom:feed  element  contains an
atom:author element itself.}}
{- atom:entry elements MAY contain any number of atom:category
elements.}
{- atom:entry elements MUST NOT contain more than one atom:content element.}
{- atom:entry elements MAY contain any number of atom:contributor elements.}
{- atom:entry elements MUST contain exactly one atom:id element.}
{-  {b atom:entry  elements that  contain  no  child  atom:content  element MUST
contain  at  least  one  atom:link  element   with  a  rel  attribute  value  of
"alternate".}}
{- {b atom:entry elements MUST NOT  contain more than one atom:link element with
a rel attribute value  of "alternate" that has the same  combination of type and
hreflang attribute values.}}
{- atom:entry  elements MAY contain  additional atom:link elements  beyond those
described above.}
{- atom:entry elements MUST NOT contain more than one atom:published element.}
{- atom:entry elements MUST NOT contain more than one atom:rights element.}
{- atom:entry elements MUST NOT contain more than one atom:source element.}
{- atom:entry  elements MUST contain an  atom:summary element  in either  of the
following cases:
  {ul
  {- the atom:entry contains an atom:content  that has a "src" attribute (and is
  thus empty).}
  {- the atom:entry contains content that is encoded in Base64; i.e., the "type"
  attribute of atom:content  is a MIME media type [MIMEREG],  but  is not an XML
  media type  [RFC3023],  does not  begin with "text/",  and  does not  end with
  "/xml" or "+xml".}}}
{- atom:entry elements MUST NOT contain more than one atom:summary element.}
{- atom:entry elements MUST contain exactly one atom:title element.}
{- atom:entry elements MUST contain exactly one atom:updated element.}
}

*)
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

(**

{{: http://tools.ietf.org/html/rfc4287#section-4.1.1} See RFC 4287 § 4.1.1 }

The "atom:feed"  element is the  document (i.e.,  top-level) element  of an Atom
Feed Document,  acting as a container for  metadata and data associated with the
feed. Its element children consist of metadata elements followed by zero or more
atom:entry child elements.

{[
  atomFeed =
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
]}

This specification assigns  no significance to the order  of atom:entry elements
within the feed.

The following  child elements are defined  by this specification  (note that the
presence of some of these elements is required):

{ul
{- atom:feed elements MUST contain one or more atom:author elements,  unless all
of  the atom:feed  element's child  atom:entry  elements  contain  at  least one
atom:author element.}
{- atom:feed elements MAY contain any number of atom:category elements.}
{- atom:feed elements MAY contain any number of atom:contributor elements.}
{- atom:feed elements MUST NOT contain more than one atom:generator element.}
{- atom:feed elements MUST NOT contain more than one atom:icon element.}
{- atom:feed elements MUST NOT contain more than one atom:logo element.}
{- atom:feed elements MUST contain exactly one atom:id element.}
{- atom:feed elements SHOULD contain one  atom:link element with a rel attribute
value of "self".  This  is the preferred URI for  retrieving Atom Feed Documents
representing this Atom feed.}
{- atom:feed  elements MUST NOT contain  more than one atom:link  element with a
rel attribute  value of "alternate"  that has the  same combination of  type and
hreflang attribute values.}
{- atom:feed  elements MAY  contain additional  atom:link elements  beyond those
described above.}
{- atom:feed elements MUST NOT contain more than one atom:rights element.}
{- atom:feed elements MUST NOT contain more than one atom:subtitle element.}
{- atom:feed elements MUST contain exactly one atom:title element.}
{- atom:feed elements MUST contain exactly one atom:updated element.}
}

If multiple  atom:entry elements with the  same atom:id value appear  in an Atom
Feed Document,  they  represent the  same entry.  Their  atom:updated timestamps
SHOULD be different. If an Atom Feed Document contains multiple entries with the
same atom:id,  Atom Processors MAY choose to  display all of them or some subset
of them. One typical behavior would be to display only the entry with the latest
atom:updated timestamp.

*)
type feed =
  {
    authors : author list;
    categories : category list;
    contributors : author list;
    generator : generator option;
    icon : icon option;
    id : id;
    links : link list;
    logo : logo option;
    rights : rights option;
    subtitle : subtitle option;
    title : title;
    updated : updated;
    entries : entry list;
  }

val analyze : Xmlm.input -> feed

(** Analysis without verification, enjoy ! *)
val unsafe : Xmlm.input ->
  [> `Feed of
       [> `Author of
            [> `Email of string | `Name of string | `URI of string ] list
       | `Category of
            [> `Label of string | `Scheme of string | `Term of string ] list
       | `Contributor of
            [> `Email of string | `Name of string | `URI of string ] list
       | `Entry of
            [> `Author of
                 [> `Email of string | `Name of string | `URI of string ]
                   list
            | `Category of
                 [> `Label of string | `Scheme of string | `Term of string ]
                   list
            | `Content of
                 [> `Data of string | `SRC of string | `Type of string ]
                   list
            | `Contributor of
                 [> `Email of string | `Name of string | `URI of string ]
                   list
            | `ID of [> `URI of string ] list
            | `Link of
                 [> `HREF of string
                 | `HREFLang of string
                 | `Length of string
                 | `Rel of string
                 | `Title of string
                 | `Type of string ]
                   list
            | `Published of [> `Date of string ] list
            | `Rights of [> `Data of string ] list
            | `Source of
                 [> `Author of
                      [> `Email of string | `Name of string | `URI of string ]
                        list
                 | `Category of
                      [> `Label of string
                      | `Scheme of string
                      | `Term of string ]
                        list
                 | `Contributor of
                      [> `Email of string | `Name of string | `URI of string ]
                        list
                 | `Generator of
                      [> `Content of string
                      | `URI of string
                      | `Version of string ]
                        list
                 | `ID of [> `URI of string ] list
                 | `Icon of [> `URI of string ] list
                 | `Link of
                      [> `HREF of string
                      | `HREFLang of string
                      | `Length of string
                      | `Rel of string
                      | `Title of string
                      | `Type of string ]
                        list
                 | `Logo of [> `URI of string ] list
                 | `Rights of [> `Data of string ] list
                 | `Subtitle of [> `Data of string ] list
                 | `Title of [> `Data of string ] list
                 | `Updated of [> `Date of string ] list ]
                   list
            | `Summary of [> `Data of string ] list
            | `Title of [> `Data of string ] list
            | `Updated of [> `Date of string ] list ]
              list
       | `Generator of
            [> `Content of string | `URI of string | `Version of string ]
              list
       | `ID of [> `URI of string ] list
       | `Icon of [> `URI of string ] list
       | `Link of
            [> `HREF of string
            | `HREFLang of string
            | `Length of string
            | `Rel of string
            | `Title of string
            | `Type of string ]
              list
       | `Logo of [> `URI of string ] list
       | `Rights of [> `Data of string ] list
       | `Subtitle of [> `Data of string ] list
       | `Title of [> `Data of string ] list
       | `Updated of [> `Date of string ] list ]
         list ]
