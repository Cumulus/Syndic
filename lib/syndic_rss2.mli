(** [Syndic.Rss2]: compliant with
    {{: http://www.rssboard.org/rss-specification#ltcloudgtSubelementOfLtchannelgt} RSS 2.0}. *)

module Error : Syndic_error.T

(**

{{: http://www.rssboard.org/rss-specification#ltimagegtSubelementOfLtchannelgt} See RSS 2.0 about <image> }

<image> is an optional sub-element  of <channel>,  which contains three required
and three optional sub-elements.

<url> is the URL of a GIF, JPEG or PNG image that represents the channel.

<title> describes the  image,  it's used in the ALT attribute  of the HTML <img>
tag when the channel is rendered in HTML.

<link> is the URL of the site, when the channel is rendered, the image is a link
to the site.  (Note,  in  practice the image <title> and  <link> should have the
same value as the channel's <title> and <link>.

Optional elements include <width>  and <height>,  numbers,  indicating the width
and height of the image in pixels.  <description> contains text that is included
in  the  TITLE attribute  of  the link  formed  around  the  image  in  the HTML
rendering.

Maximum value for width is 144, default value is 88.

Maximum value for height is 400, default value is 31.

*)
type image =
  {
    url: Uri.t;
    title: string;
    link: Uri.t;
    width: int;
    height: int;
    description: string option;
  }

(**

{{: http://www.rssboard.org/rss-specification#ltcloudgtSubelementOfLtchannelgt} See RSS 2.0 about <cloud> }

<cloud> is an optional sub-element of <channel>.

It specifies  a web service  that supports the  rssCloud interface which  can be
implemented in HTTP-POST, XML-RPC or SOAP 1.1.

Its purpose  is to allow  processes to register with  a cloud to  be notified of
updates to  the channel,  implementing a  lightweight publish-subscribe protocol
for RSS feeds.

{[ <cloud domain="rpc.sys.com" port="80" path="/RPC2" registerProcedure="myCloud.rssPleaseNotify" protocol="xml-rpc" /> ]}

In this example, to request notification on the channel it appears in, you would
send an XML-RPC  message to rpc.sys.com on port 80,  with  a path of /RPC2.  The
procedure to call is myCloud.rssPleaseNotify.

*)
type cloud =
  {
    domain: Uri.t;
    port: int;
    path: string;
    registerProcedure: string;
    protocol: string;
  }

(**

{{: http://www.rssboard.org/rss-specification#lttextinputgtSubelementOfLtchannelgt} See RSS 2.0 about <textinput> }

A channel may optionally contain a <textInput> sub-element,  which contains four
required sub-elements.

<title> -- The label of the Submit button in the text input area.

<description> -- Explains the text input area.

<name> -- The name of the text object in the text input area.

<link> -- The URL of the CGI script that processes text input requests.

The purpose of the <textInput> element is something of a mystery. You can use it
to specify a search engine box.  Or to allow a reader to provide feedback.  Most
aggregators ignore it.

*)
type textinput =
  {
    title: string;
    description: string;
    name: string;
    link: Uri.t;
  }

(**

{{: http://www.rssboard.org/rss-specification#ltcategorygtSubelementOfLtitemgt} See RSS 2.0 about <category> }

<category> is an optional sub-element of <item>.

It has one optional attribute, domain, a string that identifies a categorization
taxonomy.

The value of  the element is a forward-slash-separated  string that identifies a
hierarchic  location  in  the  indicated  taxonomy.   Processors  may  establish
conventions  for the  interpretation of  categories.  Two examples  are provided
below:

{[ <category>Grateful Dead</category> ]}

{[ <category domain="http://www.fool.com/cusips">MSFT</category> ]}

You may include as many category elements as you need to, for different domains,
and to have an item cross-referenced in different parts of the same domain.

*)
type category =
  {
    data: string;
    domain: Uri.t option;
  }

(**

{{: http://www.rssboard.org/rss-specification#ltenclosuregtSubelementOfLtitemgt} See RSS 2.0 about <enclosure> }

<enclosure> is an optional sub-element of <item>.

It  has three  required attributes.  url  says where  the enclosure  is located,
length says how big it is in bytes,  and type says what its type is,  a standard
MIME type.

The url must be an http url.

{[ <enclosure url="http://www.scripting.com/mp3s/weatherReportSuite.mp3" length="12216320" type="audio/mpeg" /> ]}

*)
type enclosure =
  {
    url: Uri.t;
    length: int;
    mime: string;
  }

(**

{{: http://www.rssboard.org/rss-specification#ltguidgtSubelementOfLtitemgt} See RSS 2.0 about <guid> }

<guid> is an optional sub-element of <item>.

guid  stands  for  globally  unique  identifier.  It's  a  string  that uniquely
identifies the item.  When present,  an aggregator may choose to use this string
to determine if an item is new.

{[ <guid>http://some.server.com/weblogItem3207</guid> ]}

There are no  rules for the syntax  of a guid.  Aggregators must view  them as a
string.  It's up to  the source of the  feed to establish the  uniqueness of the
string.

If the guid element has an attribute named isPermaLink with a value of true, the
reader may assume that it is a permalink to the item, that is, a url that can be
opened in a Web  browser,  that points to the full item  described by the <item>
element. An example:

{[ <guid isPermaLink="true">http://inessential.com/2002/09/01.php#a2</guid> ]}

isPermaLink is optional,  its default value is true.  If its value is false, the
guid may not be assumed to be a url, or a url to anything in particular.

*)
type guid =
  {
    data: Uri.t; (* must be uniq *)
    permalink: bool; (* default true *)
  }

(**

{{: http://www.rssboard.org/rss-specification#ltsourcegtSubelementOfLtitemgt} See RSS 2.0 about <source> }

<source> is an optional sub-element of <item>.

Its value is the name of the  RSS channel that the item came from,  derived from
its <title>.  It has one required attribute,  url, which links to the XMLization
of the source.

{[ <source url="http://www.tomalak.org/links2.xml">Tomalak's Realm</source> ]}

The purpose of this element is  to propagate credit for links,  to publicize the
sources of news items.  It can be used in the Post command of an aggregator.  It
should be generated automatically when forwarding  an item from an aggregator to
a weblog authoring tool.

*)
type source =
  {
    data: string;
    url: Uri.t;
  }

type story =
  | All of string * string
  | Title of string
  | Description of string

(**

{{: http://www.rssboard.org/rss-specification#hrelementsOfLtitemgt} See RSS 2.0 about <item> }

A channel may contain any number of <item>s.  An item may represent a "story" --
much like a story  in  a  newspaper  or  magazine;  if  so  its description is a
synopsis of the story,  and the link points to the full story.  An item may also
be complete in itself,  if so, the description contains the text (entity-encoded
HTML is  allowed;  see examples),  and the  link and title  may be omitted.  All
elements  of  an  item  are optional,  {b  however  at  least  one  of  title or
description must be present.}

{ul
{- title       : The title of the item. }
{- link        : The URL of the item. }
{- description : The item synopsis. }
{- author      : Email address of the author of the item. }
{- category    : Includes the item in one or more categories. }
{- comments    : URL of a page for comments relating to the item. }
{- enclosure   : Describes a media object that is attached to the item. }
{- guid        : A string that uniquely identifies the item. }
{- pubDate     : Indicates when the item was published. }
{- source      : The RSS channel that the item came from. }
}

*)
type item =
  {
    story: story;
    link: Uri.t option;
    author:  string option; (* e-mail *)
    category: category option;
    comments: Uri.t option;
    enclosure: enclosure option;
    guid: guid option;
    pubDate: Netdate.t option; (* date *)
    source: source option;
  }

(**

{{: http://www.rssboard.org/rss-specification#requiredChannelElements} See RSS 2.0 about <channel> }

Here's a list of the required  channel elements,  each with a brief description,
an example, and where available, a pointer to a more complete description.

{ul
{- title       : The name of the channel. It's how people refer to your service.
If you have an HTML website that contains the same information as your RSS file,
the title of your channel should be the same as the title of your website. }
{- link        : The URL to the HTML website corresponding to the channel. }
{- description : Phrase or sentence describing the channel. }
}

Here's a list of optional channel elements.

{ul
{- language :
The language  the channel is written  in.  This allows aggregators  to group all
Italian language  sites,  for example,  on a  single page.  A list  of allowable
values for this  element,  as provided by Netscape,  is here.  You  may also use
values defined by the W3C. }
{- copyright :
Copyright notice for content in the channel. }
{- managingEditor :
Email address  for person responsible for editorial content. }
{-  webMaster :
Email address for person responsible for technical issues relating to channel. }
{- pubDate :
The publication date for the content in the channel.  For example,  the New York
Times publishes  on a  daily basis,  the  publication date  flips once  every 24
hours.  That's when the  pubDate of the channel changes.  All  date-times in RSS
conform to the Date and Time  Specification of RFC 822,  with the exception that
the  year  may  be  expressed  with  two  characters  or  four  characters (four
preferred). }
{- lastBuildDate :
The last time the content of the channel changed. }
{- category :
Specify one  or more categories that  the channel belongs  to.  Follows the same
rules as the <item>-level category element. More info. }
{- generator :
A string indicating the program used to generate the channel. }
{- docs :
A URL that points to the documentation for the format used in the RSS file. It's
probably a pointer to this page. It's for people who might stumble across an RSS
file on a Web server 25 years from now and wonder what it is. }
{- cloud :
Allows processes  to register  with a  cloud to  be notified  of updates  to the
channel,  implementing a  lightweight publish-subscribe protocol  for RSS feeds.
More info here. }
{- ttl :
ttl stands for time to live.  It's a number of minutes that indicates how long a
channel can be cached before refreshing from the source. More info here. }
{- image :
Specifies a GIF, JPEG or PNG image that can be displayed with the channel.  More
info here. }
{- rating :
The PICS rating for the channel. }
{- textInput :
Specifies  a  text  input  box  that  can  be displayed with the
channel. More info here. }
{- skipHours :
A hint  for aggregators telling them  which hours  they can  skip.  This element
contains up to 24 <hour> sub-elements whose  value is a number between 0 and 23,
representing a time in GMT,  when aggregators,  if they support the feature, may
not read the  channel  on  hours  listed  in  the <skipHours> element.  The hour
beginning at midnight is hour zero. }
{- skipDays :
A hint  for aggregators  telling them  which days  they can  skip.  This element
contains  up  to  seven  <day>  sub-elements  whose  value  is Monday,  Tuesday,
Wednesday,  Thursday,  Friday, Saturday or Sunday.  Aggregators may not read the
channel during days listed in the <skipDays> element. }
}

*)
type channel =
  {
    title: string;
    link: Uri.t;
    description: string;
    language: string option;
    copyright: string option;
    managingEditor: string option;
    webMaster: string option;
    pubDate: Netdate.t option;
    lastBuildDate: Netdate.t option;
    category: string option;
    generator: string option;
    docs: Uri.t option;
    cloud: cloud option;
    ttl: int option; (** {{:
      http://www.rssboard.org/rss-specification#ltcloudgtSubelementOfLtchannelgt} See RSS 2.0 about <ttl> } *)
    image: image option;
    rating: int option; (* lol *)
    textInput: textinput option;
    skipHours: int option;
    skipDays: int option;
    items: item list;
  }

val analyze : Xmlm.input -> channel

(** Analysis without verification, enjoy ! *)
val unsafe : Xmlm.input ->
  [> `Channel of
       [> `Category of string
       | `Cloud of
            [> `Domain of string
            | `Path of string
            | `Port of string
            | `Protocol of string
            | `RegisterProcedure of string ]
              list
       | `Copyright of string
       | `Description of string
       | `Docs of string
       | `Generator of string
       | `Image of
            [> `Description of string
            | `Height of string
            | `Link of string
            | `Title of string
            | `URL of string
            | `Width of string ]
              list
       | `Item of
            [> `Author of string
            | `Category of [> `Data of string | `Domain of string ] list
            | `Comments of string
            | `Description of string
            | `Enclosure of
                 [> `Length of string | `Mime of string | `URL of string ] list
            | `Guid of [> `Data of string | `Permalink of string ] list
            | `Link of string
            | `PubDate of string
            | `Source of [> `Data of string | `URL of string ] list
            | `Title of string ]
              list
       | `Language of string
       | `LastBuildDate of string
       | `Link of string
       | `ManagingEditor of string
       | `PubDate of string
       | `Rating of string
       | `SkipDays of string
       | `SkipHours of string
       | `TTL of string
       | `TextInput of
            [> `Description of string
            | `Link of string
            | `Name of string
            | `Title of string ]
              list
       | `Title of string
       | `WebMaster of string ]
         list ]
