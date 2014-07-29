type rel =
  | Alternate
  | Related
  | Self
  | Enclosure
  | Via
  | Link of Uri.t

type link = [
  | `Uri of Uri.t
  | `Rel of rel
  | `Type of string
  | `Lang of string
  | `Title of string
  | `Length of int
]

type author = [
  | `Name of string
  | `Link of link list
  | `EMail of string
]

type category = [
  | `Name of string
  | `Scheme of Uri.t
  | `Label of string
]

type media = [
  | `Link of link list
  | `Length of int
  | `MimeType of string
]

type image = [
  | `Url of Uri.t
  | `Title of string
  | `Link of link list
  | `Width of int
  | `Height of int
  | `Decription of string
]

type source = [
  | `Name of string
  | `Url of Uri.t
  | `Category of category
  | `Contributor of author
  | `Icon of Uri.t
  | `ID of Uri.t
  | `Logo of Uri.t
  | `Right of string
  | `Subtitle of string
  | `Date of string (* Calendar *)
]

type feed = [
  | `Title of string
  | `Link of link list
  | `Description of string
  | `Story of string
  | `Author of author list
  | `Category of category list
  | `Comments of Uri.t
  | `Enclosure of media list
  | `GUID of string | `ID of Uri.t
  | `Date of string (* Calendar *)
  | `Source of source list
  | `Summary of string
]

type textinput = [
  | `Title of string
  | `Description of string
  | `Name of string
  | `Link of link list
]

type generator = [
  | `Version of string
  | `Name of string
  | `Link of link list
]

type root = [
  | `Author of author
  | `Category of category
  | `Contributor of author
  | `Icon of Uri.t
  | `ID of Uri.t
  | `Logo of Uri.t
  | `Rights of string
  | `Subtitle of string
  | `Title of string
  | `Date of string (* Calendar *)
  | `Feed of feed list
  | `Language of string
  | `Copyright of string
  | `ManagingEditor of string
  | `WebMaster of string
  | `Category of category
  | `RFC of Uri.t
  | `Cloud of [
      | `Domain of Uri.t
      | `Port of int
      | `Path of string
      | `RegisterProcedure of string
      | `Protocol of string ] list
  | `TTL of int
  | `Image of image list
  | `Rating of int
  | `Textinput of textinput list
  | `SkipHours of int
  | `SkipDays of int
  | `Generator of generator list
]
