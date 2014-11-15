type t = CalendarLib.Calendar.t

val compare : t -> t -> int
val now : unit -> t
val from_unixfloat : float -> t

val of_rfc822 : string -> CalendarLib.Calendar.t
val to_rfc822 : CalendarLib.Calendar.t -> string

val of_rfc3339 : string -> CalendarLib.Calendar.t
val to_rfc3339 : CalendarLib.Calendar.t -> string
