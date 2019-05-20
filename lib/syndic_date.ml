open Printf
open Scanf

type t = Ptime.t

let epoch = Ptime.epoch
let compare = Ptime.compare
let max d1 d2 = if compare d1 d2 < 0 then d2 else d1
let min d1 d2 = if compare d1 d2 < 0 then d1 else d2
let month_to_int = Hashtbl.create 12

let () =
  let add m i = Hashtbl.add month_to_int m i in
  add "Jan" 1 ;
  add "Feb" 2 ;
  add "Mar" 3 ;
  add "Apr" 4 ;
  add "May" 5 ;
  add "Jun" 6 ;
  add "Jul" 7 ;
  add "Aug" 8 ;
  add "Sep" 9 ;
  add "Oct" 10 ;
  add "Nov" 11 ;
  add "Dec" 12

let map f = function Some x -> f x | None -> None
let map2 f a b = match (a, b) with Some a, Some b -> f a b | _ -> None

(* Format:
   http://www.rssboard.org/rss-specification#ltpubdategtSubelementOfLtitemgt
   Examples: Sun, 19 May 2002 15:21:36 GMT Sat, 25 Sep 2010 08:01:00 -0700 20
   Mar 2013 03:47:14 +0000 *)
let of_rfc822 s =
  let make_date day month year h m maybe_s z =
    let month =
      if String.length month <= 3 then month else String.sub month 0 3
    in
    let month = Hashtbl.find month_to_int month in
    let date = Ptime.of_date (year, month, day) in
    let s =
      if maybe_s <> "" && maybe_s.[0] = ':' then
        float_of_string (String.sub maybe_s 1 (String.length maybe_s - 1))
      else 0.
    in
    let span = Ptime.Span.of_int_s ((h * 3600) + (m * 60)) in
    let span =
      map (fun x -> Some (Ptime.Span.add span x)) (Ptime.Span.of_float_s s)
    in
    let date_and_time =
      if z = "" || z = "GMT" || z = "UT" || z = "Z" then
        map2 (fun date span -> Ptime.add_span date span) date span
        |> map (fun x -> Some (Ptime.to_date_time x))
      else
        (* FIXME: this should be made more robust. *)
        let tz_offset_s =
          match z with
          | "EST" -> -5 * 3600
          | "EDT" -> -4 * 3600
          | "CST" -> -6 * 3600
          | "CDT" -> -5 * 3600
          | "MST" -> -7 * 3600
          | "MDT" -> -6 * 3600
          | "PST" -> -8 * 3600
          | "PDT" -> -7 * 3600
          | "A" -> -1 * 3600
          | "M" -> -12 * 3600
          | "N" -> 1 * 3600
          | "Y" -> 12 * 3600
          | _ ->
              let zh = sscanf (String.sub z 0 3) "%i" (fun i -> i) in
              let zm = sscanf (String.sub z 3 2) "%i" (fun i -> i) in
              let tz_sign = if zh < 0 then -1 else 1 in
              if zh < 0 then tz_sign * ((-zh * 3600) + (zm * 60))
              else tz_sign * ((zh * 3600) + (zm * 60))
        in
        let rt = map2 (fun date span -> Ptime.add_span date span) date span in
        (* XXX: We lose minutes with this conversion, but Calendar does not
           propose to handle minutes. *)
        map (fun x -> Some (Ptime.to_date_time ~tz_offset_s x)) rt
    in
    match map Ptime.of_date_time date_and_time with
    | Some x -> x
    | None -> invalid_arg (sprintf "Syndic.Date.of_rfc822: cannot parse")
  in
  try
    if 'A' <= s.[0] && s.[0] <= 'Z' then
      try sscanf s "%_s %i %s %i %i:%i%s %s" make_date with _ ->
        try sscanf s "%_s %ist %s %i %i:%i%s %s" make_date with _ ->
          (* For e.g. "May 15th, 2019" — even though it is not standard *)
          sscanf s "%s %i%_s %i" (fun m d y -> make_date d m y 0 0 "" "UT")
    else
      try sscanf s "%i %s %i %i:%i%s %s" make_date with _ ->
        sscanf s "%i %s %i" (fun d m y -> make_date d m y 0 0 "" "UT")
  with _ -> invalid_arg (sprintf "Syndic.Date.of_string+: cannot parse %S" s)

type month =
  | Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec

type day = Thu | Fri | Sat | Sun | Mon | Tue | Wed

let string_of_month = function
  | Jan -> "Jan"
  | Feb -> "Feb"
  | Mar -> "Mar"
  | Apr -> "Apr"
  | May -> "May"
  | Jun -> "Jun"
  | Jul -> "Jul"
  | Aug -> "Aug"
  | Sep -> "Sep"
  | Oct -> "Oct"
  | Nov -> "Nov"
  | Dec -> "Dec"

let month_of_date =
  let months =
    [|Jan; Feb; Mar; Apr; May; Jun; Jul; Aug; Sep; Oct; Nov; Dec|]
  in
  fun t ->
    let _, i, _ = Ptime.to_date t in
    months.(i - 1)

(* RFC3339 date *)
let of_rfc3339 s =
  match Ptime.of_rfc3339 s with
  | Result.Error _ ->
      invalid_arg (sprintf "Syndic.Date.of_string: cannot parse %S" s)
  | Result.Ok (t, tz_offset_s, _) -> (
    match Ptime.of_date_time @@ Ptime.to_date_time ?tz_offset_s t with
    | Some x -> x
    | None -> invalid_arg (sprintf "Syndic.Data.of_string: cannot part %S" s) )

let to_rfc3339 d =
  (* Example: 2014-03-19T15:51:25.050-07:00 *)
  Ptime.to_rfc3339 d

(* Convenience functions *)

let day_of_week =
  let wday = [|Thu; Fri; Sat; Sun; Mon; Tue; Wed|] in
  fun t ->
    let i = fst Ptime.(Span.to_d_ps @@ to_span t) mod 7 in
    wday.((if i < 0 then 7 + i else i))

let string_of_day = function
  | Thu -> "Thu"
  | Fri -> "Fri"
  | Sat -> "Sat"
  | Sun -> "Sun"
  | Mon -> "Mon"
  | Tue -> "Tue"
  | Wed -> "Wed"

let year t =
  let year, _, _ = Ptime.to_date t in
  year

let month = month_of_date

let day t =
  let (_, _, day), _ = Ptime.to_date_time t in
  day

let hour t =
  let _, ((hh, _, _), _) = Ptime.to_date_time t in
  hh

let minute t =
  let _, ((_, mm, _), _) = Ptime.to_date_time t in
  mm

let second t =
  let _, ((_, _, ss), _) = Ptime.to_date_time t in
  float_of_int ss

let to_rfc822 t =
  (* Example: Sat, 25 Sep 2010 08:01:00 -0700 *)
  let ds = day_of_week t |> string_of_day in
  let ms = month_of_date t |> string_of_month in
  let (y, _m, d), ((hh, mm, ss), t) = Ptime.to_date_time t in
  Printf.sprintf "%s, %d %s %d %02d:%02d:%02d %04d" ds d ms y hh mm ss t
