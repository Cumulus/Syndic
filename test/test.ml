open Lwt

module CLU = Cohttp_lwt_unix
module CLB = Cohttp_lwt_body

type result =
  | Ok
  | Error of string

exception Is_not_a_file

type src =
  [
    | `String of string
    | `Uri of Uri.t
  ]

type fmt =
  [
    | `Atom
    | `Rss1
    | `Rss2
    | `Opml1
  ]

let get = function
  | `Uri src ->
    CLU.Client.get src
    >>= fun (response, body) -> CLB.to_string body
    >>= fun data -> Lwt.return (`String (0, data))
  | `String data -> Lwt.return (`String (0, data))
  | _ -> raise (Invalid_argument "Not implemented yet")

let parse ?xmlbase = function
  | `Rss1 -> fun src -> `Rss1 (Syndic.Rss1.parse ?xmlbase src)
  | `Rss2 -> fun src -> `Rss2 (Syndic.Rss2.parse ?xmlbase src)
  | `Atom -> fun src -> `Atom (Syndic.Rss2.parse ?xmlbase src)
  | `Opml1 -> fun src -> `Opml1 (Syndic.Rss2.parse ?xmlbase src)

let string_of_src = function
  | `Uri uri -> Printf.sprintf "'%s'" (Uri.to_string uri)
  | `String data ->
    let buffer = Buffer.create 16 in
    Buffer.add_string buffer (String.sub data 0 16);
    Buffer.add_string buffer "...";
    Buffer.contents buffer

let string_of_fmt = function
  | `Rss1 -> "RSS 1.0"
  | `Rss2 -> "RSS 2.0"
  | `Atom -> "Atom"
  | `Opml1 -> "OPML 1.0"

let tests : ([> src ] * [< fmt ] * result) list =
  [
    (`Uri (Uri.of_string "http://ocaml.org/feed.xml"), `Atom, Ok);
  ]

let%lwt () =
  Lwt_list.map_p
    (fun (src, fmt, result) ->
       Printf.printf "[%s] %s:%!"
         (string_of_fmt fmt)
         (string_of_src src);
       get src
       >>= (fun src ->
         try%lwt let _ = parse fmt (Xmlm.make_input src) in Lwt.return Ok
         with exn -> Lwt.return (Error (Printexc.to_string exn)))
       >>= function
         | Ok -> Lwt.return (Printf.printf " Ok.\n%!")
         | Error str -> Lwt.return (Printf.printf " %s.\n%!" str))
    tests
  >>= fun _ -> Lwt.return ()
