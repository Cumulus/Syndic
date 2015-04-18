open Lwt

module CLU = Cohttp_lwt_unix
module CLB = Cohttp_lwt_body

type result =
  | Ok
  | SyndicError of (Xmlm.pos * string)
  | W3CError of (Xmlm.pos * string) list
  | AnotherError of exn

exception Is_not_a_file

type src =
  [ `Data of string
  | `Uri of Uri.t  ]

type fmt =
  [ `Atom
  | `Rss1
  | `Rss2
  | `Opml1 ]

let get : src -> Xmlm.source Lwt.t = function
  | `Uri src ->
     CLU.Client.get src
     >>= fun (response, body) -> CLB.to_string body
     >>= fun data -> Lwt.return (`String (0, data))
  | `Data data -> Lwt.return (`String (0, data))

let parse ?xmlbase = function
  | `Rss1 -> fun src -> `Rss1 (Syndic.Rss1.parse ?xmlbase src)
  | `Rss2 -> fun src -> `Rss2 (Syndic.Rss2.parse ?xmlbase src)
  | `Atom -> fun src -> `Atom (Syndic.Atom.parse ?xmlbase src)
  | `Opml1 -> fun src -> `Opml1 (Syndic.Opml1.parse ?xmlbase src)

let string_of_src = function
  | `Uri uri -> Printf.sprintf "'%s'" (Uri.to_string uri)
  | `Data data ->
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

let () =
  Printexc.register_printer
    (function Syndic_error.Error _ as exn ->
                Some (Syndic_error.to_string exn)
            | _ -> None)

module Printf =
  struct
    include Printf

    let add_list ?(sep="") add_data ch lst =
      let rec aux = function
        | [] -> ()
        | [ x ] -> add_data ch x
        | x :: r ->
          Printf.fprintf ch "%a%s" add_data x sep; aux r
      in aux lst

  end

let state, switch =
  let e = ref false in
  (fun () -> !e),
  (fun () -> e := true)

let make_test (src, fmt, result) =
  Printf.printf "-+-+-+-+-+- [%s] %s -+-+-+-+-+-\n%!"
    (string_of_fmt fmt)
    (string_of_src src);
  get src
  >>= fun xmlm_source ->
  Lwt.catch
    (fun () -> let _ = parse fmt (Xmlm.make_input xmlm_source) in Lwt.return Ok)
    (function Syndic_error.Error (pos, err) ->
              get (`Uri (Syndic.W3C.url src))
              >>= fun xmlm_source ->
              Lwt.return (Syndic.W3C.parse (Xmlm.make_input xmlm_source))
              >>= (function [] -> Lwt.return (SyndicError (pos, err))
                          | errors ->
                            Lwt.return (W3CError (List.map Syndic.W3C.to_error errors)))
            | exn -> Lwt.return (AnotherError exn))
  >>= function SyndicError (pos, err) ->
               Printf.printf "Syndic: %s\n%!"
                 (Syndic_error.to_string (Syndic_error.Error (pos, err)));

               switch ();

               Lwt.return ()
             | W3CError errors ->
               let add_error ch (pos, err) =
                 Printf.fprintf ch "W3C: %s"
                   (Syndic_error.Error (pos, err) |> Syndic_error.to_string)
               in
               Printf.printf "%a%!"
                 (Printf.add_list ~sep:"\n" add_error) errors;
               Lwt.return ()
             | AnotherError exn ->
               Printf.printf "Error: %s\n%!"
                 (Printexc.to_string exn);

               switch ();

               Lwt.return ()
             | Ok ->
               Printf.printf "This document is valid!\n%!";
               Lwt.return ()

let () =
  Lwt_unix.run (Lwt_list.map_s make_test tests >>= fun _ -> Lwt.return ());
  if state () then exit 1 else exit 0
