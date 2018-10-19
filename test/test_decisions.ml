(* Use the W3C validator to motivate some decisions that were taken. *)

open Printf
open Lwt
open Syndic
module Client = Cohttp_lwt_unix.Client
module Body = Cohttp_lwt.Body

let check_feed data =
  printf "ATOM: %s\n" data ;
  Client.get (W3C.url (`Data data))
  >>= fun (_r, b) ->
  Body.to_string b
  >>= fun body ->
  let err, warn = `String (0, body) |> Xmlm.make_input |> W3C.parse in
  List.iter (fun e -> printf "ERROR: %s\n" e.W3C.text) err ;
  List.iter (fun e -> printf "WARNING: %s\n" e.W3C.text) warn ;
  return ()

let empty_title =
  "<?xml version='1.0' encoding='utf-8'?>\n\
  \   <feed xmlns='http://www.w3.org/2005/Atom'>\n\
  \     <title></title>\n\
  \   </feed>"

let main = check_feed empty_title
let () = Lwt_main.run main
