#use "topfind" ;;

#require "fmt" ;;
#require "fpath" ;;
#require "uri" ;;

#require "curl" ;;

let () = Printexc.register_printer
    (function
      | Curl.CurlException (code, errno, err) ->
        Some (Fmt.strf "(CurlException (%s, %d, %s))" (Curl.strerror code) errno err)
      | _ -> None)

let curl_setup_simple h =
  let open Curl in
  set_useragent h "Syndic" ;
  set_nosignal h true ;
  set_connecttimeout h 5 ;
  set_timeout h 10 ;
  set_followlocation h true ;
  set_maxredirs h 10 ;
  set_ipresolve h IPRESOLVE_V4 ;
  set_encoding h CURL_ENCODING_ANY

let download h =
  let b = Buffer.create 16 in
  Curl.set_writefunction h (fun s -> Buffer.add_string b s ; String.length s) ;
  Curl.perform h ;
  Buffer.contents b

let get url =
  let h = Curl.init () in
  Curl.set_url h (Uri.to_string url) ;
  curl_setup_simple h ;
  download h

let man () = Fmt.epr "%s --uri <uri> --output <file>\n%!" Sys.argv.(0)
let success = 0
let failure = 1

let () =
  let uri, output = try
      match Sys.argv with
      | [| _; "--uri"; uri; "--output"; output |] -> Uri.of_string uri, Fpath.v output
      | _ -> man () ; exit failure
    with _ -> man () ; exit failure in
  match get uri with
  | contents ->
    let oc = open_out (Fpath.to_string output) in
    let ppf = Format.formatter_of_out_channel oc in
    Fmt.pf ppf "%s%!" contents ;
    close_out oc ;
    exit success
  | exception exn ->
    Fmt.epr "Retrieve an error: %s.\n%!" (Printexc.to_string exn) ; exit failure
