open Printf

type error = [
  | `Expected of string
]

exception Error of Xmlm.pos * error

let to_string = function
  | Error (pos, `Expected str) ->
    sprintf "%s at l.%d" str (fst pos)
  | exn -> Printexc.to_string exn
