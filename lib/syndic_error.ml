open Printf

type expected =
  | Attr of string
  | Tag of string
  | Data
  | Root

type error = [
  | `Expected_data of string
  | `Expected_node of string
  | `Expected_attr of string
]

exception Expected of expected * expected
exception Expected_Data
exception Error of Xmlm.pos * error

let string_of_expected () = function
  | Attr a -> a ^ "="
  | Tag a -> "<" ^ a ^ ">"
  | Data -> "data"
  | Root -> "root"

let string_of_expectation (a, b) =
  sprintf "Expected %a in %a" string_of_expected a string_of_expected b

let raise_expectation data in_data = raise (Expected (data, in_data))

