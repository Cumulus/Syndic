
(** The common signature that all error modules must (at least) satisfy. *)
module type T =
  sig
    type expected_type =
      | Attr of string
      | Tag of string
      | Data
      | Root

    exception Expected of expected_type * expected_type
    exception Expected_Leaf

    val string_of_expectation : expected_type * expected_type -> string
    val raise_expectation : expected_type -> expected_type -> 'a
  end
