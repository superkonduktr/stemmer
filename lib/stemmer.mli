open Core.Std

(** A representation of char in terms of vowels and consonants. *)
module Char_phoneme : sig
  type t
  val of_char : char -> t
  val to_char : t -> char
end


(** Helpers *)

(** Extracts the (VC) pattern from a word. *)
val stem : string -> string

(** Returns the measure of a word, i.e. the number of repetitions of the (VC) pattern. *)
val measure : string -> int

(** If possible, substitutes a given suffix in a string with another one. *)
val replace_suffix : word: string -> string -> string -> string

(** A rule is accepts a condition predicate to string and a production function executed if the predicate is true. *)
val rule : (word: string -> bool) -> (word: string -> string) -> string -> [> `Rule_ignore | `Rule_match of string ]

(** A step is a sequence of rules evaluated one at a time. Only the first matched rule is fired. *)
val step : (string -> [< `Rule_ignore | `Rule_match of string ]) list -> word: string -> string


(** Conditions *)

(** Checks for the *v* pattern, i.e., whether a string contains a vowel. *)
val contains_v : string -> bool

(** Checks for the *d pattern, i.e. whether a string ends with a double consonant. *)
val ends_d : string -> bool

(** Checks for the *o pattern, i.e. whether a string ends with a *cvc pattern, where the second c is not W, X or Y. *)
val ends_with : string -> word: string -> bool


(** Intermediate transformations *)
val step_1a : string -> string
val step_1b : string -> string
val step_1c : string -> string
val step_2 : string -> string
val step_3 : string -> string
val step_4 : string -> string
val step_5a : string -> string
val step_5b : string -> string

(** Aggregated transformation *)
val run : string -> string
