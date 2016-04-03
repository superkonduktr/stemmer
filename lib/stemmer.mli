(** 
   
   An implementation of the Porter stemming algorithm

   http://tartarus.org/martin/PorterStemmer

   The original paper: http://tartarus.org/martin/PorterStemmer/def.txt

 *)


open Core.Std


(** A representation of char in terms of vowels and consonants. *)

module Char_phoneme : sig

  (** A character (a-z) is classified as either a vowel or a consonant.
      'y' is considered a consonant. *)
  type t

  (** Coerces a char to a Vowel or a Consonant. *)
  val of_char : char -> t

  (** Coerces a Vowel or a Consonant into a char. *)
  val to_char : t -> char
end


(** Helpers *)

(** Extracts the (VC) pattern from a word. *)
val stem : string -> string

(** Returns the measure of a word, i.e. the number of repetitions of the (VC)
    pattern. *)
val measure : string -> int

(** If possible, substitutes a given suffix in a string with another one. *)
val replace_suffix : word: string -> string -> string -> string

(** A rule accepts a condition predicate that accepts a string, and a 
    production function of a string executed if the predicate is true.
    Depending on whether the condition is met or not, the rule is considered
    ignored or matched. *)
val rule : (word: string -> bool) -> (word: string -> string) -> string ->
    [> `Rule_ignore | `Rule_match of string ]

(** A rule with entry accepts an additional condition that is checked before
    applying the main condition. If the entry condition is met, the rule is
    either failed or matched, otherwise it is ignored. *)
val rule_with_entry : entry: (word: string -> bool) -> (word: string -> bool) ->
    (word: string -> string) -> string ->
    [> `Rule_fail | `Rule_ignore | `Rule_match of string ]

(** A step is a sequence of rules applied to a string, evaluated one at a time.
    If a rule is ignored, the next one is tried. If a rule is failed or matched,
    its result is returned as a result of the entire step. *)
val step : (string -> [< `Rule_ignore | `Rule_match of string ]) list ->
           word: string -> string


(** Conditions *)

(** Checks for the *v* pattern, i.e., whether a string contains a vowel. *)
val contains_v : string -> bool

(** Checks for the *d pattern, i.e. whether a string ends with a double 
    consonant. *)
val ends_d : string -> bool

(** Checks for the *o pattern, i.e. whether a string ends with a *cvc pattern,
    where the second c is not W, X or Y. *)
val ends_with : string -> word: string -> bool

(** Checks if a string ends with a suffix; if so, checks if a predicate is true
    when applied to the stem preceding the suffix. *)
val stem_suffix_cond : suffix: string -> stem_f: (string -> bool) ->
                       word: string -> bool


(** Intermediate transformations *)
val step_1a : string -> string
val step_1b : string -> string
val step_1c : string -> string
val step_2 : string -> string
val step_3 : string -> string
val step_4 : string -> string
val step_5a : string -> string
val step_5b : string -> string

(** Aggregated transformation. For any string of length > 2 passes it through
    all the steps. *)
val run : string -> string
