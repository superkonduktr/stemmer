open Core.Std
open Re2.Std

module Char_phoneme = struct
  type t =
    | Vowel of char
    | Consonant of char

  let of_char c =
    let vowels = String.to_list "aeiou" in
    match List.exists ~f: (fun c' -> c' = c) vowels with
    | true -> Vowel c
    | false -> Consonant c

  let to_char t = match t with Vowel c | Consonant c -> c
end

let stem word =
  let rec drop_y w =
    match String.chop_prefix ~prefix: "y" w with
    | None -> w
    | Some w' -> drop_y w' in
  let word = drop_y word in
  let re =
    ["^(";
     "([^aeiou]*(?P<s1>y.*?)[aeiou]+)|";
     "([^aeiou]*(?P<s2>.*?)[aeiou]+)|";
     "([^aeiou]*(?P<s3>y.*?[^aeiou]+)y)|";
     "([^aeiou]*(?P<s4>.*?[^aeiou]+)y)|";
     "([^aeiou]*(?P<s5>y.*))|";
     "([^aeiou]*(?P<s6>.*))";
     ")$"]
    |> String.concat |> Re2.create_exn in
  let apply pattern =
    match Re2.find_first ~sub: (`Name pattern) re word with
    | Core_kernel.Result.Error _ -> None
    | Core_kernel.Result.Ok s -> Some s
  in
  let rec iter = function
  | [] -> ""
  | p :: tl ->
    match apply p with
    | Some s -> s
    | None -> iter tl
  in iter ["s1"; "s2"; "s3"; "s4"; "s5"; "s6"]

let measure word =
  let open Char_phoneme in
  let stem_measure word_stem =
    (* is_v holds a flag for whether the current iteration is a V *)
    let rec iter l m is_v =
      match l, is_v with
      | [], _ -> m
      | s :: tl, false ->
        (match s with
         | Vowel _ | Consonant 'y' -> iter tl (m + 1) true
         | _ -> iter tl m false)
      | s :: tl, true ->
        (match s with
         | Vowel _ -> iter tl m true
         | _ -> iter tl m false)
    in iter (word_stem |> String.to_list |> List.map ~f: of_char) 0 false
  in word |> stem |> stem_measure

let replace_suffix ~word suffix replacement =
  match String.chop_suffix ~suffix word with
  | None -> word
  | Some w -> w ^ replacement

let drop_last ~word =
  match String.length word with
  | 0 -> ""
  | _ -> String.sub word ~pos: 0 ~len: (String.length word - 1)

let rule condition production =
  fun word ->
    match condition ~word with
    | true -> `Rule_match (production ~word)
    | false -> `Rule_ignore

let rule_with_entry ~entry condition production =
  fun word ->
    match entry ~word with
    | false -> `Rule_ignore
    | true ->
      match condition ~word with
      | false -> `Rule_fail
      | true -> `Rule_match (production ~word)
      
let step rules ~word =
  let rec iter = function
  | [] -> word
  | r :: tl ->
    match r word with
    | `Rule_fail -> word
    | `Rule_match w -> w
    | `Rule_ignore -> iter tl
  in iter rules

let ends_with suffix ~word =
  String.is_suffix ~suffix word

let stem_suffix_cond ~suffix ~stem_f ~word =
  match String.chop_suffix ~suffix word with
  | None -> false
  | Some s -> stem_f s

let contains_v str =
  let re = Re2.create_exn "^(.*[aeiou].*|.*[^aeiouy]y.*)$" in
  Re2.matches re str

let ends_d str =
  (* Should have been just ".*([^aeiou])\1$", but Re2 won't allow it. *)
  let length = String.length str in
  let open Char_phoneme in
  length > 1 &&
  match str |> String.sub ~len: 2 ~pos: (length - 2)
            |> String.to_list
            |> List.map ~f: of_char
  with
  | Consonant f :: [Consonant s] -> f = s
  | _ -> false

let ends_cvc ~word =
  let re = Re2.create_exn "^.*[^aeiou][aeiouy][^aeiouwxy]$" in
  Re2.matches re word

let step_1a word =
  let rules = [rule (ends_with "sses") (replace_suffix "sses" "ss");
               rule (ends_with "ies") (replace_suffix "ies" "i");
               rule (ends_with "ss") (replace_suffix "ss" "ss");
               rule (ends_with "s") (replace_suffix "s" "")]
  in step ~word rules

(* Step 1b has its own logic of chaining conditions *)
let step_1b word =
  let rule_1 =
    rule_with_entry ~entry: (ends_with "eed")
                    (stem_suffix_cond ~suffix: "eed"
                                      ~stem_f: (Fn.compose ((<) 0) measure))
                    (replace_suffix "eed" "ee") in
  match rule_1 word with
  | `Rule_match w -> w
  | `Rule_fail -> word
  | `Rule_ignore ->
    let step' rules ~word =
      let rec iter = function
      | [] -> `Step_fail word
      | r :: tl ->
        match r word with
        | `Rule_match w -> `Step_match w
        | _ -> iter tl
      in iter rules
    in
    match step' ~word [rule (stem_suffix_cond ~suffix: "ed" ~stem_f: contains_v)
                            (replace_suffix "ed" "");
                       rule (stem_suffix_cond ~suffix: "ing" ~stem_f: contains_v)
                            (replace_suffix "ing" "")]
    with
    | `Step_fail _ -> word
    | `Step_match w' ->
       step ~word: w' [rule (ends_with "at") (replace_suffix "at" "ate");
                       rule (ends_with "bl") (replace_suffix "bl" "ble");
                       rule (ends_with "iz") (replace_suffix "iz" "ize");
                       rule (fun ~word ->
                               ends_d word &&
                               not (ends_with "l" ~word ||
                                    ends_with "s" ~word ||
                                    ends_with "z" ~word))
                            drop_last;
                       rule (fun ~word -> measure word = 1 && ends_cvc word)
                            (replace_suffix "" "e")]

let step_1c word =
  step ~word [rule (stem_suffix_cond ~suffix: "y" ~stem_f: contains_v)
                   (replace_suffix "y" "i")]

let step_2 word =
  let rules =
    ["ational", "ate";
     "tional", "tion";
     "enci", "ence";
     "anci", "ance";
     "izer", "ize";
     "bli", "ble";
     "alli", "al";
     "entli", "ent";
     "eli", "e";
     "ousli", "ous";
     "ization", "ize";
     "ation", "ate";
     "ator", "ate";
     "alism", "al";
     "iveness", "ive";
     "fulness", "ful";
     "ousness", "ous";
     "aliti", "al";
     "iviti", "ive";
     "biliti", "ble";
     "logi", "log"]
    |> List.map ~f: (fun (suffix, replacement) ->
                       rule (stem_suffix_cond ~suffix
                                              ~stem_f: (Fn.compose ((<) 0) measure))
                            (replace_suffix suffix replacement))
  in step ~word rules

let step_3 word =
  let rules =
    ["icate", "ic";
     "ative", "";
     "alize", "al";
     "iciti", "ic";
     "ical", "ic";
     "ful", "";
     "ness", ""]
    |> List.map ~f: (fun (suffix, replacement) ->
                       rule (stem_suffix_cond ~suffix
                                              ~stem_f: (Fn.compose ((<) 0) measure))
                            (replace_suffix suffix replacement))
  in step ~word rules

let step_4 word =
  let default_condition suffix =
    stem_suffix_cond ~suffix ~stem_f: (Fn.compose ((<) 1) measure) in
  let default_rule suffix =
    rule_with_entry ~entry: (ends_with suffix)
                    (default_condition suffix)
                    (replace_suffix suffix "") in
  let rules =
    List.concat [List.map ~f: default_rule ["al"; "ance"; "ence"; "er"; "ic";
                                            "able"; "ible"; "ant"; "ement";
                                            "ment"; "ent"];
                 [rule (stem_suffix_cond ~suffix: "ion"
                                         ~stem_f: (fun s ->
                                                     (measure s > 1) &&
                                                     (ends_with "s" ~word: s ||
                                                      ends_with "t" ~word: s)))
                       (replace_suffix "ion" "")];
                 List.map ~f: default_rule ["ou"; "ism"; "ate"; "iti"; "ous";
                                            "ive"; "ize"]]
  in step ~word rules

let step_5a word =
  let rules =
    [rule (stem_suffix_cond ~suffix: "e" ~stem_f: (Fn.compose ((<) 1) measure))
          (replace_suffix "e" "");
     rule (stem_suffix_cond ~suffix: "e"
                            ~stem_f: (fun s -> (measure s = 1) && not (ends_cvc s)))
          (replace_suffix "e" "")]
  in step ~word rules

let step_5b word =
  step ~word [rule (fun ~word -> (measure word > 1) && (ends_with ~word "ll"))
                   drop_last]

let run word =
  let word = String.lowercase word in
  if String.length word < 3 then word
  else [step_1a; step_1b; step_1c; step_2; step_3; step_4; step_5a; step_5b]
       |> List.fold ~init: word ~f: (fun w step -> step w)
