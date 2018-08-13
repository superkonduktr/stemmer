open Core.Std
open OUnit2

let read_data path =
  let file = In_channel.create path in
  let lines = In_channel.input_lines file in
  In_channel.close file;
  lines

let main _ =
  let vocabulary, output = read_data "test/test_data/voc.txt", read_data "test/test_data/output.txt" in
  List.map2_exn vocabulary output ~f: (fun v o -> v, o) |>
  List.iter ~f: (fun (v, o) -> assert_equal (Stemmer.stem v) o)

let () =
  run_test_tt_main ("suite" >::: ["main" >:: main])
