(* The data stamp (lib/data_stamp.ml). The fixture and its expected hex are spelled the same in
   fey-rs (crates/fey_core/tests/data_stamp.rs): the two sides must agree. *)

open Alcotest

let write path content =
  let channel = open_out_bin path in
  output_string channel content;
  close_out channel
;;

let rec mkdir_p dir =
  if not (Sys.file_exists dir)
  then (
    mkdir_p (Filename.dirname dir);
    Sys.mkdir dir 0o755)
;;

let counter = ref 0

(* A fresh folder holding the three fixture files, changed by [tweak] *)
let fixture ?(tweak = fun _ -> ()) () =
  incr counter;
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "stamp-fixture-%d-%d" (Unix.getpid ()) !counter)
  in
  mkdir_p (Filename.concat dir "sub/deep");
  write (Filename.concat dir "a.json") "{}";
  write (Filename.concat dir "sub/b.txt") "hello\n";
  write (Filename.concat dir "sub/deep/c.json") "{\"k\":1}";
  tweak dir;
  dir
;;

(* The same value as `FIXTURE_STAMP` in fey-rs's data_stamp.rs *)
let expected = "ce393fb633163bec"

let test_fixture_stamp () =
  check string "the shared fixture" expected (Gamedata.Data_stamp.of_directory (fixture ()))
;;

let test_a_changed_byte_a_rename_and_an_added_file_each_change_it () =
  let stamp tweak = Gamedata.Data_stamp.of_directory (fixture ~tweak ()) in
  let base = stamp (fun _ -> ()) in
  let differs name s = check bool name true (s <> base) in
  differs "a changed byte" (stamp (fun d -> write (Filename.concat d "a.json") "{ }"));
  differs "a renamed file" (stamp (fun d -> Sys.rename (Filename.concat d "a.json") (Filename.concat d "z.json")));
  differs "an added file" (stamp (fun d -> write (Filename.concat d "sub/new.json") ""));
  differs "a moved file" (stamp (fun d -> Sys.rename (Filename.concat d "sub/b.txt") (Filename.concat d "b.txt")))
;;

let test_the_order_of_creation_does_not_matter () =
  let in_reverse d =
    Sys.remove (Filename.concat d "a.json");
    write (Filename.concat d "a.json") "{}"
  in
  check string "same stamp" expected (Gamedata.Data_stamp.of_directory (fixture ~tweak:in_reverse ()))
;;

let () =
  run
    "Data stamp"
    [ ( "stamp",
        [ test_case "fixture" `Quick test_fixture_stamp;
          test_case "changes" `Quick test_a_changed_byte_a_rename_and_an_added_file_each_change_it;
          test_case "creation order" `Quick test_the_order_of_creation_does_not_matter
        ] )
    ]
;;
