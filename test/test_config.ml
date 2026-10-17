(* Config Module Tests - the paths of fey-data.config and their defaults *)

open Alcotest
open Config.Constants

(* Paths are compared with '/' separators so the checks do not depend on the platform *)
let normalize path = Str.global_replace (Str.regexp "\\\\") "/" path

let test_json_folder () =
  check string "json folder" "../fey-rs/data/json" (normalize json_folder)
;;

let test_media_folder () =
  check string "media folder" "../fey-assets/game-media" (normalize media_folder)
;;

let test_entity_reference_indices_file () =
  check
    string
    "index file"
    "../fey-rs/data/entity-reference-indices.json"
    (normalize entity_reference_indices_file);
  check
    bool
    "index file has a .json extension"
    true
    (Filename.check_suffix entity_reference_indices_file ".json")
;;

let test_rust_output_path () =
  check
    string
    "rust output"
    "../fey-rs/crates/fey_data/src/generated"
    (normalize rust_output_path)
;;

(* The index sits next to the JSON folder, where the Rust round-trip test reads it *)
let test_index_next_to_json () =
  check
    string
    "index parent is the json parent"
    (Filename.dirname json_folder)
    (Filename.dirname entity_reference_indices_file)
;;

(* Dataset paths are cut at the first "/json" of the file path *)
let test_json_sub_folder () =
  check string "json_sub_folder" "/json" json_sub_folder;
  check
    string
    "json folder ends with the sub folder name"
    "json"
    (Filename.basename json_folder)
;;

let () =
  run
    "Config Module Tests"
    [ ( "Paths",
        [ test_case "json folder" `Quick test_json_folder;
          test_case "media folder" `Quick test_media_folder;
          test_case
            "entity reference indices file"
            `Quick
            test_entity_reference_indices_file;
          test_case "rust output path" `Quick test_rust_output_path;
          test_case "index next to json" `Quick test_index_next_to_json;
          test_case "json sub folder" `Quick test_json_sub_folder
        ] )
    ]
;;
