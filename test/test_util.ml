(* Util Module Tests - Comprehensive test suite for utility functions *)

open Alcotest
open Util.String

(* Test fixtures helper module *)
module TestFixtures = struct
  (* Sample strings with different quote patterns *)
  let double_quoted_string = "\"hello world\""
  let single_quoted_string = "'hello world'"
  let mixed_quoted_string = "\"hello 'world'\""
  let no_quotes_string = "hello world"
  let empty_string = ""
  let only_quotes_string = "\"\'\"\'"
  let string_with_internal_quotes = "hello \"quote\" world"

  (* Sample string with prefix for remove_before_prefix tests *)
  let prefix_test_string = "some_prefix_important_data"
  let prefix_string = "prefix"
  let multiprefix_string = "prefix_something_prefix_important"
  let no_prefix_string = "important_data_only"
end

(* Test Suite 1: String Processing - strip_quotes function *)
let test_strip_quotes_double_quotes () =
  let input = TestFixtures.double_quoted_string in
  let expected = "hello world" in
  let result = strip_quotes input in
  check string "strip_quotes should remove double quotes" expected result
;;

let test_strip_quotes_single_quotes () =
  let input = TestFixtures.single_quoted_string in
  let expected = "hello world" in
  let result = strip_quotes input in
  check string "strip_quotes should remove single quotes" expected result
;;

let test_strip_quotes_mixed_quotes () =
  let input = TestFixtures.mixed_quoted_string in
  let expected = "hello world" in
  let result = strip_quotes input in
  check string "strip_quotes should remove all quotes" expected result
;;

let test_strip_quotes_no_quotes () =
  let input = TestFixtures.no_quotes_string in
  let expected = "hello world" in
  let result = strip_quotes input in
  check string "strip_quotes should leave string unchanged when no quotes" expected result
;;

let test_strip_quotes_empty_string () =
  let input = TestFixtures.empty_string in
  let expected = "" in
  let result = strip_quotes input in
  check string "strip_quotes should handle empty string" expected result
;;

let test_strip_quotes_only_quotes () =
  let input = TestFixtures.only_quotes_string in
  let expected = "" in
  let result = strip_quotes input in
  check string "strip_quotes should remove string of only quotes" expected result
;;

let test_strip_quotes_internal_quotes () =
  let input = TestFixtures.string_with_internal_quotes in
  let expected = "hello quote world" in
  let result = strip_quotes input in
  check string "strip_quotes should remove all quote characters" expected result
;;

(* Test Suite 2: Type String Conversion Functions *)
(* Note: These tests require actual enum values from the generated types *)

let test_string_of_entity_type_weapon () =
  (* Test with a weapon entity type *)
  let entity_type = `Weapon in
  let result = Data.Entity_util.string_of_entity_type entity_type in
  (* Result should be a clean string without quotes *)
  check
    bool
    "Data.Entity_util.string_of_entity_type should not contain quotes"
    true
    (not (String.contains result '"' || String.contains result '\''));
  check bool "Data.Entity_util.string_of_entity_type should not be empty" true (String.length result > 0)
;;

let test_string_of_entity_type_character () =
  (* Test with a character entity type *)
  let entity_type = `Character in
  let result = Data.Entity_util.string_of_entity_type entity_type in
  check
    bool
    "Data.Entity_util.string_of_entity_type should not contain quotes"
    true
    (not (String.contains result '"' || String.contains result '\''));
  check bool "Data.Entity_util.string_of_entity_type should not be empty" true (String.length result > 0)
;;

let test_string_of_entity_type_skill () =
  (* Test with a skill entity type *)
  let entity_type = `Skill in
  let result = Data.Entity_util.string_of_entity_type entity_type in
  check
    bool
    "Data.Entity_util.string_of_entity_type should not contain quotes"
    true
    (not (String.contains result '"' || String.contains result '\''));
  check bool "Data.Entity_util.string_of_entity_type should not be empty" true (String.length result > 0)
;;

let test_string_of_entity_type_equipment () =
  (* Test with an equipment entity type *)
  let entity_type = `Equipment in
  let result = Data.Entity_util.string_of_entity_type entity_type in
  check
    bool
    "Data.Entity_util.string_of_entity_type should not contain quotes"
    true
    (not (String.contains result '"' || String.contains result '\''));
  check bool "Data.Entity_util.string_of_entity_type should not be empty" true (String.length result > 0)
;;

let test_string_of_cursor_type_basic () =
  (* Test cursor type conversion *)
  let cursor_type = `General in
  let result = Data.Entity_util.string_of_cursor_type cursor_type in
  check
    bool
    "Data.Entity_util.string_of_cursor_type should not contain quotes"
    true
    (not (String.contains result '"' || String.contains result '\''));
  check bool "Data.Entity_util.string_of_cursor_type should not be empty" true (String.length result > 0)
;;

let test_string_of_stat_type_basic () =
  (* Test stat type conversion *)
  let stat_type = `Health in
  let result = Data.Entity_util.string_of_stat_type stat_type in
  check
    bool
    "Data.Entity_util.string_of_stat_type should not contain quotes"
    true
    (not (String.contains result '"' || String.contains result '\''));
  check bool "Data.Entity_util.string_of_stat_type should not be empty" true (String.length result > 0)
;;

let test_string_of_quality_type_common () =
  (* Test quality type conversion *)
  let quality_type = `Common in
  let result = Data.Entity_util.string_of_quality_type quality_type in
  check
    bool
    "Data.Entity_util.string_of_quality_type should not contain quotes"
    true
    (not (String.contains result '"' || String.contains result '\''));
  check bool "Data.Entity_util.string_of_quality_type should not be empty" true (String.length result > 0)
;;

let test_string_of_quality_type_rare () =
  (* Test another quality type *)
  let quality_type = `Rare in
  let result = Data.Entity_util.string_of_quality_type quality_type in
  check
    bool
    "Data.Entity_util.string_of_quality_type rare should not contain quotes"
    true
    (not (String.contains result '"' || String.contains result '\''));
  check
    bool
    "Data.Entity_util.string_of_quality_type rare should not be empty"
    true
    (String.length result > 0)
;;

(* Test Suite 3: String Manipulation - remove_before_prefix function *)
let test_remove_before_prefix_found () =
  let input = TestFixtures.prefix_test_string in
  let prefix = TestFixtures.prefix_string in
  let result = remove_before_prefix prefix input in
  let expected = "prefix_important_data" in
  check string "remove_before_prefix should remove text before prefix" expected result
;;

let test_remove_before_prefix_not_found () =
  let input = TestFixtures.no_prefix_string in
  let prefix = TestFixtures.prefix_string in
  (* This should raise Not_found exception, let's test that it does *)
  check_raises
    "remove_before_prefix should raise Not_found when prefix not found"
    Not_found
    (fun () -> ignore (remove_before_prefix prefix input))
;;

let test_remove_before_prefix_multiple_occurrences () =
  let input = TestFixtures.multiprefix_string in
  let prefix = TestFixtures.prefix_string in
  let result = remove_before_prefix prefix input in
  (* Should find the first occurrence *)
  let expected = "prefix_something_prefix_important" in
  check string "remove_before_prefix should find first occurrence" expected result
;;

let test_remove_before_prefix_prefix_at_start () =
  let input = "prefix_at_start_data" in
  let prefix = "prefix" in
  let result = remove_before_prefix prefix input in
  let expected = "prefix_at_start_data" in
  check string "remove_before_prefix should work when prefix is at start" expected result
;;

let test_remove_before_prefix_empty_prefix () =
  let input = "some_data" in
  let prefix = "" in
  let result = remove_before_prefix prefix input in
  let expected = "some_data" in
  check string "remove_before_prefix should handle empty prefix" expected result
;;

let test_remove_before_prefix_empty_string () =
  let input = "" in
  let prefix = "prefix" in
  (* This should raise Not_found exception *)
  check_raises
    "remove_before_prefix should raise Not_found for empty string"
    Not_found
    (fun () -> ignore (remove_before_prefix prefix input))
;;

(* Test Suite 4: Integration Tests *)
let test_strip_quotes_integration_with_type_conversion () =
  (* Test that the type conversion functions properly strip quotes *)
  let entity_type = `Weapon in
  let result = Data.Entity_util.string_of_entity_type entity_type in
  (* The result should be clean without any quote characters *)
  let manual_stripped = strip_quotes ("\"" ^ result ^ "\"") in
  check
    string
    "integration: strip_quotes should work with type conversion"
    result
    manual_stripped
;;

let test_type_conversion_consistency () =
  (* Test that all type conversion functions behave consistently *)
  let entity_result = Data.Entity_util.string_of_entity_type `Weapon in
  let quality_result = Data.Entity_util.string_of_quality_type `Common in
  (* All should be non-empty and quote-free *)
  check
    bool
    "entity type result should be non-empty"
    true
    (String.length entity_result > 0);
  check
    bool
    "quality type result should be non-empty"
    true
    (String.length quality_result > 0);
  check
    bool
    "entity type should not contain quotes"
    true
    (not (String.contains entity_result '"' || String.contains entity_result '\''));
  check
    bool
    "quality type should not contain quotes"
    true
    (not (String.contains quality_result '"' || String.contains quality_result '\''))
;;

(* Test Suite 5: Edge Cases and Error Conditions *)
let test_strip_quotes_unicode_quotes () =
  (* Test with Unicode quote characters if any *)
  let input = "\u{201C}hello world\u{201D}" in
  (* Unicode quotes *)
  let result = strip_quotes input in
  (* Should only remove ASCII quotes, so Unicode quotes remain *)
  check
    string
    "strip_quotes should only remove ASCII quotes"
    "\u{201C}hello world\u{201D}"
    result
;;

let test_remove_before_prefix_special_characters () =
  let input = "special@#$prefix%^&data" in
  let prefix = "prefix" in
  let result = remove_before_prefix prefix input in
  let expected = "prefix%^&data" in
  check string "remove_before_prefix should handle special characters" expected result
;;

let test_string_functions_with_various_inputs () =
  (* Test various edge cases for string functions *)
  let newline_string = "hello\nworld" in
  let tab_string = "hello\tworld" in
  let space_string = "hello world" in
  (* strip_quotes should preserve other whitespace *)
  check
    string
    "strip_quotes preserves newlines"
    newline_string
    (strip_quotes newline_string);
  check string "strip_quotes preserves tabs" tab_string (strip_quotes tab_string);
  check string "strip_quotes preserves spaces" space_string (strip_quotes space_string)
;;

(* Test Suite 6: Performance and Boundary Tests *)
let test_strip_quotes_large_string () =
  (* Test with a larger string *)
  let large_content = String.make 1000 'a' in
  let quoted_large = "\"" ^ large_content ^ "\"" in
  let result = strip_quotes quoted_large in
  check string "strip_quotes should handle large strings" large_content result;
  check int "strip_quotes result should be correct length" 1000 (String.length result)
;;

let test_remove_before_prefix_long_prefix () =
  let long_prefix = String.make 50 'p' in
  let input = "start_" ^ long_prefix ^ "_end" in
  let result = remove_before_prefix long_prefix input in
  let expected = long_prefix ^ "_end" in
  check string "remove_before_prefix should handle long prefixes" expected result
;;

(* Test runner *)
let () =
  run
    "Util Module Tests"
    [ ( "String Processing - strip_quotes",
        [ test_case "strip double quotes" `Quick test_strip_quotes_double_quotes;
          test_case "strip single quotes" `Quick test_strip_quotes_single_quotes;
          test_case "strip mixed quotes" `Quick test_strip_quotes_mixed_quotes;
          test_case "no quotes to strip" `Quick test_strip_quotes_no_quotes;
          test_case "empty string" `Quick test_strip_quotes_empty_string;
          test_case "only quotes" `Quick test_strip_quotes_only_quotes;
          test_case "internal quotes" `Quick test_strip_quotes_internal_quotes
        ] );
      ( "Type String Conversion",
        [ test_case "entity type weapon" `Quick test_string_of_entity_type_weapon;
          test_case "entity type character" `Quick test_string_of_entity_type_character;
          test_case "entity type skill" `Quick test_string_of_entity_type_skill;
          test_case "entity type equipment" `Quick test_string_of_entity_type_equipment;
          test_case "cursor type basic" `Quick test_string_of_cursor_type_basic;
          test_case "stat type basic" `Quick test_string_of_stat_type_basic;
          test_case "quality type common" `Quick test_string_of_quality_type_common;
          test_case "quality type rare" `Quick test_string_of_quality_type_rare
        ] );
      ( "String Manipulation - remove_before_prefix",
        [ test_case "prefix found" `Quick test_remove_before_prefix_found;
          test_case "prefix not found" `Quick test_remove_before_prefix_not_found;
          test_case
            "multiple occurrences"
            `Quick
            test_remove_before_prefix_multiple_occurrences;
          test_case "prefix at start" `Quick test_remove_before_prefix_prefix_at_start;
          test_case "empty prefix" `Quick test_remove_before_prefix_empty_prefix;
          test_case "empty string" `Quick test_remove_before_prefix_empty_string
        ] );
      ( "Integration Tests",
        [ test_case
            "strip quotes integration"
            `Quick
            test_strip_quotes_integration_with_type_conversion;
          test_case "type conversion consistency" `Quick test_type_conversion_consistency
        ] );
      ( "Edge Cases and Error Conditions",
        [ test_case "unicode quotes" `Quick test_strip_quotes_unicode_quotes;
          test_case
            "special characters in prefix"
            `Quick
            test_remove_before_prefix_special_characters;
          test_case
            "various whitespace inputs"
            `Quick
            test_string_functions_with_various_inputs
        ] );
      ( "Performance and Boundary Tests",
        [ test_case "large string strip quotes" `Quick test_strip_quotes_large_string;
          test_case "long prefix removal" `Quick test_remove_before_prefix_long_prefix
        ] )
    ]
;;
