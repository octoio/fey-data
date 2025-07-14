(* IO Module Tests - Comprehensive test suite for IO operations *)

open Alcotest
open Gamedata.Io

(* Test fixtures helper module *)
module TestFixtures = struct
  (* Helper to create temporary files for testing *)
  let create_temp_file content =
    let temp_file = Filename.temp_file "test_io" ".txt" in
    let oc = open_out temp_file in
    output_string oc content;
    close_out oc;
    temp_file
  ;;

  (* Helper to create temporary directory for testing *)
  let create_temp_dir () =
    let temp_dir = Filename.temp_file "test_io_dir" "" in
    Sys.remove temp_dir;
    Unix.mkdir temp_dir 0o755;
    temp_dir
  ;;

  (* Helper to check if string contains substring *)
  let string_contains s substr =
    try
      let _ = Str.search_forward (Str.regexp_string substr) s 0 in
      true
    with
    | Not_found -> false
  ;;

  (* Sample content for test files *)
  let sample_file_content = "Hello, World!\nThis is a test file.\nLine 3 content."
  let sample_json_content = {|{"id": "test", "type": "sample", "value": 42}|}
  let multiline_content = "Line 1\nLine 2\nLine 3\nLine 4"

  (* Clean up function *)
  let cleanup_file file_path = if Sys.file_exists file_path then Sys.remove file_path

  let cleanup_dir dir_path =
    if Sys.file_exists dir_path
    then (
      let files = Sys.readdir dir_path in
      Array.iter
        (fun file ->
          let full_path = Filename.concat dir_path file in
          if Sys.file_exists full_path then Sys.remove full_path)
        files;
      Unix.rmdir dir_path)
  ;;
end

(* File Operations Tests *)
module FileOperationTests = struct
  let test_read_file_existing () =
    let temp_file = TestFixtures.create_temp_file TestFixtures.sample_file_content in
    let content = read_file temp_file in
    check string "File content matches" TestFixtures.sample_file_content content;
    TestFixtures.cleanup_file temp_file
  ;;

  let test_read_file_nonexistent () =
    let nonexistent_file = "/path/to/nonexistent/file.txt" in
    try
      let _ = read_file nonexistent_file in
      check bool "Should fail on nonexistent file" false true
    with
    | Sys_error _ -> check bool "Correctly fails on nonexistent file" true true
    | _ -> check bool "Unexpected error type" false true
  ;;

  let test_write_file_new () =
    let temp_path = Filename.temp_file "test_write" ".txt" in
    TestFixtures.cleanup_file temp_path;
    (* Remove the temp file created by temp_file *)
    let test_content = "New file content for testing" in
    write_file temp_path test_content;
    check bool "File was created" true (Sys.file_exists temp_path);
    let written_content = read_file temp_path in
    check string "Written content matches" test_content written_content;
    TestFixtures.cleanup_file temp_path
  ;;

  let test_write_file_overwrite () =
    let temp_file = TestFixtures.create_temp_file "Original content" in
    let new_content = "Overwritten content" in
    write_file temp_file new_content;
    let content = read_file temp_file in
    check string "File content was overwritten" new_content content;
    TestFixtures.cleanup_file temp_file
  ;;

  let test_remove_file_existing () =
    let temp_file = TestFixtures.create_temp_file "Content to be removed" in
    check bool "File exists before removal" true (Sys.file_exists temp_file);
    remove_file temp_file;
    check bool "File was removed" false (Sys.file_exists temp_file)
  ;;

  let test_remove_file_nonexistent () =
    let nonexistent_file = "/path/to/nonexistent/file.txt" in
    (* This should not throw an exception *)
    remove_file nonexistent_file;
    check bool "Remove nonexistent file succeeds" true true
  ;;

  let test_create_file_if_not_exists () =
    let temp_path = Filename.temp_file "test_create" ".txt" in
    TestFixtures.cleanup_file temp_path;
    (* Remove the temp file created by temp_file *)
    create_file_if_not_exists temp_path;
    check bool "File was created" true (Sys.file_exists temp_path);
    let content = read_file temp_path in
    check string "Created content is empty" "" content;
    (* Test that it doesn't overwrite existing file *)
    write_file temp_path "Some content";
    let original_content = read_file temp_path in
    create_file_if_not_exists temp_path;
    let unchanged_content = read_file temp_path in
    check string "File content unchanged" original_content unchanged_content;
    TestFixtures.cleanup_file temp_path
  ;;
end

(* Directory Operations Tests *)
module DirectoryOperationTests = struct
  let test_read_all_files_in_directory () =
    let temp_dir = TestFixtures.create_temp_dir () in
    (* Create test files *)
    let file1 = Filename.concat temp_dir "test1.txt" in
    let file2 = Filename.concat temp_dir "test2.json" in
    let file3 = Filename.concat temp_dir "test3.md" in
    write_file file1 "Content 1";
    write_file file2 TestFixtures.sample_json_content;
    write_file file3 "Markdown content";
    let files = read_all_files_in_directory temp_dir in
    check int "Found all files" 3 (List.length files);
    (* Check that all created files are in the list *)
    let file_names = List.map Filename.basename files in
    check bool "Contains test1.txt" true (List.mem "test1.txt" file_names);
    check bool "Contains test2.json" true (List.mem "test2.json" file_names);
    check bool "Contains test3.md" true (List.mem "test3.md" file_names);
    TestFixtures.cleanup_dir temp_dir
  ;;

  let test_find_all_files_in_directory_with_filter () =
    let temp_dir = TestFixtures.create_temp_dir () in
    (* Create test files with different extensions *)
    let json_file1 = Filename.concat temp_dir "data1.json" in
    let json_file2 = Filename.concat temp_dir "data2.json" in
    let txt_file = Filename.concat temp_dir "readme.txt" in
    let md_file = Filename.concat temp_dir "notes.md" in
    write_file json_file1 TestFixtures.sample_json_content;
    write_file json_file2 {|{"id": "test2", "value": 100}|};
    write_file txt_file "Text content";
    write_file md_file "Markdown content";
    (* Filter for JSON files only *)
    let json_filter file = Filename.check_suffix file ".json" in
    let json_files = find_all_files_in_directory json_filter temp_dir in
    check int "Found only JSON files" 2 (List.length json_files);
    let json_file_names = List.map Filename.basename json_files in
    check bool "Contains data1.json" true (List.mem "data1.json" json_file_names);
    check bool "Contains data2.json" true (List.mem "data2.json" json_file_names);
    check bool "Does not contain txt file" false (List.mem "readme.txt" json_file_names);
    check bool "Does not contain md file" false (List.mem "notes.md" json_file_names);
    TestFixtures.cleanup_dir temp_dir
  ;;

  let test_find_all_files_in_directory_recursive () =
    let temp_dir = TestFixtures.create_temp_dir () in
    (* Create nested directory structure *)
    let sub_dir = Filename.concat temp_dir "subdir" in
    Unix.mkdir sub_dir 0o755;
    let deep_dir = Filename.concat sub_dir "deep" in
    Unix.mkdir deep_dir 0o755;
    (* Create files at different levels *)
    let root_file = Filename.concat temp_dir "root.json" in
    let sub_file = Filename.concat sub_dir "sub.json" in
    let deep_file = Filename.concat deep_dir "deep.json" in
    let non_json = Filename.concat temp_dir "other.txt" in
    write_file root_file {|{"level": "root"}|};
    write_file sub_file {|{"level": "sub"}|};
    write_file deep_file {|{"level": "deep"}|};
    write_file non_json "Not JSON";
    (* Test recursive search for JSON files *)
    let json_filter file = Filename.check_suffix file ".json" in
    let all_json_files = find_all_files_in_directory json_filter temp_dir in
    check int "Found all JSON files recursively" 3 (List.length all_json_files);
    (* Cleanup nested structure *)
    TestFixtures.cleanup_file root_file;
    TestFixtures.cleanup_file sub_file;
    TestFixtures.cleanup_file deep_file;
    TestFixtures.cleanup_file non_json;
    Unix.rmdir deep_dir;
    Unix.rmdir sub_dir;
    TestFixtures.cleanup_dir temp_dir
  ;;
end

(* Text Processing Tests *)
module TextProcessingTests = struct
  let test_read_all_lines () =
    let temp_file = TestFixtures.create_temp_file TestFixtures.multiline_content in
    let lines = read_all_lines temp_file in
    check int "Correct number of lines" 4 (List.length lines);
    check string "First line correct" "Line 1" (List.hd lines);
    check string "Last line correct" "Line 4" (List.hd (List.rev lines));
    let all_lines = List.fold_left ( ^ ) "" lines in
    check
      bool
      "Contains all content"
      true
      (TestFixtures.string_contains all_lines "Line 1");
    check
      bool
      "Contains all content"
      true
      (TestFixtures.string_contains all_lines "Line 4");
    TestFixtures.cleanup_file temp_file
  ;;

  let test_read_all_lines_empty_file () =
    let temp_file = TestFixtures.create_temp_file "" in
    let lines = read_all_lines temp_file in
    check int "Empty file has one empty line" 1 (List.length lines);
    check string "Empty file line is empty" "" (List.hd lines);
    TestFixtures.cleanup_file temp_file
  ;;

  let test_read_all_lines_single_line () =
    let single_line = "Only one line without newline" in
    let temp_file = TestFixtures.create_temp_file single_line in
    let lines = read_all_lines temp_file in
    check int "Single line file" 1 (List.length lines);
    check string "Line content correct" single_line (List.hd lines);
    TestFixtures.cleanup_file temp_file
  ;;
end

(* Color Text Function Tests *)
module ColorTextTests = struct
  let test_color_text_functions () =
    let test_text = "Test message" in
    (* Test that color functions return strings containing the original text *)
    let green_result = green_text test_text in
    let red_result = red_text test_text in
    let yellow_result = yellow_text test_text in
    let blue_result = blue_text test_text in
    check
      bool
      "Green text contains original"
      true
      (TestFixtures.string_contains green_result test_text);
    check
      bool
      "Red text contains original"
      true
      (TestFixtures.string_contains red_result test_text);
    check
      bool
      "Yellow text contains original"
      true
      (TestFixtures.string_contains yellow_result test_text);
    check
      bool
      "Blue text contains original"
      true
      (TestFixtures.string_contains blue_result test_text);
    (* Test that colored text is different from original (has ANSI codes) *)
    check bool "Green text is colored" true (green_result <> test_text);
    check bool "Red text is colored" true (red_result <> test_text);
    check bool "Yellow text is colored" true (yellow_result <> test_text);
    check bool "Blue text is colored" true (blue_result <> test_text)
  ;;

  let test_color_text_empty_string () =
    let empty_text = "" in
    let green_result = green_text empty_text in
    let red_result = red_text empty_text in
    let yellow_result = yellow_text empty_text in
    let blue_result = blue_text empty_text in
    (* Even empty strings should get color codes *)
    check bool "Green empty text has codes" true (String.length green_result > 0);
    check bool "Red empty text has codes" true (String.length red_result > 0);
    check bool "Yellow empty text has codes" true (String.length yellow_result > 0);
    check bool "Blue empty text has codes" true (String.length blue_result > 0)
  ;;
end

(* Print Function Tests - These capture output to test print functions *)
module PrintFunctionTests = struct
  (* Note: These tests verify the functions execute without error.
     In a real application, you might want to capture stdout/stderr to verify output. *)

  let test_print_functions () =
    let test_message = "Test print message" in
    (* Test that print functions execute without throwing exceptions *)
    (try
       print_error test_message;
       check bool "print_error executes" true true
     with
     | _ -> check bool "print_error should not throw" false true);
    (try
       print_success test_message;
       check bool "print_success executes" true true
     with
     | _ -> check bool "print_success should not throw" false true);
    (try
       print_warning test_message;
       check bool "print_warning executes" true true
     with
     | _ -> check bool "print_warning should not throw" false true);
    try
      print_info test_message;
      check bool "print_info executes" true true
    with
    | _ -> check bool "print_info should not throw" false true
  ;;

  let test_print_functions_empty_message () =
    let empty_message = "" in
    (* Test print functions with empty messages *)
    try
      print_error empty_message;
      print_success empty_message;
      print_warning empty_message;
      print_info empty_message;
      check bool "Print functions handle empty messages" true true
    with
    | _ -> check bool "Print functions should handle empty messages" false true
  ;;
end

(* Edge Cases and Integration Tests *)
module EdgeCaseTests = struct
  let test_large_file_operations () =
    let large_content = String.make 10000 'A' in
    (* 10KB of 'A' characters *)
    let temp_file = TestFixtures.create_temp_file large_content in
    let read_content = read_file temp_file in
    check
      int
      "Large file content length"
      (String.length large_content)
      (String.length read_content);
    check string "Large file content matches" large_content read_content;
    TestFixtures.cleanup_file temp_file
  ;;

  let test_special_characters_in_content () =
    let special_content = "Special chars: éñüíóá\n日本語\n🚀🎯\nTabs:\t\tEnd" in
    let temp_file = TestFixtures.create_temp_file special_content in
    let read_content = read_file temp_file in
    check string "Special characters preserved" special_content read_content;
    let lines = read_all_lines temp_file in
    check int "Special content line count" 4 (List.length lines);
    check bool "Contains emoji" true (TestFixtures.string_contains read_content "🚀");
    check bool "Contains Japanese" true (TestFixtures.string_contains read_content "日本語");
    TestFixtures.cleanup_file temp_file
  ;;

  let test_empty_directory_operations () =
    let temp_dir = TestFixtures.create_temp_dir () in
    let files = read_all_files_in_directory temp_dir in
    check int "Empty directory has no files" 0 (List.length files);
    let json_filter file = Filename.check_suffix file ".json" in
    let filtered_files = find_all_files_in_directory json_filter temp_dir in
    check int "Empty directory filtered has no files" 0 (List.length filtered_files);
    TestFixtures.cleanup_dir temp_dir
  ;;
end

(* Integration Tests *)
module IntegrationTests = struct
  let test_file_workflow_integration () =
    (* Test a complete workflow: create, read, modify, remove *)
    let temp_path = Filename.temp_file "integration" ".json" in
    TestFixtures.cleanup_file temp_path;
    let initial_content = TestFixtures.sample_json_content in
    (* Create file *)
    write_file temp_path initial_content;
    check bool "File created in workflow" true (Sys.file_exists temp_path);
    (* Read and verify *)
    let read_content = read_file temp_path in
    check string "Initial content correct" initial_content read_content;
    (* Modify *)
    let modified_content = {|{"id": "modified", "type": "updated", "value": 100}|} in
    write_file temp_path modified_content;
    let updated_content = read_file temp_path in
    check string "Modified content correct" modified_content updated_content;
    (* Process as lines *)
    let lines = read_all_lines temp_path in
    check int "Modified file has content" 1 (List.length lines);
    (* Clean up *)
    remove_file temp_path;
    check bool "File removed in workflow" false (Sys.file_exists temp_path)
  ;;

  let test_directory_processing_integration () =
    (* Test complete directory processing workflow *)
    let temp_dir = TestFixtures.create_temp_dir () in
    (* Create mixed content *)
    let json_files = [ "data1.json"; "config.json"; "entities.json" ] in
    let other_files = [ "readme.txt"; "notes.md" ] in
    List.iter
      (fun file ->
        let path = Filename.concat temp_dir file in
        write_file path ("{\"file\": \"" ^ file ^ "\"}"))
      json_files;
    List.iter
      (fun file ->
        let path = Filename.concat temp_dir file in
        write_file path ("Content of " ^ file))
      other_files;
    (* Process all files *)
    let all_files = read_all_files_in_directory temp_dir in
    check int "All files found" 5 (List.length all_files);
    (* Filter and process JSON files *)
    let json_filter file = Filename.check_suffix file ".json" in
    let json_only = find_all_files_in_directory json_filter temp_dir in
    check int "Only JSON files filtered" 3 (List.length json_only);
    (* Verify each JSON file content *)
    List.iter
      (fun json_file ->
        let content = read_file json_file in
        let filename = Filename.basename json_file in
        check
          bool
          ("JSON file " ^ filename ^ " has correct content")
          true
          (TestFixtures.string_contains content filename))
      json_only;
    TestFixtures.cleanup_dir temp_dir
  ;;
end

(* Main test suite *)
let file_operation_tests =
  [ ("read_file_existing", `Quick, FileOperationTests.test_read_file_existing);
    ("read_file_nonexistent", `Quick, FileOperationTests.test_read_file_nonexistent);
    ("write_file_new", `Quick, FileOperationTests.test_write_file_new);
    ("write_file_overwrite", `Quick, FileOperationTests.test_write_file_overwrite);
    ("remove_file_existing", `Quick, FileOperationTests.test_remove_file_existing);
    ("remove_file_nonexistent", `Quick, FileOperationTests.test_remove_file_nonexistent);
    ( "create_file_if_not_exists",
      `Quick,
      FileOperationTests.test_create_file_if_not_exists )
  ]
;;

let directory_operation_tests =
  [ ( "read_all_files_in_directory",
      `Quick,
      DirectoryOperationTests.test_read_all_files_in_directory );
    ( "find_all_files_with_filter",
      `Quick,
      DirectoryOperationTests.test_find_all_files_in_directory_with_filter );
    ( "find_all_files_recursive",
      `Quick,
      DirectoryOperationTests.test_find_all_files_in_directory_recursive )
  ]
;;

let text_processing_tests =
  [ ("read_all_lines", `Quick, TextProcessingTests.test_read_all_lines);
    ("read_all_lines_empty", `Quick, TextProcessingTests.test_read_all_lines_empty_file);
    ("read_all_lines_single", `Quick, TextProcessingTests.test_read_all_lines_single_line)
  ]
;;

let color_text_tests =
  [ ("color_text_functions", `Quick, ColorTextTests.test_color_text_functions);
    ("color_text_empty", `Quick, ColorTextTests.test_color_text_empty_string)
  ]
;;

let print_function_tests =
  [ ("print_functions", `Quick, PrintFunctionTests.test_print_functions);
    ( "print_functions_empty",
      `Quick,
      PrintFunctionTests.test_print_functions_empty_message )
  ]
;;

let edge_case_tests =
  [ ("large_file_operations", `Quick, EdgeCaseTests.test_large_file_operations);
    ("special_characters", `Quick, EdgeCaseTests.test_special_characters_in_content);
    ("empty_directory", `Quick, EdgeCaseTests.test_empty_directory_operations)
  ]
;;

let integration_tests =
  [ ("file_workflow_integration", `Quick, IntegrationTests.test_file_workflow_integration);
    ( "directory_processing_integration",
      `Quick,
      IntegrationTests.test_directory_processing_integration )
  ]
;;

(* Test runner *)
let () =
  run
    "IO Module Tests"
    [ ("File Operations", file_operation_tests);
      ("Directory Operations", directory_operation_tests);
      ("Text Processing", text_processing_tests);
      ("Color Text Functions", color_text_tests);
      ("Print Functions", print_function_tests);
      ("Edge Cases", edge_case_tests);
      ("Integration Tests", integration_tests)
    ]
;;
