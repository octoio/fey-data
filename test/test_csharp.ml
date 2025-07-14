open Alcotest

(* Import the modular csharp library *)
module CsharpUtils = Csharp.Utils
module CsharpMain = Csharp.Main
module CsharpOutput = Csharp.Output
module CsharpTypes = Csharp.Types
module CsharpGeneratorTypes = Csharp.Generator_types

(* Test fixtures and helper functions *)
module TestFixtures = struct
  (* Sample ATD content for testing *)
  let sample_enum_atd =
    {|
type sample_enum <cs namespace="Test.Enums" name="SampleEnum"> = [
  | Value1 <json name="value1">
  | Value2 <json name="value2">
  | Value3 <json name="value3">
]
|}
  ;;

  let sample_record_atd =
    {|
type sample_record <cs namespace="Test.Records" name="SampleRecord" style="class"> = {
  id : string <json name="id">;
  name : string <json name="name">;
  value : int <json name="value">;
}
|}
  ;;

  let sample_struct_atd =
    {|
type position <cs namespace="Test.Structs" name="Position" style="struct"> = {
  x : float <json name="x">;
  y : float <json name="y">;
}
|}
  ;;

  (* Create temporary ATD file for testing *)
  let create_temp_atd_file content =
    let temp_file = Filename.temp_file "test_csharp" ".atd" in
    let oc = open_out temp_file in
    output_string oc content;
    close_out oc;
    temp_file
  ;;

  (* Helper to check if string contains substring *)
  let string_contains s substr =
    try
      let _ = Str.search_forward (Str.regexp_string substr) s 0 in
      true
    with
    | Not_found -> false
  ;;
end

(* Utility Function Tests *)
module UtilityTests = struct
  let test_is_primitive_type () =
    check bool "int is primitive" true (CsharpUtils.is_primitive_type "int");
    check bool "float is primitive" true (CsharpUtils.is_primitive_type "float");
    check bool "string is primitive" true (CsharpUtils.is_primitive_type "string");
    check bool "bool is primitive" true (CsharpUtils.is_primitive_type "bool");
    check bool "uint is primitive" true (CsharpUtils.is_primitive_type "uint");
    check bool "custom type is not primitive" false (CsharpUtils.is_primitive_type "CustomType");
    check bool "case insensitive - INT is primitive" true (CsharpUtils.is_primitive_type "INT");
    check bool "case insensitive - STRING is primitive" true (CsharpUtils.is_primitive_type "STRING")
  ;;

  let test_to_pascal_case () =
    check string "simple word" "Test" (CsharpUtils.to_pascal_case "test");
    check string "underscore separated" "TestCase" (CsharpUtils.to_pascal_case "test_case");
    check
      string
      "multiple underscores"
      "TestCaseExample"
      (CsharpUtils.to_pascal_case "test_case_example");
    check string "already pascal case" "TestCase" (CsharpUtils.to_pascal_case "TestCase");
    check string "empty string" "" (CsharpUtils.to_pascal_case "");
    check string "single character" "A" (CsharpUtils.to_pascal_case "a")
  ;;

  let test_to_camel_case () =
    check string "simple word" "test" (CsharpUtils.to_camel_case "test");
    check string "underscore separated" "testCase" (CsharpUtils.to_camel_case "test_case");
    check
      string
      "multiple underscores"
      "testCaseExample"
      (CsharpUtils.to_camel_case "test_case_example");
    check string "already camel case" "testCase" (CsharpUtils.to_camel_case "testCase");
    check string "empty string" "" (CsharpUtils.to_camel_case "");
    check string "single character" "a" (CsharpUtils.to_camel_case "a");
    check string "starts with uppercase" "TestCase" (CsharpUtils.to_camel_case "Test_case")
  ;;
end

(* ATD Parsing Tests *)
module AtdParsingTests = struct
  let test_parse_atd_file_valid () =
    let temp_file = TestFixtures.create_temp_atd_file TestFixtures.sample_enum_atd in
    let result = CsharpUtils.parse_atd_file temp_file in
    let _module_head, module_body = result in
    check bool "parsing succeeds" true (List.length module_body > 0);
    Sys.remove temp_file
  ;;

  let test_parse_atd_file_invalid () =
    let invalid_content = "invalid atd syntax {]" in
    let temp_file = TestFixtures.create_temp_atd_file invalid_content in
    (try
       let _ = CsharpUtils.parse_atd_file temp_file in
       check bool "should fail on invalid syntax" false true
     with
     | _ -> check bool "correctly fails on invalid syntax" true true);
    Sys.remove temp_file
  ;;

  let test_parse_atd_file_nonexistent () =
    try
      let _ = CsharpUtils.parse_atd_file "/nonexistent/file.atd" in
      check bool "should fail on missing file" false true
    with
    | Sys_error _ -> check bool "correctly fails on missing file" true true
    | _ -> check bool "unexpected error type" false true
  ;;
end

(* Integration Tests using real ATD parsing *)
module IntegrationTests = struct
  let test_generate_enum_from_atd () =
    let temp_file = TestFixtures.create_temp_atd_file TestFixtures.sample_enum_atd in
    let parsed = CsharpUtils.parse_atd_file temp_file in
    let _module_head, module_body = parsed in
    let enums = CsharpMain.generate_csharp_enum_from_module module_body in
    check bool "generates enum" true (List.length enums > 0);
    let enum_code =
      match enums with
      | file :: _ -> CsharpOutput.string_of_csharp_code file.lines
      | [] -> ""
    in
    check
      bool
      "contains enum declaration"
      true
      (TestFixtures.string_contains enum_code "public enum");
    check
      bool
      "contains namespace"
      true
      (TestFixtures.string_contains enum_code "namespace");
    Sys.remove temp_file
  ;;

  let test_generate_struct_from_atd () =
    let temp_file = TestFixtures.create_temp_atd_file TestFixtures.sample_record_atd in
    let parsed = CsharpUtils.parse_atd_file temp_file in
    let _module_head, module_body = parsed in
    let structs = CsharpMain.generate_csharp_struct module_body in
    check bool "generates struct" true (List.length structs > 0);
    let struct_code =
      match structs with
      | file :: _ -> CsharpOutput.string_of_csharp_code file.lines
      | [] -> ""
    in
    check
      bool
      "contains class declaration"
      true
      (TestFixtures.string_contains struct_code "public class");
    check
      bool
      "contains namespace"
      true
      (TestFixtures.string_contains struct_code "namespace");
    Sys.remove temp_file
  ;;

  let test_generate_all_files () =
    let temp_file = TestFixtures.create_temp_atd_file TestFixtures.sample_struct_atd in
    let parsed = CsharpUtils.parse_atd_file temp_file in
    let _module_head, module_body = parsed in
    let all_files = CsharpMain.generate_csharp [ module_body ] in
    check bool "generates files" true (List.length all_files > 0);
    Sys.remove temp_file
  ;;

  let test_generate_enum_with_namespace () =
    let temp_file = TestFixtures.create_temp_atd_file TestFixtures.sample_enum_atd in
    let parsed = CsharpUtils.parse_atd_file temp_file in
    let _module_head, module_body = parsed in
    let enums = CsharpMain.generate_csharp_enum_from_module module_body in
    let enum_file = List.hd enums in
    check string "enum has correct namespace" "Test.Enums" enum_file.namespace;
    check string "enum has correct name" "SampleEnum" enum_file.name;
    check
      (of_pp (fun fmt kind ->
         Format.fprintf
           fmt
           "%s"
           (match kind with
            | `Enum -> "Enum"
            | `Struct -> "Struct"
            | `Class -> "Class"
            | `Unknown -> "Unknown")))
      "file kind"
      `Enum
      enum_file.file_kind;
    Sys.remove temp_file
  ;;

  let test_generate_struct_with_namespace () =
    let temp_file = TestFixtures.create_temp_atd_file TestFixtures.sample_record_atd in
    let parsed = CsharpUtils.parse_atd_file temp_file in
    let _module_head, module_body = parsed in
    let structs = CsharpMain.generate_csharp_struct module_body in
    let struct_file = List.hd structs in
    check string "struct has correct namespace" "Test.Records" struct_file.namespace;
    check string "struct has correct name" "SampleRecord" struct_file.name;
    check
      (of_pp (fun fmt kind ->
         Format.fprintf
           fmt
           "%s"
           (match kind with
            | `Enum -> "Enum"
            | `Struct -> "Struct"
            | `Class -> "Class"
            | `Unknown -> "Unknown")))
      "file kind"
      `Class
      struct_file.file_kind;
    Sys.remove temp_file
  ;;
end

(* File Output Tests *)
module FileOutputTests = struct
  let test_indent_line () =
    let test_line = (`Field, "public string Name { get; set; }") in
    let indented = CsharpOutput.indent_line 1 test_line in
    check string "single indentation" "    public string Name { get; set; }" indented;
    let double_indented = CsharpOutput.indent_line 2 test_line in
    check
      string
      "double indentation"
      "        public string Name { get; set; }"
      double_indented;
    let blank_line = (`Blank, "") in
    let indented_blank = CsharpOutput.indent_line 1 blank_line in
    check string "blank line indentation" "" indented_blank
  ;;

  let test_string_of_csharp_code () =
    let test_lines =
      [ (`Namespace, "namespace Test");
        (`Bracket, "{");
        (`Class, "public class TestClass");
        (`Bracket, "{");
        (`Field, "public string Name { get; set; }");
        (`Bracket, "}");
        (`Bracket, "}")
      ]
    in
    let code = CsharpOutput.string_of_csharp_code test_lines in
    check
      bool
      "contains namespace"
      true
      (TestFixtures.string_contains code "namespace Test");
    check
      bool
      "contains class"
      true
      (TestFixtures.string_contains code "public class TestClass");
    check
      bool
      "contains field"
      true
      (TestFixtures.string_contains code "public string Name { get; set; }");
    check
      bool
      "properly indented"
      true
      (TestFixtures.string_contains code "    public class TestClass")
  ;;

  let test_remove_base_namespace () =
    let result1 = CsharpOutput.remove_base_namespace "Octoio.Fey.Data" "Octoio.Fey.Data.Dto" in
    check string "removes base namespace" "Dto" result1;
    let result2 = CsharpOutput.remove_base_namespace "Octoio.Fey" "Octoio.Fey.Data.Type" in
    check string "removes partial base namespace" "Data/Type" result2;
    let result3 = CsharpOutput.remove_base_namespace "Different.Base" "Octoio.Fey.Data" in
    check string "no common base" "Octoio/Fey/Data" result3;
    let result4 =
      CsharpOutput.remove_base_namespace "Octoio.Fey.Data" "namespace Octoio.Fey.Data.Dto"
    in
    check string "handles namespace prefix" "Dto" result4
  ;;
end

(* Test runner *)
let () =
  let utility_tests =
    [ ("is_primitive_type", `Quick, UtilityTests.test_is_primitive_type);
      ("to_pascal_case", `Quick, UtilityTests.test_to_pascal_case);
      ("to_camel_case", `Quick, UtilityTests.test_to_camel_case)
    ]
  in
  let atd_parsing_tests =
    [ ("parse_atd_file_valid", `Quick, AtdParsingTests.test_parse_atd_file_valid);
      ("parse_atd_file_invalid", `Quick, AtdParsingTests.test_parse_atd_file_invalid);
      ( "parse_atd_file_nonexistent",
        `Quick,
        AtdParsingTests.test_parse_atd_file_nonexistent )
    ]
  in
  let integration_tests =
    [ ("generate_enum_from_atd", `Quick, IntegrationTests.test_generate_enum_from_atd);
      ("generate_struct_from_atd", `Quick, IntegrationTests.test_generate_struct_from_atd);
      ("generate_all_files", `Quick, IntegrationTests.test_generate_all_files);
      ( "generate_enum_with_namespace",
        `Quick,
        IntegrationTests.test_generate_enum_with_namespace );
      ( "generate_struct_with_namespace",
        `Quick,
        IntegrationTests.test_generate_struct_with_namespace )
    ]
  in
  let file_output_tests =
    [ ("indent_line", `Quick, FileOutputTests.test_indent_line);
      ("string_of_csharp_code", `Quick, FileOutputTests.test_string_of_csharp_code);
      ("remove_base_namespace", `Quick, FileOutputTests.test_remove_base_namespace)
    ]
  in
  run
    "C# Code Generation Tests"
    [ ("Utility Functions", utility_tests);
      ("ATD Parsing", atd_parsing_tests);
      ("Integration Tests", integration_tests);
      ("File Output", file_output_tests)
    ]
;;
