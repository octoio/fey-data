open Atd.Import
open Types
open Utils
open Annotations

let generate_json_converter base_name variants _namespace converter_config =
  let base_class_name = to_pascal_case base_name in
  let converter_name = Printf.sprintf "%sConverter" base_class_name in
  let enum_name =
    match converter_config with
    | Some config ->
      (match config.enumname with
       | Some enum_name -> Printf.sprintf "Type.%s" enum_name
       | None -> Printf.sprintf "Type.%s" base_class_name)
    | None -> Printf.sprintf "Type.%s" base_class_name
  in
  let mapper_namespace = "Octoio.Fey.Data.Mapper" in
  let imports =
    [ "Newtonsoft.Json";
      "Newtonsoft.Json.Linq";
      "Octoio.Fey.Data.Dto";
      "Octoio.Fey.Utils"
    ]
  in
  let ns =
    [ (`Namespace, Printf.sprintf "namespace %s" mapper_namespace); open_bracket ]
  in
  let import_lines =
    List.map (fun imp -> (`Import, Printf.sprintf "using %s;" imp)) imports
  in
  let switch_cases =
    variants
    |> List.filter_map (fun variant ->
      match get_variant_info variant with
      | Some (variant_name, type_name) ->
        let class_name = to_pascal_case type_name in
        let enum_case = to_pascal_case variant_name in
        Some (`Field, Printf.sprintf "%s.%s => new %s()," enum_name enum_case class_name)
      | None -> None)
  in
  { lines =
      import_lines
      @ blank_line_concat
      @ ns
      @ [ blank_line;
          (`Class, Printf.sprintf "public class %s : JsonConverter" converter_name);
          open_bracket;
          (`Field, "public override bool CanConvert(System.Type objectType)");
          open_bracket;
          ( `Field,
            Printf.sprintf
              "return typeof(%s).IsAssignableFrom(objectType);"
              base_class_name );
          close_bracket;
          blank_line;
          ( `Field,
            "public override object ReadJson(JsonReader reader, System.Type objectType, \
             object existingValue, JsonSerializer serializer)" );
          open_bracket;
          (`Field, "// Load the JSON into a JObject for inspection.");
          (`Field, "JObject jo = JObject.Load(reader);");
          (`Field, "var type = jo[\"type\"]?.ToString();");
          (`Field, "if (type == null)");
          open_bracket;
          (`Field, "throw new JsonSerializationException(\"Missing 'type' property.\");");
          close_bracket;
          (`Field, Printf.sprintf "var enumType = EEnum.Parse<%s>(type);" enum_name);
          (`Field, Printf.sprintf "var target = enumType switch");
          open_bracket
        ]
      @ switch_cases
      @ [ (`Field, Printf.sprintf "_ => null as %s" base_class_name);
          close_bracket;
          ( `Field,
            Printf.sprintf
              " ?? throw new JsonSerializationException($\"Unknown node type: {type}\");"
          );
          blank_line;
          (`Field, "// Populate the target with the JSON properties.");
          (`Field, "serializer.Populate(jo.CreateReader(), target);");
          (`Field, "return target;");
          close_bracket;
          blank_line;
          ( `Field,
            "public override void WriteJson(JsonWriter writer, object value, \
             JsonSerializer serializer)" );
          open_bracket;
          ( `Field,
            "// For serialization, you can usually defer to the default serializer." );
          (`Field, "serializer.Serialize(writer, value);");
          close_bracket;
          close_bracket;
          blank_line
        ]
      @ close_bracket_concat;
    file_kind = `Class;
    namespace = "Octoio.Fey.Data.Mapper";
    name = converter_name
  }
;;

let generate_converter_generators type_name type_expr annot =
  match type_expr with
  | Atd.Ast.Sum (_, variants, _) ->
    let base_name = extract_base_class_name type_name in
    let converter_config = get_generator_config annot "converter" in
    (match converter_config with
     | Some config ->
       let converter_namespace =
         Option.value config.namespace ~default:"Octoio.Fey.Data.Mapper"
       in
       let converter_name = Option.value config.name ~default:base_name in
       [ generate_json_converter converter_name variants converter_namespace (Some config)
       ]
     | None -> [])
  | _ -> []
;;
