(* Configuration file reader *)
let read_config_value key default_value =
  try
    let config_file = "fey-data.config" in
    let ic = open_in config_file in
    let rec find_key () =
      try
        let line = input_line ic in
        let trimmed = String.trim line in
        if String.length trimmed > 0 && trimmed.[0] <> '#' then
          match String.split_on_char '=' trimmed with
          | [k; v] when String.trim k = key -> Some (String.trim v |> fun s -> if String.length s > 1 && s.[0] = '"' && s.[String.length s - 1] = '"' then String.sub s 1 (String.length s - 2) else s)
          | _ -> find_key ()
        else
          find_key ()
      with End_of_file -> None
    in
    let result = find_key () in
    close_in ic;
    match result with
    | Some value -> value
    | None -> default_value
  with
  | Sys_error _ -> default_value
;;

(* Configuration constants *)
let game_root = read_config_value "game_root" "../fey-game-mock"
let streaming_assets_path = read_config_value "streaming_assets_path" "Assets/StreamingAssets"
let json_path = read_config_value "json_path" "json"
let scripts_path = read_config_value "scripts_path" "Assets/Scripts/Octoio/Fey/Data/Dto"

(* Derived paths *)
let base_asset_folder = Filename.concat game_root "Assets"
let streaming_assets_folder = Filename.concat game_root streaming_assets_path
let base_script_folder = Filename.concat game_root "Assets/Scripts"
let json_folder = Filename.concat streaming_assets_folder json_path
let script_output_folder = Filename.concat game_root scripts_path

(* Legacy constants *)
let json_sub_folder = "/json"
let generated_file = "generated.txt"
let base_namespace = "Octoio.Fey"
let data_folder = Filename.concat streaming_assets_folder "/data"

let entity_reference_indices_file =
  Filename.concat streaming_assets_folder "entity-reference-indices.json"
;;