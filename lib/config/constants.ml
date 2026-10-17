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

(* Configuration constants: every path is relative to the fey-data folder *)
let json_folder = read_config_value "json_path" "../fey-rs/data/json"

(* The media files (images, audio, models) the entity JSON names; validation checks they exist *)
let media_folder = read_config_value "media_path" "../fey-assets/game-media"

let entity_reference_indices_file =
  read_config_value "entity_index_path" "../fey-rs/data/entity-reference-indices.json"
;;

let rust_output_path =
  read_config_value "rust_output_path" "../fey-rs/crates/fey_data/src/generated"
;;

(* Dataset paths are kept from the first "/json" on *)
let json_sub_folder = "/json"
