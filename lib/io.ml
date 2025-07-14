let color_text color text = color ^ text ^ "\027[0m"
let green_text = color_text "\027[32m"
let red_text = color_text "\027[31m"
let yellow_text = color_text "\027[33m"
let blue_text = color_text "\027[34m"
let print_error error = print_endline @@ red_text error
let print_success success = print_endline @@ green_text success
let print_warning warning = print_endline @@ yellow_text warning
let print_info info = print_endline @@ blue_text info

let read_file filename =
  let channel = open_in filename in
  try
    let length = in_channel_length channel in
    let content = really_input_string channel length in
    close_in channel;
    content
  with
  | e ->
    close_in channel;
    raise e
;;

let read_all_lines filename = read_file filename |> String.split_on_char '\n'

let write_file filename content =
  let channel = open_out filename in
  output_string channel content;
  close_out channel
;;

let remove_file filename = if Sys.file_exists filename then Sys.remove filename

let read_all_files_in_directory dir =
  let entries = Array.to_list @@ Sys.readdir dir in
  List.map (fun entry -> Filename.concat dir entry) entries
;;

let rec find_all_files_in_directory f dir =
  let entries = Array.to_list @@ Sys.readdir dir in
  List.fold_left
    (fun acc entry ->
      let full_path = Filename.concat dir entry in
      if Sys.is_directory full_path
      then find_all_files_in_directory f full_path @ acc
      else if f full_path
      then full_path :: acc
      else acc)
    []
    entries
;;

let create_file_if_not_exists filename =
  if not @@ Sys.file_exists filename then write_file filename ""
;;
