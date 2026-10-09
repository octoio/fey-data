open Types

let rec mkdir_p dir =
  if dir <> "" && dir <> "." && dir <> "/" && not (Sys.file_exists dir)
  then (
    mkdir_p (Filename.dirname dir);
    Sys.mkdir dir 0o755)
;;

(* The output directory is owned by the generator: stale .rs files are removed first *)
let clean_directory dir =
  if Sys.file_exists dir
  then
    Sys.readdir dir
    |> Array.iter (fun f ->
      if Filename.check_suffix f ".rs" then Sys.remove (Filename.concat dir f))
;;

let write_rs_files (dir : string) (files : rs_file list) : string list =
  mkdir_p dir;
  clean_directory dir;
  List.map
    (fun { name; content } ->
      let path = Filename.concat dir (name ^ ".rs") in
      Gamedata.Io.print_info @@ Printf.sprintf "Writing to file: %s" path;
      Gamedata.Io.write_file path content;
      path)
    files
;;
