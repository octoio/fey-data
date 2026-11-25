(* The data stamp: what `gamedata` ran against. FNV-1a 64 over every file under the data folder, in
   sorted relative-path order (paths written with `/`): the path, a newline, the file's raw bytes, a
   newline. It answers "did `gamedata` run on these bytes"; it is not the sim's data fingerprint
   (`GameData::fingerprint` in fey-rs, a hash of the parsed gameplay entities that answers "were the
   baselines made from this data" and ignores achievements). `gamedata` writes it beside the index
   once validation, coverage and the index passed; a fey-rs test recomputes it. The Rust side is
   `fey_core::data::data_stamp`: the same definition, one fixture of three files in both test suites. *)

let fnv_offset = 0xcbf29ce484222325L
let fnv_prime = 0x100000001b3L

let absorb hash text =
  String.fold_left
    (fun h c -> Int64.mul (Int64.logxor h (Int64.of_int (Char.code c))) fnv_prime)
    hash
    text
;;

let read_bytes path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel (in_channel_length channel))
;;

(* The files under [dir] as (path relative to [dir] with `/`, absolute path), sorted by the former *)
let files dir =
  let prefix = String.length dir + 1 in
  Io.find_all_files_in_directory (fun _ -> true) dir
  |> List.map (fun path -> (String.sub path prefix (String.length path - prefix), path))
  |> List.sort compare
;;

(* Sixteen lowercase hex digits *)
let of_directory dir =
  let dir = if String.ends_with ~suffix:"/" dir then String.sub dir 0 (String.length dir - 1) else dir in
  let hash =
    List.fold_left
      (fun h (relative, path) -> absorb (absorb (absorb (absorb h relative) "\n") (read_bytes path)) "\n")
      fnv_offset
      (files dir)
  in
  Printf.sprintf "%016Lx" hash
;;
