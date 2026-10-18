(* Checks of the portrait metadata (schema: lib/data/portrait.atd). The schema reads the shape
   (strict fields, types, simple ranges); the checks here are the ones that relate several fields:
   rects inside their atlas and cell, slots naming existing layers, every layer having every frame. *)

open Data.Portrait_t

let errorf errors fmt = Printf.ksprintf (fun s -> errors := s :: !errors) fmt

(* ---- helpers ------------------------------------------------------------ *)

let check_rect errors ~where ~sheet:(sw, sh) ?cell (x, y, w, h, ox, oy) =
  if w <= 0 || h <= 0
  then errorf errors "%s: rect %dx%d is empty" where w h
  else if x < 0 || y < 0 || x + w > sw || y + h > sh
  then errorf errors "%s: rect [%d,%d,%d,%d] lies outside the %dx%d atlas" where x y w h sw sh;
  match cell with
  | Some (cw, ch) when ox < 0 || oy < 0 || ox + w > cw || oy + h > ch ->
    errorf
      errors
      "%s: offset [%d,%d] with size %dx%d lies outside the %dx%d cell"
      where
      ox
      oy
      w
      h
      cw
      ch
  | _ -> ()
;;

let in_cell errors ~where (cw, ch) (px, py) =
  if cw <= 0 || ch <= 0
  then errorf errors "%s: cell_px %dx%d is empty" where cw ch
  else if px < 0. || py < 0. || px > float_of_int cw || py > float_of_int ch
  then errorf errors "%s: pivot_px lies outside the %dx%d cell" where cw ch
;;

let check_slots errors ~where ~layer_exists slots =
  List.iter
    (fun (slot, ({ layers; _ } : portrait_slot)) ->
      List.iter
        (fun (variant, layer) ->
          if not (layer_exists layer)
          then errorf errors "%s: slot %s variant %s names the missing layer %s" where slot variant layer)
        layers)
    slots
;;

let check_portrait errors name (p : portrait) =
  let where = "portrait " ^ name in
  in_cell errors ~where p.cell_px p.pivot_px;
  List.iter
    (fun (id, (l : portrait_layer)) ->
      let lwhere = Printf.sprintf "%s layer %s" where id in
      if List.length l.frames <> p.frames
      then errorf errors "%s: %d frames, the portrait says %d" lwhere (List.length l.frames) p.frames;
      List.iteri
        (fun f rect ->
          check_rect
            errors
            ~where:(Printf.sprintf "%s frame %d" lwhere f)
            ~sheet:l.size
            ~cell:p.cell_px
            rect)
        l.frames)
    p.layers;
  check_slots errors ~where ~layer_exists:(fun l -> List.mem_assoc l p.layers) p.slots
;;

let check_portraits_meta (m : portrait_meta) : string list =
  let errors = ref [] in
  check_portrait errors "full" m.full;
  check_portrait errors "frame" m.frame;
  List.rev !errors
;;

(* ---- entry points -------------------------------------------------------- *)

let validation_errors = function
  | None -> []
  | Some e -> [ Atdgen_runtime.Util.Validation.string_of_error e ]
;;

let read_then parse validate check text =
  match parse text with
  | exception Yojson.Json_error msg -> [ "invalid JSON: " ^ msg ]
  | exception Atdgen_runtime.Oj_run.Error msg -> [ "does not match the schema: " ^ msg ]
  | exception Failure msg -> [ "does not match the schema: " ^ msg ]
  | value ->
    (match validation_errors (validate [] value) with
     | [] -> check value
     | errors -> errors)
;;

let check_portraits_string text =
  read_then
    Data.Portrait_j.portrait_meta_of_string
    Data.Portrait_v.validate_portrait_meta
    check_portraits_meta
    text
;;

let is_meta_file path = Filename.basename path = "portraits.json"

let check_file path =
  if is_meta_file path
  then check_portraits_string (Io.read_file path)
  else [ Filename.basename path ^ ": expected a file named portraits.json" ]
;;

(* Every portraits.json under [path] (or [path] itself), sorted, with its errors *)
let check_path path : (string * string list) list =
  let files =
    if Sys.is_directory path
    then Io.find_all_files_in_directory is_meta_file path
    else [ path ]
  in
  List.sort compare files |> List.map (fun f -> f, check_file f)
;;
