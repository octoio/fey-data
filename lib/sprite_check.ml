(* Checks of the sprite pipeline metadata (schema: lib/data/sprite.atd). The schema reads the shape
   (strict fields, types, simple ranges); the checks here are the ones that relate several fields:
   rects inside their sheet, every listed animation having a sheet, phases covering the clip, ... *)

open Data.Sprite_t

let errorf errors fmt = Printf.ksprintf (fun s -> errors := s :: !errors) fmt

(* ---- helpers ------------------------------------------------------------ *)

(* "head.bare.idle" -> ("head.bare", "idle"): animation ids hold no dot, layer ids may *)
let split_sheet_id id =
  match String.rindex_opt id '.' with
  | Some i -> Some (String.sub id 0 i, String.sub id (i + 1) (String.length id - i - 1))
  | None -> None
;;

let check_rect errors ~where ~sheet:(sw, sh) ?cell (x, y, w, h, ox, oy) =
  if w <= 0 || h <= 0
  then errorf errors "%s: rect %dx%d is empty" where w h
  else if x < 0 || y < 0 || x + w > sw || y + h > sh
  then errorf errors "%s: rect [%d,%d,%d,%d] lies outside the %dx%d sheet" where x y w h sw sh;
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

let check_slots errors ~where ~layer_exists ?slot_of slots =
  List.iter
    (fun (slot, ({ layers; _ } : sprite_slot)) ->
      List.iter
        (fun (variant, layer) ->
          if not (layer_exists layer)
          then errorf errors "%s: slot %s variant %s names the missing layer %s" where slot variant layer
          else (
            match slot_of with
            | Some f when f layer <> Some slot ->
              errorf errors "%s: layer %s is listed in slot %s but says another" where layer slot
            | _ -> ()))
        layers)
    slots
;;

(* ---- sprites.json -------------------------------------------------------- *)

let check_animation errors name (a : sprite_animation) =
  let where = "animation " ^ name in
  (match a.impact_frame, a.impact_time, a.phases with
   | None, None, None -> ()
   | Some frame, Some time, Some { windup = w0, w1; impact = i0, i1; recovery = r0, r1 } ->
     if time > a.duration +. 1e-9
     then errorf errors "%s: impact_time %g is past the duration %g" where time a.duration;
     if w0 <> 0 || w1 + 1 <> i0 || i1 + 1 <> r0 || r1 <> a.frames - 1 || w1 < w0 || i1 < i0 || r1 < r0
     then
       errorf
         errors
         "%s: phases [%d,%d] [%d,%d] [%d,%d] must follow one another and cover the %d frames"
         where
         w0
         w1
         i0
         i1
         r0
         r1
         a.frames
     else if frame < i0 || frame > i1
     then errorf errors "%s: impact_frame %d is outside the impact phase [%d,%d]" where frame i0 i1
   | _ -> errorf errors "%s: impact_time, impact_frame and phases come together" where)
;;

let check_sprites_meta (m : sprite_meta) : string list =
  let errors = ref [] in
  let animations = m.animations and layers = m.layers and sheets = m.sheets in
  let animation id = List.assoc_opt id animations in
  List.iter (fun (name, a) -> check_animation errors name a) animations;
  in_cell errors ~where:"sprites" m.cell_px (float_of_int (fst m.pivot_px), float_of_int (snd m.pivot_px));
  (* sheets *)
  List.iter
    (fun (id, (s : sprite_sheet)) ->
      let where = "sheet " ^ id in
      match split_sheet_id id with
      | None -> errorf errors "%s: the id is not <layer>.<animation>" where
      | Some (layer, anim) ->
        (match List.assoc_opt layer layers, animation anim with
         | None, _ -> errorf errors "%s: the layer %s does not exist" where layer
         | _, None -> errorf errors "%s: the animation %s does not exist" where anim
         | Some l, Some a ->
           if not (List.mem anim l.animations)
           then errorf errors "%s: layer %s does not list the animation %s" where layer anim;
           if List.length s.frames <> m.directions
           then
             errorf errors "%s: %d direction lists, expected %d" where (List.length s.frames) m.directions;
           List.iteri
             (fun d frames ->
               if List.length frames <> a.frames
               then
                 errorf
                   errors
                   "%s: direction %d has %d frames, the animation has %d"
                   where
                   d
                   (List.length frames)
                   a.frames;
               List.iteri
                 (fun f cell ->
                   match cell with
                   | None -> ()
                   | Some rect ->
                     check_rect
                       errors
                       ~where:(Printf.sprintf "%s direction %d frame %d" id d f)
                       ~sheet:s.size
                       ~cell:m.cell_px
                       rect)
                 frames)
             s.frames))
    sheets;
  (* layers *)
  List.iter
    (fun (id, (l : sprite_layer)) ->
      let where = "layer " ^ id in
      List.iter
        (fun anim ->
          if animation anim = None
          then errorf errors "%s: the animation %s does not exist" where anim
          else if not (List.mem_assoc (id ^ "." ^ anim) sheets)
          then errorf errors "%s: no sheet %s.%s" where id anim)
        l.animations;
      (match l.weapon_class with
       | None -> ()
       | Some _ ->
         if l.weapon_key = None || l.hand = None || l.sheathe_location = None
         then errorf errors "%s: a weapon layer needs weapon_key, hand and sheathe_location" where;
         (match l.attack_animation with
          | None -> errorf errors "%s: a weapon layer needs attack_animation" where
          | Some a when animation a = None ->
            errorf errors "%s: attack_animation %s does not exist" where a
          | Some _ -> ())))
    layers;
  check_slots
    errors
    ~where:"sprites"
    ~layer_exists:(fun l -> List.mem_assoc l layers)
    ~slot_of:(fun l -> Option.map (fun (x : sprite_layer) -> x.slot) (List.assoc_opt l layers))
    m.slots;
  (match m.weapon_attack with
   | None -> ()
   | Some pairs ->
     List.iter
       (fun (cls, a) ->
         if animation a = None
         then errorf errors "weapon_attack: %s plays %s, which is not an animation" cls a)
       pairs);
  (match m.variant_order, m.variants with
   | Some order, Some variants ->
     List.iter
       (fun v ->
         if not (List.mem_assoc v variants)
         then errorf errors "variant_order: %s is not a variant" v)
       order
   | Some _, None -> errorf errors "variant_order without variants"
   | _ -> ());
  List.rev !errors
;;

(* ---- portraits.json ------------------------------------------------------ *)

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

let check_sprites_string text =
  read_then
    Data.Sprite_j.sprite_meta_of_string
    Data.Sprite_v.validate_sprite_meta
    check_sprites_meta
    text
;;

let check_portraits_string text =
  read_then
    Data.Sprite_j.portrait_meta_of_string
    Data.Sprite_v.validate_portrait_meta
    check_portraits_meta
    text
;;

(* sprites.json or portraits.json, by file name *)
let check_file path =
  match Filename.basename path with
  | "sprites.json" -> check_sprites_string (Io.read_file path)
  | "portraits.json" -> check_portraits_string (Io.read_file path)
  | other -> [ other ^ ": expected a file named sprites.json or portraits.json" ]
;;

let is_meta_file path =
  match Filename.basename path with
  | "sprites.json" | "portraits.json" -> true
  | _ -> false
;;

(* Every sprites.json and portraits.json under [path] (or [path] itself), sorted, with its errors *)
let check_path path : (string * string list) list =
  let files =
    if Sys.is_directory path
    then Io.find_all_files_in_directory is_meta_file path
    else [ path ]
  in
  List.sort compare files |> List.map (fun f -> f, check_file f)
;;
