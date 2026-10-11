(* Sprite metadata schema (lib/data/sprite.atd) and its cross-field checks (lib/sprite_check.ml) *)

open Alcotest

(* A hero-like document: two directions, one body layer with idle (2 frames) and an attack (4 frames),
   a weapon layer, and the attack mapping *)
let sprites ?(attack_phases = {|{"windup":[0,1],"impact":[2,2],"recovery":[3,3]}|})
  ?(impact_frame = "2") ?(rect = "[0,0,8,10,4,6]") ?(extra = "") ?(attack_sheet_dirs = 2)
  ?(weapon_attack = {|"OneHandSword":"attack_1h"|}) () =
  let dir frames = "[" ^ String.concat "," (List.init frames (fun _ -> rect)) ^ "]" in
  let dirs n frames = "[" ^ String.concat "," (List.init n (fun _ -> dir frames)) ^ "]" in
  let sheet name n frames =
    Printf.sprintf
      {|"%s":{"files":{"albedo":"%s.albedo.png","normal":"%s.normal.png","depth":"%s.depth.png"},"size":[32,32],"frames":%s}|}
      name
      name
      name
      name
      (dirs n frames)
  in
  Printf.sprintf
    {|{"version":3,"kind":"hero","cell_px":[16,16],"pivot_px":[8,12],"directions":2,
"direction_convention":"k","camera":{"yaw_deg":45.0,"pitch_deg":35.0,"ortho_height_per_distance":0.66,"default_distance":23.0,"reference_window_height_px":900,"pixelation_divisor":2},
"scale":{"pixels_per_world_unit":29.6,"world_units_per_pixel":0.0337,"character_height_world":1.7},
"normal_encoding":"n","depth_encoding":{"format":"f","formula":"g","half_range_world":3.0,"offset_meaning":"m"},"frame_encoding":"e",
"palette":{"source":"p","colors":22},
"animations":{
 "idle":{"clip":"idle","kind":"idle","loop":true,"sample_fps":15.0,"frames":2,"duration":0.13,"native_fps":30.0,"ground_speed":0.0},
 "attack_1h":{"clip":"attack_1h","kind":"attack","loop":false,"sample_fps":30.0,"frames":4,"duration":0.13,"native_fps":30.0,"impact_time":0.07,"impact_frame":%s,"phases":%s,"peak_tip_speed":25.5}},
"weapon_attack":{%s},
"slots":{"body":{"order":0,"layers":{"bare":"body.bare"}},"weapon":{"order":7,"layers":{"rustysword":"weapon.rustysword"}}},
"layers":{
 "body.bare":{"slot":"body","variant":"bare","item":null,"order":0,"animations":["idle","attack_1h"]},
 "weapon.rustysword":{"slot":"weapon","variant":"rustysword","item":"RustySword","order":7,"animations":["idle","attack_1h"],"weapon_key":"RustySword","weapon_class":"OneHandSword","hand":"MainHand","sheathe_location":"Hips","attack_animation":"attack_1h"}},
"sheets":{%s,%s,%s,%s},
"portraits":"portraits/portraits.json"%s}|}
    impact_frame
    attack_phases
    weapon_attack
    (sheet "body.bare.idle" 2 2)
    (sheet "body.bare.attack_1h" attack_sheet_dirs 4)
    (sheet "weapon.rustysword.idle" 2 2)
    (sheet "weapon.rustysword.attack_1h" 2 4)
    extra
;;

let portraits ?(frames = 2) ?(rect = "[0,0,8,10,4,6]") () =
  let layer =
    Printf.sprintf
      {|{"files":{"albedo":"a.png","normal":"n.png","depth":"d.png"},"size":[32,32],"frames":[%s],"world_units_per_pixel":0.01,"texel_scale":3.0,"subject_extent_world":0.8,"anchor":null}|}
      (String.concat "," (List.init 2 (fun _ -> rect)))
  in
  let one =
    Printf.sprintf
      {|{"cell_px":[16,24],"pivot_px":[8.0,12.0],"camera":{"yaw_deg":32,"pitch_deg":14},"direction":0,"clip":"idle","fps":15.0,"frames":%d,"loop":true,"slots":{"body":{"order":0,"layers":{"green":"body_green"}}},"layers":{"body_green":%s}}|}
      frames
      layer
  in
  Printf.sprintf {|{"full":%s,"frame":%s}|} one one
;;

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0
;;

let check_ok msg errors =
  if errors <> [] then failf "%s: unexpected errors:\n%s" msg (String.concat "\n" errors)
;;

let check_error msg errors needle =
  if not (List.exists (fun e -> contains e needle) errors)
  then
    failf
      "%s: expected an error containing %S, got:\n%s"
      msg
      needle
      (String.concat "\n" errors)
;;

let check_sprites s = Gamedata.Sprite_check.check_sprites_string s
let check_portraits s = Gamedata.Sprite_check.check_portraits_string s

let test_valid_sprites () = check_ok "valid" (check_sprites (sprites ()))

let test_unknown_field_is_refused () =
  check_error
    "strict fields"
    (check_sprites (sprites ~extra:{|,"surprise":1|} ()))
    "surprise"
;;

let test_wrong_version () =
  let s = sprites () in
  let s = Str.global_replace (Str.regexp_string {|"version":3|}) {|"version":2|} s in
  check_error "version" (check_sprites s) "version"
;;

let test_rect_outside_sheet () =
  check_error
    "rect outside"
    (check_sprites (sprites ~rect:"[30,0,8,10,4,6]" ()))
    "outside the 32x32 sheet"
;;

let test_offset_outside_cell () =
  check_error
    "offset outside the cell"
    (check_sprites (sprites ~rect:"[0,0,8,10,12,6]" ()))
    "outside the 16x16 cell"
;;

let test_direction_count () =
  check_error
    "directions"
    (check_sprites (sprites ~attack_sheet_dirs:1 ()))
    "body.bare.attack_1h"
;;

let test_impact_frame_in_impact_phase () =
  check_error
    "impact frame"
    (check_sprites (sprites ~impact_frame:"0" ()))
    "impact_frame"
;;

let test_phases_cover_the_clip () =
  check_error
    "phases"
    (check_sprites
       (sprites ~attack_phases:{|{"windup":[0,1],"impact":[2,2],"recovery":[3,5]}|} ()))
    "phases"
;;

let test_weapon_attack_names_an_animation () =
  check_error
    "weapon_attack"
    (check_sprites (sprites ~weapon_attack:{|"Bow":"attack_ranged"|} ()))
    "attack_ranged"
;;

let test_missing_sheet () =
  let replace a b s = Str.global_replace (Str.regexp_string a) b s in
  let s =
    sprites ()
    |> replace
         {|"animations":{
 "idle"|}
         {|"animations":{
 "walk":{"clip":"walk","kind":"move","loop":true,"sample_fps":15.0,"frames":2,"duration":0.13,"native_fps":30.0},
 "idle"|}
    |> replace {|"order":0,"animations":["idle","attack_1h"]},|} {|"order":0,"animations":["idle","attack_1h","walk"]},|}
  in
  check_error "missing sheet" (check_sprites s) "no sheet body.bare.walk"
;;

let test_valid_portraits () = check_ok "valid" (check_portraits (portraits ()))

let test_portrait_frame_count () =
  check_error "frames" (check_portraits (portraits ~frames:3 ())) "frames"
;;

let test_portrait_rect_outside () =
  check_error
    "rect"
    (check_portraits (portraits ~rect:"[30,0,8,10,4,6]" ()))
    "outside the 32x32 sheet"
;;

let test_file_kind_by_name () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "fey_sprite_test" in
  (try Sys.mkdir dir 0o755 with Sys_error _ -> ());
  let write name content =
    let path = Filename.concat dir name in
    let oc = open_out path in
    output_string oc content;
    close_out oc;
    path
  in
  let a = write "sprites.json" (sprites ()) in
  let b = write "portraits.json" (portraits ~frames:3 ()) in
  check_ok "sprites.json" (Gamedata.Sprite_check.check_file a);
  check_error "portraits.json" (Gamedata.Sprite_check.check_file b) "frames";
  Sys.remove a;
  Sys.remove b;
  Sys.rmdir dir
;;

let () =
  run
    "Sprite metadata"
    [ ( "sprites",
        [ test_case "a valid document passes" `Quick test_valid_sprites;
          test_case "unknown field is refused" `Quick test_unknown_field_is_refused;
          test_case "version 3 is required" `Quick test_wrong_version;
          test_case "rect outside the sheet" `Quick test_rect_outside_sheet;
          test_case "offset outside the cell" `Quick test_offset_outside_cell;
          test_case "one frame list per direction" `Quick test_direction_count;
          test_case "impact frame inside the impact phase" `Quick test_impact_frame_in_impact_phase;
          test_case "phases cover the clip" `Quick test_phases_cover_the_clip;
          test_case "weapon_attack names an animation" `Quick test_weapon_attack_names_an_animation;
          test_case "a listed animation needs a sheet" `Quick test_missing_sheet
        ] );
      ( "portraits",
        [ test_case "a valid document passes" `Quick test_valid_portraits;
          test_case "frame count matches" `Quick test_portrait_frame_count;
          test_case "rect outside the sheet" `Quick test_portrait_rect_outside;
          test_case "file kind comes from the name" `Quick test_file_kind_by_name
        ] )
    ]
;;
