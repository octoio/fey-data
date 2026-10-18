(* Portrait metadata schema (lib/data/portrait.atd) and its cross-field checks (lib/portrait_check.ml) *)

open Alcotest

let portraits ?(frames = 2) ?(rect = "[0,0,8,10,4,6]") ?(slot_layer = "body_green") ?(extra = "") () =
  let layer =
    Printf.sprintf
      {|{"files":{"albedo":"a.png","normal":"n.png","depth":"d.png"},"size":[32,32],"frames":[%s],"world_units_per_pixel":0.01,"texel_scale":3.0,"subject_extent_world":0.8,"anchor":null}|}
      (String.concat "," (List.init 2 (fun _ -> rect)))
  in
  let one =
    Printf.sprintf
      {|{"cell_px":[16,24],"pivot_px":[8.0,12.0],"camera":{"yaw_deg":32,"pitch_deg":14},"direction":0,"clip":"idle","fps":15.0,"frames":%d,"loop":true,"slots":{"body":{"order":0,"layers":{"green":"%s"}}},"layers":{"body_green":%s}%s}|}
      frames
      slot_layer
      layer
      extra
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

let check_portraits s = Gamedata.Portrait_check.check_portraits_string s

let test_valid_portraits () = check_ok "valid" (check_portraits (portraits ()))

let test_portrait_frame_count () =
  check_error "frames" (check_portraits (portraits ~frames:3 ())) "frames"
;;

let test_portrait_rect_outside () =
  check_error
    "rect"
    (check_portraits (portraits ~rect:"[30,0,8,10,4,6]" ()))
    "outside the 32x32 atlas"
;;

let test_unknown_field_is_refused () =
  check_error
    "strict fields"
    (check_portraits (portraits ~extra:{|,"surprise":1|} ()))
    "surprise"
;;

let test_slot_names_a_layer () =
  check_error
    "slot layer"
    (check_portraits (portraits ~slot_layer:"body_red" ()))
    "names the missing layer body_red"
;;

let test_file_kind_by_name () =
  let dir = Filename.concat (Filename.get_temp_dir_name ()) "fey_portrait_test" in
  (try Sys.mkdir dir 0o755 with Sys_error _ -> ());
  let write name content =
    let path = Filename.concat dir name in
    let oc = open_out path in
    output_string oc content;
    close_out oc;
    path
  in
  let a = write "portraits.json" (portraits ()) in
  let b = write "other.json" (portraits ()) in
  check_ok "portraits.json" (Gamedata.Portrait_check.check_file a);
  check_error "other.json" (Gamedata.Portrait_check.check_file b) "expected a file named portraits.json";
  Sys.remove a;
  Sys.remove b;
  Sys.rmdir dir
;;

let () =
  run
    "Portrait metadata"
    [ ( "portraits",
        [ test_case "a valid document passes" `Quick test_valid_portraits;
          test_case "unknown field is refused" `Quick test_unknown_field_is_refused;
          test_case "frame count matches" `Quick test_portrait_frame_count;
          test_case "rect outside the atlas" `Quick test_portrait_rect_outside;
          test_case "a slot names an existing layer" `Quick test_slot_names_a_layer;
          test_case "file kind comes from the name" `Quick test_file_kind_by_name
        ] )
    ]
;;
