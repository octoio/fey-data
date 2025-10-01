let generate_atdgen_rules base =
  (* Generate -j rules *)
  Printf.printf
    {|(rule
 (targets %s_j.ml %s_j.mli)
 (deps %s.atd)
 (mode fallback)
 (action
  (run atdgen -j -j-strict-fields -j-std %%{deps} -deriving-conv "eq,show")))

|}
    base
    base
    base;
  (* Generate -t rules *)
  Printf.printf
    {|(rule
 (targets %s_t.ml %s_t.mli)
 (deps %s.atd)
 (mode fallback)
 (action
  (run atdgen -t %%{deps} -deriving-conv "eq,show")))

|}
    base
    base
    base;
  (* Generate -v rules *)
  Printf.printf
    {|(rule
 (targets %s_v.ml %s_v.mli)
 (deps %s.atd)
 (mode fallback)
 (action
  (run atdgen -v %%{deps} -deriving-conv "eq,show")))

|}
    base
    base
    base
;;

let collect_atd_files () =
  Sys.readdir "."
  |> Array.to_list
  |> List.sort String.compare
  |> List.filter_map (fun filename ->
    match Filename.chop_suffix_opt ~suffix:".atd" filename with
    | Some base -> Some base
    | None -> None)
;;

let generate_modules_list atd_files =
  let all_modules =
    List.concat_map (fun base -> [ base ^ "_j"; base ^ "_t"; base ^ "_v" ]) atd_files
  in
  Printf.printf "(library\n";
  Printf.printf " (public_name gamedata.data)\n";
  Printf.printf " (name data)\n";
  Printf.printf " (modules\n";
  List.iter
    (fun module_name -> Printf.printf "  %s\n" module_name)
    (List.sort String.compare all_modules);
  (* Add manually maintained modules *)
  Printf.printf "  entity_util\n";
  Printf.printf "  validation)\n";
  Printf.printf " (libraries atd atdgen-runtime re base config util)\n";
  Printf.printf " (preprocess\n";
  Printf.printf "  (pps ppx_deriving.eq ppx_deriving.show)))\n"
;;

let () =
  let atd_files = collect_atd_files () in
  List.iter generate_atdgen_rules atd_files;
  generate_modules_list atd_files
;;
