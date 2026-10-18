(* Validate the portrait metadata: dune exec portraitcheck -- <file or directory>...
   Reads every portraits.json under the given paths. Exit code 1 on any error. *)

let () =
  let paths = List.tl (Array.to_list Sys.argv) in
  if paths = []
  then (
    prerr_endline "usage: portraitcheck <portraits.json | directory>...";
    exit 2);
  let results = List.concat_map Gamedata.Portrait_check.check_path paths in
  let failed = ref 0 in
  List.iter
    (fun (file, errors) ->
      if errors = []
      then Gamedata.Io.print_success ("ok " ^ file)
      else (
        incr failed;
        Gamedata.Io.print_error ("FAIL " ^ file);
        List.iter (fun e -> print_endline ("  " ^ e)) errors))
    results;
  if results = [] then (prerr_endline "no portraits.json found"; exit 2);
  Printf.printf "%d file(s) checked, %d failed\n" (List.length results) !failed;
  if !failed > 0 then exit 1
;;
