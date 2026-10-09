(* ============================================================================ *)
(* ANNOTATIONS                                                                 *)
(* ============================================================================ *)

(* Rust-specific annotations live in `<rs ...>` sections, like `<cs ...>` for C#:
     ignore="true"       skip the type or record field
     name="Foo"          Rust name of a type
     usetype="Foo"       Rust type of a record field
     attributes="..."    raw attribute lines (separated by ';') before a type or field
     derive="Eq,Hash"    additional derives for a type
   JSON-shape annotations (`<json name="...">`, `<json adapter.ocaml="...">`) are shared
   with atdgen and read from `<json ...>` sections. *)

let find_in_sections (sections : string list) (annot : Atd.Ast.annot) key =
  let rec go = function
    | [] -> None
    | (name, (_, fields)) :: rest ->
      if List.mem name sections
      then (
        match List.find_opt (fun (field_name, _) -> field_name = key) fields with
        | Some (_, (_, Some value)) -> Some value
        | _ -> go rest)
      else go rest
  in
  go annot
;;

let get_rs (annot : Atd.Ast.annot) key = find_in_sections [ "rs" ] annot key
let get_json (annot : Atd.Ast.annot) key = find_in_sections [ "json" ] annot key
let get_ocaml (annot : Atd.Ast.annot) key = find_in_sections [ "ocaml" ] annot key
let is_ignored annot = get_rs annot "ignore" <> None

let split_list sep value =
  String.split_on_char sep value |> List.map String.trim |> List.filter (( <> ) "")
;;

let attributes annot =
  match get_rs annot "attributes" with
  | Some attrs -> split_list ';' attrs
  | None -> []
;;

let extra_derives annot =
  match get_rs annot "derive" with
  | Some derives -> split_list ',' derives
  | None -> []
;;

type adapter = Type_field

let adapter_of_annot annot =
  match get_json annot "adapter.ocaml" with
  | None -> None
  | Some "Atdgen_runtime.Json_adapter.Type_field" -> Some Type_field
  | Some other -> failwith ("unsupported JSON adapter: " ^ other)
;;
