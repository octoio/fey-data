(* One status per meaning, named and pictured for what it does to the character (fey-rs #427).
   - A status's title is never the name of a stat ("Damage Taken", "Movement Speed"): a player reads what
     happened to the character, not the mechanism. A stat's name is its key, its title or its display label, read without case,
     spaces or a trailing "Modifier".
   - A StatChange status is applied one way only, as a gain or as a loss of its stat (the sign of its
     scalers, read against the stat's [lower_is_better]), and its picture family agrees: a [buff/] effect for
     a gain, a [debuff/] one for a loss. The same status on a protection and on a weakness is two meanings. *)

open Data.Entity_t

let letters_only name =
  String.lowercase_ascii name
  |> String.to_seq
  |> Seq.filter (fun c -> c >= 'a' && c <= 'z')
  |> String.of_seq
;;

(* "Damage Taken Modifier", "DamageTakenModifier" and "Damage Taken" all read "damagetaken" *)
let stat_name_reading name =
  let reading = letters_only name in
  let suffix = "modifier" in
  let cut = String.length reading - String.length suffix in
  if cut > 0 && String.sub reading cut (String.length suffix) = suffix
  then String.sub reading 0 cut
  else reading
;;

let stats definitions =
  List.filter_map
    (fun (definition : entity_definition_internal) ->
      match definition with
      | `Stat { key; entity; _ } -> Some (key, entity)
      | _ -> None)
    definitions
;;

let statuses definitions =
  List.filter_map
    (fun (definition : entity_definition_internal) ->
      match definition with
      | `Status { id; key; entity; _ } -> Some (id, key, entity)
      | _ -> None)
    definitions
;;

let title_error_messages definitions =
  let stat_readings =
    List.concat_map
      (fun (key, (stat : Data.Stat_t.stat)) ->
        List.filter_map
          Fun.id
          [ Some (stat_name_reading key, key);
            Some (stat_name_reading stat.metadata.title, key);
            Option.map (fun label -> stat_name_reading label, key) stat.display.label
          ])
      (stats definitions)
  in
  List.filter_map
    (fun (_, key, (status : Data.Status_t.status)) ->
      match List.assoc_opt (stat_name_reading status.metadata.title) stat_readings with
      | Some stat ->
        Some
          (Printf.sprintf
             "Status %s is titled \"%s\", the name of the stat %s: title it for what happens to the character"
             key
             status.metadata.title
             stat)
      | None -> None)
    (statuses definitions)
;;

let number = function
  | `Float f -> Some f
  | `Int i -> Some (float_of_int i)
  | _ -> None
;;

(* +1 when every scaler base is positive, -1 when every one is negative, else the application has no sign *)
let sign_of_scalers scalers =
  let bases =
    List.filter_map
      (function
        | `Assoc fields -> Option.bind (List.assoc_opt "base" fields) number
        | _ -> None)
      scalers
  in
  if bases <> [] && List.for_all (fun b -> b > 0.) bases
  then Some 1
  else if bases <> [] && List.for_all (fun b -> b < 0.) bases
  then Some (-1)
  else None
;;

(* Every application of a status in the data, whatever carries it (a skill node, a trigger, a projectile, an
   enrage): a JSON object with scalers and a status reference. *)
let rec applications (json : Yojson.Safe.t) =
  match json with
  | `Assoc fields ->
    let here =
      match List.assoc_opt "scalers" fields, List.assoc_opt "status" fields with
      | Some (`List scalers), Some (`Assoc reference) ->
        (match List.assoc_opt "id" reference, sign_of_scalers scalers with
         | Some (`String id), Some sign -> [ id, sign ]
         | _ -> [])
      | _ -> []
    in
    here @ List.concat_map (fun (_, value) -> applications value) fields
  | `List items -> List.concat_map applications items
  | _ -> []
;;

let all_applications definitions =
  List.concat_map
    (fun definition ->
      applications
        (Yojson.Safe.from_string (Data.Entity_j.string_of_entity_definition_internal definition)))
    definitions
;;

(* The family of an effect key: "buff" in "buff/haste" *)
let picture_family (status : Data.Status_t.status) =
  Option.map
    (fun (vfx : Data.Vfx_t.status_vfx) -> List.hd (String.split_on_char '/' vfx.effect))
    status.vfx
;;

let direction_error_messages definitions applied =
  let lower_is_better stat =
    List.find_map
      (fun (_, (s : Data.Stat_t.stat)) ->
        if s.stat_type = stat then Some s.display.lower_is_better else None)
      (stats definitions)
  in
  List.concat_map
    (fun (id, key, (status : Data.Status_t.status)) ->
      match status.mechanic with
      | `StatChange { stat; _ } ->
        let gains =
          List.sort_uniq
            compare
            (List.filter_map
               (fun (applied_id, sign) ->
                 if applied_id <> id
                 then None
                 else Some ((sign > 0) <> Option.value (lower_is_better stat) ~default:false))
               applied)
        in
        let family = picture_family status in
        if List.length gains > 1
        then
          [ Printf.sprintf
              "Status %s is applied both as a gain and as a loss: one status per meaning"
              key
            ]
        else (
          match gains, family with
          | [ true ], Some "debuff" | [ false ], Some "buff" ->
            [ Printf.sprintf
                "Status %s is applied as a %s but its picture is a %s effect"
                key
                (if List.hd gains then "gain" else "loss")
                (Option.get family)
            ]
          | _ -> [])
      | _ -> [])
    (statuses definitions)
;;

(* The walk finds applications by the shape of the data; should a schema change hide them all, the rule would
   pass for ever, so a dataset with StatChange statuses and no application found fails instead. *)
let blind_walk_error_messages definitions applied =
  let stat_changes =
    List.exists
      (fun (_, _, (status : Data.Status_t.status)) ->
        match status.mechanic with
        | `StatChange _ -> true
        | _ -> false)
      (statuses definitions)
  in
  if stat_changes && applied = []
  then
    [ "Status check: StatChange statuses exist but no application of any status was found: the walk in lib/status_check.ml no longer matches the schema"
    ]
  else []
;;

let error_messages definitions =
  let applied = all_applications definitions in
  title_error_messages definitions
  @ blind_walk_error_messages definitions applied
  @ direction_error_messages definitions applied
;;
