(* String utilities *)
let strip_quotes s =
  let replace_quotes = Re.replace_string (Re.Perl.compile_pat "[\"\']") ~by:"" in
  replace_quotes s
;;

(* Entity type conversion utilities *)
let string_of_entity_type (entity_type : Common_t.entity_type) =
  strip_quotes @@ Common_j.string_of_entity_type entity_type
;;

let string_of_cursor_type (cursor_type : Cursor_t.cursor_type) =
  strip_quotes @@ Cursor_j.string_of_cursor_type cursor_type
;;

let string_of_stat_type (stat_type : Stat_t.stat_type) =
  strip_quotes @@ Stat_j.string_of_stat_type stat_type
;;

let string_of_quality_type (quality_type : Quality_t.quality_type) =
  strip_quotes @@ Quality_j.string_of_quality_type quality_type
;;