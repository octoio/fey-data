let strip_quotes s =
  let replace_quotes = Re.replace_string (Re.Perl.compile_pat "[\"\']") ~by:"" in
  replace_quotes s
;;

let remove_before_prefix prefix s =
  let re = Str.regexp prefix in
  let idx = Str.search_forward re s 0 in
  Stdlib.String.sub s idx (Stdlib.String.length s - idx)
;;