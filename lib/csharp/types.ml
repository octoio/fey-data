(* ============================================================================ *)
(* TYPES AND CONSTANTS                                                         *)
(* ============================================================================ *)

type line_type =
  [ `Import
  | `Namespace
  | `Class
  | `Struct
  | `Enum
  | `Blank
  | `Bracket
  | `Field
  | `Attribute
  | `Ignore
  ]

type line = line_type * string

type cs_file =
  { lines : line list;
    file_kind : [ `Enum | `Struct | `Class | `Unknown ];
    namespace : string;
    name : string
  }

(* Generator configuration record *)
type generator_config =
  { generator_type : string;
    namespace : string option;
    name : string option;
    enumname : string option;
    modifiers : string list option
  }

(* Common line constants *)
let blank_line = (`Blank, "")
let blank_line_concat = [ blank_line ]
let open_bracket = (`Bracket, "{")
let open_bracket_concat = [ open_bracket ]
let close_bracket = (`Bracket, "}")
let close_bracket_concat = [ close_bracket ]
let ignore_line = (`Ignore, "")
let ignore_line_concat = [ ignore_line ]
