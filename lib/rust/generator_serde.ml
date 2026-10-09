(* ============================================================================ *)
(* CUSTOM SERDE IMPLS FOR SUMS WITH PAYLOADS                                   *)
(* ============================================================================ *)

(* serde's derived enums use `{"Tag": payload}`; atdgen uses `["Tag", payload]`, or the
   `{"type": "Tag", ...}` object form with the Type_field adapter. Both are written by hand
   here, in the same spirit as the C# converters. *)

open Types
open Generator_types
open Annotations

let generate_impls (entry : entry) (variants : variant list) (adapter : adapter option) :
  string
  =
  let name = entry.rust_name in
  let ser_fn, parse_fn =
    match adapter with
    | Some Type_field -> ("serialize_type_field", "parse_type_field")
    | None -> ("serialize_tagged", "parse_tagged")
  in
  let ser_arms =
    List.map
      (fun v ->
        match v.payload with
        | None ->
          Printf.sprintf
            "            %s::%s => serializer.serialize_str(\"%s\"),\n"
            name v.ident v.tag
        | Some _ ->
          Printf.sprintf
            "            %s::%s(payload) => runtime::%s(serializer, \"%s\", payload),\n"
            name v.ident ser_fn v.tag)
      variants
  in
  let de_arms =
    List.map
      (fun v ->
        match v.payload with
        | None ->
          Printf.sprintf "            (\"%s\", None) => Ok(%s::%s),\n" v.tag name v.ident
        | Some _ ->
          Printf.sprintf
            "            (\"%s\", Some(payload)) => runtime::from_payload(payload).map(%s::%s),\n"
            v.tag name v.ident)
      variants
  in
  Printf.sprintf
    "\nimpl Serialize for %s {\n\
    \    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {\n\
    \        match self {\n\
     %s\
    \        }\n\
    \    }\n\
     }\n\
     \n\
     impl<'de> Deserialize<'de> for %s {\n\
    \    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {\n\
    \        let value = serde_json::Value::deserialize(deserializer)?;\n\
    \        let (tag, payload) = runtime::%s(value).map_err(serde::de::Error::custom)?;\n\
    \        match (tag.as_str(), payload) {\n\
     %s\
    \            (other, _) => Err(runtime::bad_variant(\"%s\", other)),\n\
    \        }\n\
    \    }\n\
     }\n"
    name
    (String.concat "" ser_arms)
    name
    parse_fn
    (String.concat "" de_arms)
    name
;;
