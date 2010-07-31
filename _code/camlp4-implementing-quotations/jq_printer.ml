open Format
open Jq_ast

let rec list f ppf = function
  | [] -> ()
  | [ e ] -> f ppf e
  | e :: es -> fprintf ppf "%a,@;<1 2>" f e; list f ppf es

let rec t ppf = function
  | Jq_null  -> fprintf ppf "null"
  | Jq_bool b -> fprintf ppf "%B" b
  | Jq_number n -> fprintf ppf "%g" n
  | Jq_string s -> fprintf ppf "%S" s
  | Jq_array ts -> fprintf ppf "@[<hv>[@;<1 2>%a@ ]@]" (list t) ts
  | Jq_object ts -> fprintf ppf "@[<hv>{@;<1 2>%a@ }@]" (list kv) ts

and kv ppf (k, v) = fprintf ppf "@[<h>%S@ :@ %a@]" k t v
