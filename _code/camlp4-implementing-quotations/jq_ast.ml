module Jq_ast =
struct
  type float' = float

  type t =
    | Jq_null
    | Jq_bool   of bool
    | Jq_number of float'
    | Jq_string of string
    | Jq_array  of t list
    | Jq_object of (string * t) list
end

include Jq_ast

open Camlp4.PreCast (* for Ast refs in generated code *)

module MetaExpr =
struct
  let meta_float' _loc f = <:expr< $`flo:f$ >>
  include Camlp4Filters.MetaGeneratorExpr(Jq_ast)
end

module MetaPatt =
struct
  let meta_float' _loc f = <:patt< $`flo:f$ >>
  include Camlp4Filters.MetaGeneratorPatt(Jq_ast)
end
