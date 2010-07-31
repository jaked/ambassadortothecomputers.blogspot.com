open Camlp4.PreCast

type t =
  | Jq_null
  | Jq_bool   of bool
  | Jq_number of float
  | Jq_string of string
  | Jq_array  of t list
  | Jq_object of (string * t) list

module MetaExpr :
sig
  val meta_t : Ast.loc -> t -> Ast.expr
end

module MetaPatt :
sig
  val meta_t : Ast.loc -> t -> Ast.patt
end
