open Camlp4.PreCast

module Q = Syntax.Quotation

let json_eoi = Jq_parser.Gram.Entry.mk "json_eoi"

EXTEND Jq_parser.Gram
  json_eoi: [[ x = Jq_parser.json; EOI -> x ]];
END;;

let parse_quot_string loc s =
  Jq_parser.Gram.parse_string json_eoi loc s

let expand_expr loc _ s =
  Jq_ast.MetaExpr.meta_t loc (parse_quot_string loc s)

let expand_str_item loc _ s =
  let exp_ast = expand_expr loc None s in
  <:str_item@loc< $exp:exp_ast$ >>

let expand_patt loc _ s =
  Jq_ast.MetaPatt.meta_t loc (parse_quot_string loc s)

;;

Q.add "json" Q.DynAst.expr_tag expand_expr;
Q.add "json" Q.DynAst.patt_tag expand_patt;
Q.add "json" Q.DynAst.str_item_tag expand_str_item;
Q.default := "json"
