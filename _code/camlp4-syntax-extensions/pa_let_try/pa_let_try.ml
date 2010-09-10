open Camlp4

module Id : Sig.Id =
struct
  let name = "pa_let_try"
  let version = "0.1"
end

module Make (Syntax : Sig.Camlp4Syntax) =
struct
  open Sig
  include Syntax

  EXTEND Gram
    expr: LEVEL "top" [
      [ "let"; "try"; r = opt_rec; bi = binding; "in"; e = sequence; "with"; a = match_case ->
          let a =
            List.map
              (function
                 | <:match_case< $p$ when $w$ -> $e$ >> ->
                     <:match_case< $p$ when $w$ -> fun () -> $e$ >>
                 | mc -> mc)
              (Ast.list_of_match_case a []) in
          <:expr< (try let $rec:r$ $bi$ in fun () -> do { $e$ } with [ $list:a$ ])() >>
(* original syntax:
          let e = Ast.ExSeq (Ast.loc_of_expr e, e) in
          <:expr< (try let $rec:r$ $bi$ in fun () -> $e$ with $list:a$)() >>
*)
      ]
    ];
  END
end

module M = Register.OCamlSyntaxExtension(Id)(Make)
