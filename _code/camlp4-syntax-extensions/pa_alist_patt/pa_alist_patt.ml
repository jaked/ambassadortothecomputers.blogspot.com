open Camlp4

module Id : Sig.Id =
struct
  let name = "pa_alist_patt"
  let version = "0.1"
end

module Make1 (Syntax : Sig.Camlp4Syntax) =
struct
  open Sig
  include Syntax

  EXTEND Gram
    patt: LEVEL "simple"
    [[
       "alist"; "[";
         l =
           LIST0
             [ e = expr LEVEL "simple"; ","; p = patt LEVEL "simple" -> Ast.PaOlbi (_loc, "", p, e) ]
             SEP ";";
       "]" ->
         <:patt< $uid:"alist"$ $Ast.paSem_of_list l$ >>
    ]];
  END
end

module Make2 (AstFilters : Camlp4.Sig.AstFilters) =
struct
  open AstFilters (* for Ast module *)

  let fresh =
    let id = ref 0 in
    fun () ->
      incr id;
      "__pa_alist_patt_"  ^ string_of_int !id

  let expr_tup_of_list _loc = function
    | [] -> <:expr< () >>
    | [ v ] -> v
    | vs -> <:expr< $tup:Ast.exCom_of_list vs$ >>

  let patt_tup_of_list _loc = function
    | [] -> <:patt< () >>
    | [ p ] -> p
    | ps -> <:patt< $tup:Ast.paCom_of_list ps$ >>

  let rewrite _loc p w e =
    let k = ref (fun s f -> s) in
    let map =
      object
        inherit Ast.map as super

        method patt p =
          match super#patt p with
            | <:patt< $uid:"alist"$ $l$ >> ->
                let id = fresh () in
                let l =
                  List.map
                    (function
                       | Ast.PaOlbi (_, _, p, e) -> p, e
                       | _ -> assert false)
                    (Ast.list_of_patt l []) in
                let vs =
                  List.map
                    (fun (_, e) ->
                       <:expr<
                         try Some (List.assoc $e$ $lid:id$)
                         with Not_found -> None
                       >>)
                    l in
                let ps =
                  List.map
                    (fun (p, _) -> <:patt< Some $p$ >>)
                    l in
                let k' = !k in
                k :=
                  (fun s f ->
                     <:expr<
                       match $expr_tup_of_list _loc vs$ with
                         | $patt_tup_of_list _loc ps$ -> $k' s f$
                         | _ -> $f$
                     >>);
                <:patt< $lid:id$ >>
            | p -> p
      end in
    let p = map#patt p in
    let w = match w with <:expr< >> -> <:expr< true >> | _ -> w in
    let w = !k w <:expr< false >> in
    let e = !k e <:expr< assert false >> in
    <:match_case< $p$ when $w$ -> $e$ >>

  let filter =
    let map =
      object
        inherit Ast.map as super

        method match_case mc =
          match super#match_case mc with
            | <:match_case@_loc< $p$ when $w$ -> $e$ >> ->
                rewrite _loc p w e
            | e -> e
      end in
    map#str_item

  let _ = AstFilters.register_str_item_filter filter
end

module M1 = Register.OCamlSyntaxExtension(Id)(Make1)
module M2 = Camlp4.Register.AstFilter(Id)(Make2)
