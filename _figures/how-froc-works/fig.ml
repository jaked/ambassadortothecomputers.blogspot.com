open Mlpost
open Box
open Tree
open Color
let sprintf = Format.sprintf

let box ?l ?t ?v ?sv ?cv ?s () =
  let rr ?fill so =
    match so with
      | None -> None
      | Some s -> Some (round_rect ~stroke:None ?fill (tex s)) in
  let boxes = [
    rr ~fill:lightblue l;
    rr ~fill:lightgray t;
    (match s with None -> None | Some s -> Some (tex s));
    rr ~fill:lightgreen v;
    rr ~fill:lightred sv;
    rr ~fill:lightmagenta cv;
  ] in
  let rec loop = function
    | [] -> []
    | None :: t -> loop t
    | Some v :: t -> v :: loop t in
  hbox ~padding:(Num.bp 1.) (loop boxes)

let node ?name ?l ?t ?v ?sv ?cv ?(ls=Num.bp 24.) ?(cs=Num.bp 32.) ?s =
  node
    (* ~brush:(Brush.lightgray ()) *)
    ~edge_style:Curve
    ~arrow_style:Undirected
    ~ls
    ~cs
    (rect ?name (box ?l ?t ?v ?sv ?cv ?s ()))

let nodel ?name ?l ?t ?v ?sv ?cv ?(ls=Num.bp 40.) ?(cs=Num.bp 32.) ?s =
  nodel
    (* ~brush:(Brush.lightgray ()) *)
    (* ~edge_style:Curve *)
    ~arrow_style:Undirected
    ~ls
    ~cs
    (rect ?name (box ?l ?t ?v ?sv ?cv ?s ()))

let leaf ?name ?l ?t ?v ?sv ?cv ?s () =
  leaf (rect ?name (box ?l ?t ?v ?sv ?cv ?s ()))

let fig_a =
  let v = leaf ~l:"v" () in
  let w = leaf ~l:"w" () in
  let x = leaf ~l:"x" () in
  let y = leaf ~l:"y" () in
  let z = leaf ~l:"z" () in
  let n0 = node ~s:"/" [v; w] in
  let n1 = node ~s:"*" [x; y] in
  let n2 = node ~s:"+" [n0; n1] in
  let u = node ~s:"+" [n2; z] in
  draw u

let fig_b =
  let v = leaf ~v:"4" ~l:"v" () in
  let w = leaf ~v:"2" ~l:"w" () in
  let x = leaf ~v:"2" ~l:"x" () in
  let y = leaf ~v:"3" ~l:"y" () in
  let z = leaf ~v:"1" ~l:"z" () in
  let n0 = node ~l:"n0" ~v:"2" ~s:"/" [v; w] in
  let n1 = node ~l:"n1" ~v:"6" ~s:"*" [x; y] in
  let n2 = node ~l:"n2" ~v:"8" ~s:"+" [n0; n1] in
  let u = node ~l:"u" ~v:"9" ~s:"+" [n2; z] in
  draw u

let fig_c =
  let v = leaf ~cv:"6" ~l:"v" () in
  let w = leaf ~v:"2" ~l:"w" () in
  let x = leaf ~v:"2" ~l:"x" () in
  let y = leaf ~v:"3" ~l:"y" () in
  let z = leaf ~cv:"2" ~l:"z" () in
  let n0 = node ~t:"0" ~l:"n0" ~sv:"2" ~s:"/" [v; w] in
  let n1 = node ~t:"1" ~l:"n1" ~v:"6" ~s:"*" [x; y] in
  let n2 = node ~t:"2" ~l:"n2" ~sv:"8" ~s:"+" [n0; n1] in
  let u = node ~t:"3" ~l:"u" ~sv:"9" ~s:"+" [n2; z] in
  draw u

let fig_d =
  let x = leaf ~name:"x" ~l:"x" () in
  let b = node ~t:"0" ~l:"b" ~s:"x = 0" [x] in
  let n0 = node ~t:"1" ~name:"n0" ~l:"n0" ~s:"100 / x" [] in
  let y = node ~t:"2" ~l:"y" ~s:"if b then 0 else n0" [b; n0] in
  let box = to_box y in
  Command.seq [
    Box.draw box;
    Helpers.box_line
      (* ~color:lightgray *)
      (Box.get "n0" box) (Box.get "x" box);
  ]

let fig_e =
  let x = leaf ~name:"x" ~v:"10" ~l:"x" () in
  let b = nodel ~t:"1-2" ~v:"false" ~l:"b" ~s:"x = 0" [x, (`West, Picture.tex "0")] in
  let n0 = nodel ~t:"6-7" ~v:"10" ~name:"n0" ~l:"n0" ~s:"100 / x" [] in
  let y = nodel ~t:"4-9" ~v:"10" ~l:"y" ~s:"if b then 0 else n0" [b, (`Northwest, Picture.tex "3"); n0, (`Northeast, Picture.tex "8")] in
  let box = to_box y in
  Command.seq [
    Box.draw box;
    Helpers.box_label_line
      (* ~color:lightgray *)
      ~pos:`Southeast
      (Picture.tex "5")
      (Box.get "n0" box) (Box.get "x" box);
  ]

let fig_f =
  let nodel = nodel ~ls:(Num.bp 40.) ~cs:(Num.bp 4.) in
  let ne s n = n, (`Northeast, Picture.tex s) in
  let nw s n = n, (`Northwest, Picture.tex s) in

  let t0 = leaf ~v:"C(1,t1)" ~l:"t0" () in
  let t1 = leaf ~v:"C(2,t2)" ~l:"t1" () in
  let t2 = leaf ~v:"C(3,t3)" ~l:"t2" () in
  let t3 = leaf ~v:"N" ~l:"t3" () in
  let f3 = nodel ~v:"N" ~t:"7-8" ~l:"f3" ~s:"f" [ nw "6" t3 ] in
  let f2 = nodel ~v:"C(4,f3)" ~t:"5-10" ~l:"f2" ~s:"f" [ nw "4" t2; ne "9" f3 ] in
  let f1 = nodel ~v:"C(3,f2)" ~t:"3-12" ~l:"f1" ~s:"f" [ nw "2" t1; ne "11" f2 ] in
  let f0 = nodel ~v:"C(2,f1)" ~t:"1-14" ~l:"f0" ~s:"f" [ nw "0" t0; ne "13" f1 ] in
  draw f0

(*
let fig_g =
  let nodel = nodel ~ls:(Num.bp 40.) ~cs:(Num.bp 4.) in
  let ne s n = n, (`Northeast, Picture.tex s) in
  let nw s n = n, (`Northwest, Picture.tex s) in

  let t0 = leaf ~cv:"C(2,t1)" ~l:"t0" () in
  let t1 = leaf ~v:"C(2,t2)" ~l:"t1" () in
  let t2 = leaf ~cv:"C(4,t3)" ~l:"t2" () in
  let t3 = leaf ~v:"N" ~l:"t3" () in
  let f3 = nodel ~v:"N" ~t:"7-8" ~l:"f3" ~s:"f" [ nw "6" t3 ] in
  let f2 = nodel ~v:"C(4,f3)" ~t:"5-10" ~l:"f2" ~s:"f" [ nw "4" t2; ne "9" f3 ] in
  let f1 = nodel ~v:"C(3,f2)" ~t:"3-12" ~l:"f1" ~s:"f" [ nw "2" t1; ne "11" f2 ] in
  let f0 = nodel ~v:"C(2,f1)" ~t:"1-14" ~l:"f0" ~s:"f" [ nw "0" t0; ne "13" f1 ] in

  let timeline =
    let ts s = round_rect ~stroke:None ~fill:lightgreen (tex s) in
    let sots s = round_rect ~stroke:None ~fill:lightred (tex s) in
    hbox ~padding:(Num.bp 1.) [
      ts "0"; ts "1"; ts "2"; ts "3"; ts "4"; ts "5"; ts "6"; ts "7";
      ts "8"; ts "9"; ts "10"; ts "10"; ts "12"; ts "13"; ts "14";
    ] in

  let pq =
    let r s = round_rect ~stroke:None ~fill:lightgreen (tex s) in
    let sor s = round_rect ~stroke:None ~fill:lightred (tex s) in
    hbox ~padding:(Num.bp 1.) [
      r "f0, 1-14"; r "f2, 5-10";
    ] in

  Box.draw (vbox ~padding:(Num.bp 3.) [ to_box f0; timeline; pq ])

let fig_h =
  let nodel = nodel ~ls:(Num.bp 40.) ~cs:(Num.bp 4.) in
  let ne s n = n, (`Northeast, Picture.tex s) in
  let nw s n = n, (`Northwest, Picture.tex s) in

  let t0 = leaf ~cv:"C(2,t1)" ~l:"t0" () in
  let t1 = leaf ~v:"C(2,t2)" ~l:"t1" () in
  let t2 = leaf ~cv:"C(4,t3)" ~l:"t2" () in
  let t3 = leaf ~v:"N" ~l:"t3" () in
  let f3 = nodel ~v:"N" ~t:"7-8" ~l:"f3" ~s:"f" [ nw "6" t3 ] in
  let f2 = nodel ~v:"C(4,f3)" ~t:"5-10" ~l:"f2" ~s:"f" [ nw "4" t2; ne "9" f3 ] in
  let f1 = nodel ~v:"C(3,f2)" ~t:"3-12" ~l:"f1" ~s:"f" [ nw "2" t1; ne "11" f2 ] in
  let f0 = nodel ~v:"C(2,f1)" ~t:"1-14" ~l:"f0" ~s:"f" [ nw "0" t0; ne "13" f1 ] in

  let timeline =
    let ts s = round_rect ~stroke:None ~fill:lightgreen (tex s) in
    let now s = rect (hbox ~padding:(Num.bp 1.) [ ts s ]) in
    let sots s = round_rect ~stroke:None ~fill:lightred (tex s) in
    hbox ~padding:(Num.bp 1.) [
      ts "0"; now "1"; ts "2"; ts "3"; ts "4"; ts "5"; ts "6"; ts "7";
      ts "8"; ts "9"; ts "10"; ts "10"; ts "12"; ts "13"; ts "14";
    ] in

  let pq =
    let r s = round_rect ~stroke:None ~fill:lightgreen (tex s) in
    let sor s = round_rect ~stroke:None ~fill:lightred (tex s) in
    hbox ~padding:(Num.bp 1.) [
      r "f2, 5-10";
    ] in

  Box.draw (vbox ~padding:(Num.bp 3.) [ to_box f0; timeline; pq ])
*)

;;

List.iter
  (fun (name, fig) ->
     Metapost.emit
       ("how-froc-works-" ^ name)
       (Picture.scale (Num.bp 2.) fig))
  [
    "a", fig_a;
    "b", fig_b;
    "c", fig_c;
    "d", fig_d;
    "e", fig_e;
    "f", fig_f;
(*
    "g", fig_g;
    "h", fig_h;
*)
  ]
