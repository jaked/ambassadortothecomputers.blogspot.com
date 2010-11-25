module type OrderedType =
sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t
  val make : unit -> t
  val add : t -> elt -> unit
  val peek_min : t -> elt option
  val take_min : t -> elt
  val size : t -> int
end

module Make (O : OrderedType) : S with type elt = O.t =
struct
  type elt = O.t
  type t = { mutable arr : elt array; mutable len : int }

  let make () = { arr = [||]; len = 0; }

  let compare h i1 i2 = O.compare h.arr.(i1) h.arr.(i2)

  let swap h i1 i2 =
    let t = h.arr.(i1) in
    h.arr.(i1) <- h.arr.(i2);
    h.arr.(i2) <- t

  let rec up h i =
    if i = 0 then ()
    else
      let p = (i - 1) / 2 in
      if compare h i p < 0 then begin
        swap h i p;
        up h p
      end

  let rec down h i =
    let l = 2 * i + 1 in
    let r = 2 * i + 2 in
    if l >= h.len then ()
    else
      let child =
        if r >= h.len then l
        else if compare h l r < 0 then l else r in
      if compare h i child > 0 then begin
        swap h i child;
        down h child
      end

  let add h e =
    if h.len = Array.length h.arr
    then begin
      let len = 2 * h.len + 1 in
      let arr' = Array.make len (Obj.magic 0) in
      Array.blit h.arr 0 arr' 0 h.len;
      h.arr <- arr'
    end;
    h.arr.(h.len) <- e;
    up h h.len;
    h.len <- h.len + 1

  let peek_min h =
    match h.len with
      | 0 -> None
      | _ -> Some h.arr.(0)

  let take_min h =
    match h.len with
      | 0 -> raise Not_found
      | 1 ->
          let m = h.arr.(0) in
          h.arr.(0) <- (Obj.magic 0);
          h.len <- 0;
          m
      | k ->
          let m = h.arr.(0) in
          let k = k - 1 in
          h.arr.(0) <- h.arr.(k);
          h.arr.(k) <- (Obj.magic 0);
          h.len <- k;
          down h 0;
          m

  let size h = h.len
end
