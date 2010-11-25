---
layout: post
title: Three uses for a binary heap
---

Lately I have been interviewing for jobs, so doing a lot of whiteboard
programming, and
[binary heaps](http://en.wikipedia.org/wiki/Binary_heap)
keep arising in the solutions to these interview problems. There is
nothing new or remarkable about these applications (binary heaps and
their uses are covered in any undergraduate algorithms class), but I
thought I would write them down because they are cute, and in the hope
that they might be useful to someone else who (like me) gets by most
days as a working programmer with no algorithm fancier than quicksort
or binary search.

<b>Binary heaps</b>

Here's a signature for a binary heap module `Heap`:

{% highlight ocaml %}
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

module Make (O : OrderedType) : S with type elt = O.t
{% endhighlight %}

We start with a signature for ordered types (following the `Set` and
`Map` modules in the standard library), so we can provide a
type-specific comparison function.

From an ordered type we can make a heap which supports adding
elements, peeking the smallest element (`None` if there are no
elements) without removing it, removing and returning the smallest
element (raising `Not_found` if the heap is empty), and returning the
number of elements.

We'll work out the asymptotic running times of the algorithms below,
so it will be useful to know that the worst-case running time of the
`add` and `take_min` functions is `O(log n)` where `n` is the number
of elements in the heap.

<b>Finding the k smallest elements in a list</b>

Here's a simple one. To find the smallest element in a list, we could
sort the list then take the first element in the sorted list, at a
cost of `O(log n)`. Or we could just take a pass over the list
keeping a running minimum, at a cost of `O(n)`.

What if we want the `k` smallest elements? Again, we could sort the
list, but if `k < n` we can do better by generalizing the single-pass
solution. The idea is to keep the `k` smallest elements we've seen so
far in a binary heap. For each element in the list we add it to the
heap, then (if there were already `k` elements in the heap) remove the
largest element in the heap, leaving the `k` smallest.

The running time is `O(n log k)` since we do an `add` and a `take_min`
in a heap of size `k` for each of `n` elements in the list. Here's the
code:

{% highlight ocaml %}
let kmin (type s) k l =
  let module OT = struct
    type t = s
    let compare e1 e2 = compare e2 e1
  end in
  let module H = Heap.Make(OT) in

  let h = H.make () in
  List.iter
    (fun e ->
       H.add h e;
       if H.size h > k
       then ignore (H.take_min h))
    l;
  let rec loop mins =
    match H.peek_min h with
      | None -> mins
      | _ -> loop (H.take_min h :: mins) in
  loop []
{% endhighlight %}

Here we make good use of OCaml 3.12's new feature for
[explicitly naming type variables](http://caml.inria.fr/pub/docs/manual-ocaml/manual021.html#toc80)
in a polymorphic function to make a structure matching
`OrderedType`. The heap has the same element type as the list, but we
reverse the comparison since we want to remove the largest rather than
smallest element from the heap in the loop. At the end of `kmin` we
drain the heap to build a list of the `k` smallest elements.

<b>Merging k lists</b>

Suppose we want to merge `k` lists. We could merge them pairwise until
there is only one list, but that would take `k - 1` passes, for a
worst-case running time of `O(n * (k - 1))`. Instead we can merge them
all in one pass, using a binary heap so we can find the next smallest
element of `k` lists in `O(log k)` time, for a running time of `O(n
log k)`. Here's the code:

{% highlight ocaml %}
let merge (type s) ls =
  let module OT = struct
    type t = s list
    let compare e1 e2 =
      compare (List.hd e1) (List.hd e2)
  end in
  let module H = Heap.Make(OT) in

  let h = H.make () in
  let add = function
    | [] -> ()
    | l -> H.add h l in
  List.iter add ls;
  let rec loop () =
    match H.peek_min h with
      | None -> []
      | _ ->
          match H.take_min h with
            | [] -> assert false
            | m :: t ->
                add t;
                m :: loop () in
  loop ()
{% endhighlight %}

We store the lists in the heap, and compare them by comparing their
head element (we're careful not to put an empty list in the
heap). When we take the smallest list from the heap, its head becomes
the next element in the output list, and we return its tail (if it is
not empty) to the heap.

<b>Computing a skyline</b>

The next problem was told to me in terms of computing the skyline of a
set of buildings. A building has a height and a starting and ending
`x`-coordinate; buildings may overlap. The skyline of a set of
buildings is a list of (`x`, `y`) pairs (in ascending `x` order),
describing a sequence of horizontal line segments (each starting at
(`x`, `y`) and ending at the subsequent `x`), such that at any `x`
there is no space between the line segment and the tallest
building. (Here's
[another description](http://stackoverflow.com/questions/1066234/the-skyline-problem)
with diagrams.)

I googled a bit to see what this is useful for, and didn't find much.
One application is to extract a monophonic line from polyphonic music,
where `x` is time and height is some metric on notes, like pitch or
volume. It might be useful for searching data which is only
intermittently applicable---say, to compute a schedule over time of
the nearest open restaurant.

The algorithm scans the building start and end points in ascending `x`
order, keeping the "active" buildings (those which overlap the current
`x`) in a binary heap. The height of the skyline can only change at a
building start or end point. We can determine the tallest building at
a point by calling `peek_min` on the heap.

When we hit a start point we add the building to the heap; for an end
point we do nothing (the heap has no operation to remove an
element). So we may have inactive buildings in the heap. We remove
them lazily---before checking the height of the highest building, we
call `take_min` to remove any higher inactive buildings.

The worst-case running time is `O(n log n)`, since we do some heap
operations for each building, and we might end up with all the
buildings in the heap.

Here's the code:

{% highlight ocaml %}
type building = int * int * int (* x0, x1, h *)

let skyline bs =
  let module OT = struct
    type t = int * building
    let compare (x1, _) (x2, _) = compare x1 x2
  end in
  let module Events = Heap.Make(OT) in
  let events = Events.make () in
  List.iter
    (fun ((x0,x1,_) as b) ->
       Events.add events (x0, b);
       Events.add events (x1, b))
    bs;

  let module OT = struct
    type t = building
    let compare (_,_,h1) (_,_,h2) = compare h2 h1
  end in
  let module Heights = Heap.Make(OT) in
  let heights = Heights.make () in

  let rec loop last =
    match Events.peek_min events with
      | None -> []
      | _ ->
          let (x, (x0,_,h as b)) = Events.take_min events in
          if x = x0 then Heights.add heights b;
          while (match Heights.peek_min heights with
                   | Some (_,x1,_) -> x1 <= x
                   | _ -> false) do
            ignore (Heights.take_min heights)
          done;
          let h =
            match Heights.peek_min heights with
              | Some (_,_,h) -> h
              | None -> 0 in
          match last with
            | Some h' when h = h' -> loop last
            | _ -> (x, h) :: loop (Some h) in
  loop None
{% endhighlight %}

We use a second heap `events` to store the "events" (the start and end
points of all the buildings), in order to process them in ascending
`x` order. (This use is not dynamic---we do not add new elements to
the heap while processing them---so we could just as well use another
means of sorting the points.) In this heap we store the `x` coordinate
and the building (we can tell whether we have a start or end point by
comparing the `x` coordinate to the building's start point), and
compare elements by comparing just the `x` coordinates.

The main heap `heights` stores buildings, and we compare them by
comparing heights (reversed, so `peek_min` peeks the tallest
building). While there are still events, we add the building to
`heights` if the event is a start point, clear out inactive buildings,
then return the pair (`x`, `y`) where `x` is the point we're
processing and `y` is the height of the tallest active
building. Additionally we filter out adjacent pairs with the same
height; these can arise when a shorter building starts or ends while a
taller building is active.

<b>Implementing binary heaps</b>

The following implementation is derived from the one in Daniel
Bünzli's [React](http://erratique.ch/software/react) library (edited a
little bit for readability). The [Wikipedia article on binary
heaps](http://en.wikipedia.org/wiki/Binary_heap)
explains the standard technique well, so I won't repeat it.

The only piece of trickiness is the use of `Obj.magic 0` for unused
elements of the array, so we can grow it by doubling the size rather
than adding a single element each time, and thereby amortize the cost
of blitting the old array.

{% highlight ocaml %}
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
{% endhighlight %}

(Complete code is [here](https://github.com/jaked/ambassadortothecomputers.blogspot.com/tree/master/_code/binary-heaps).)
