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
