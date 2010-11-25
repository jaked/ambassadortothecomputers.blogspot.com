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
