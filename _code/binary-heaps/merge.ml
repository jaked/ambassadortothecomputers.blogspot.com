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
