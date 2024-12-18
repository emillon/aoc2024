let parse =
  let open Parsing_util in
  let open Angstrom in
  parse_lines_using (both number (char ',' *> number))
;;

let path blocks =
  let bounds = { Vec.min = Vec.zero; max = 70, 70 } in
  let is_winning p = Vec.equal p bounds.max in
  let is_valid p = Vec.in_bounds bounds p && not (Set.mem blocks p) in
  let q = Queue.create () in
  Queue.enqueue q (Vec.zero, Set.empty (module Vec));
  let seen_set = Hash_set.create (module Vec) in
  let seen p = Hash_set.mem seen_set p in
  let mark_as_seen p = Hash_set.add seen_set p in
  let exception Found of Set.M(Vec).t in
  try
    while not (Queue.is_empty q) do
      let p, path = Queue.dequeue_exn q in
      if is_valid p && not (seen p)
      then (
        if is_winning p then raise (Found path);
        mark_as_seen p;
        p
        |> Vec.neighbours4
        |> List.map ~f:(fun p' -> p', Set.add path p')
        |> Queue.enqueue_all q)
    done;
    None
  with
  | Found s -> Some s
;;

let f1 s =
  List.take (parse s) 1024
  |> Set.of_list (module Vec)
  |> path
  |> Option.value_exn
  |> Set.length
  |> Int.to_string
;;

let f2 s =
  let t = parse s in
  let blocks0 = Set.empty (module Vec) in
  let best_path0 = path blocks0 |> Option.value_exn in
  List.fold_until
    t
    ~init:(blocks0, best_path0)
    ~f:(fun (blocks, best_path) block ->
      let blocks' = Set.add blocks block in
      if Set.mem best_path block
      then (
        match path blocks' with
        | None ->
          let x, y = block in
          Stop (Printf.sprintf "%d,%d" x y)
        | Some p -> Continue (blocks', p))
      else Continue (blocks', best_path))
    ~finish:(fun _ -> assert false)
;;

let run () = Run.run_string ~f1 ~f2 Day18_input.data
