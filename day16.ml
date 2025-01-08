type t =
  { paths : Set.M(Vec).t
  ; end_ : Vec.t
  ; dir : Vec.t
  ; pos : Vec.t
  }
[@@deriving compare, sexp]

module State = struct
  module T = struct
    type nonrec t = t [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let parse s =
  let paths, start_o, end_o =
    Vec.parse_2d
      s
      ~init:(Set.empty (module Vec), None, None)
      ~f:(fun pos (paths, start_o, end_o) c ->
        match c with
        | '#' -> paths, start_o, end_o
        | '.' -> Set.add paths pos, start_o, end_o
        | 'S' ->
          assert (Option.is_none start_o);
          Set.add paths pos, Some pos, end_o
        | 'E' ->
          assert (Option.is_none end_o);
          Set.add paths pos, start_o, Some pos
        | _ -> raise_s [%message (c : char) (pos : Vec.t)])
  in
  { paths; pos = Option.value_exn start_o; end_ = Option.value_exn end_o; dir = 1, 0 }
;;

let is_valid t = Set.mem t.paths t.pos
let is_winning t = Vec.equal t.pos t.end_

let next t =
  [ { t with pos = Vec.add t.pos t.dir }, 1
  ; { t with dir = Vec.cmul t.dir (0, 1) }, 1000
  ; { t with dir = Vec.cmul t.dir (0, -1) }, 1000
  ]
;;

let p1 t0 =
  let q = Pairing_heap.create ~cmp:(Comparable.lift Int.compare ~f:snd) () in
  Pairing_heap.add q (t0, 0);
  let seen_set = ref (Set.empty (module State)) in
  let seen t = Set.mem !seen_set t in
  let mark_seen t = seen_set := Set.add !seen_set t in
  let exception Found of int in
  try
    while not (Pairing_heap.is_empty q) do
      let t, score = Pairing_heap.pop_exn q in
      if is_valid t && not (seen t)
      then (
        if is_winning t then raise (Found score);
        mark_seen t;
        next t
        |> List.map ~f:(fun (t', cost) -> t', score + cost)
        |> List.iter ~f:(Pairing_heap.add q))
    done;
    assert false
  with
  | Found n -> n
;;

let snd3 (_, b, _) = b

let p2 t0 =
  let q = Pairing_heap.create ~cmp:(Comparable.lift Int.compare ~f:snd3) () in
  Pairing_heap.add q (t0, 0, Set.singleton (module Vec) t0.pos);
  let best_paths = ref (Set.empty (module Vec)) in
  let best_scores = ref (Map.empty (module State)) in
  let best_total_score = ref None in
  let best_score t = Map.find !best_scores t in
  let set_best_score t score = Ref.replace best_scores (Map.set ~key:t ~data:score) in
  let exception Done in
  (try
     while not (Pairing_heap.is_empty q) do
       let t, score, trace = Pairing_heap.pop_exn q in
       (match !best_total_score with
        | Some best when score > best -> raise Done
        | _ -> ());
       if is_winning t
       then (
         best_total_score := Some score;
         Ref.replace best_paths (Set.union trace));
       if is_valid t
       then (
         let can_visit =
           match best_score t with
           | Some prev_score when prev_score < score -> false
           | _ ->
             set_best_score t score;
             true
         in
         if can_visit
         then
           next t
           |> List.map ~f:(fun (t', cost) -> t', score + cost, Set.add trace t'.pos)
           |> List.iter ~f:(Pairing_heap.add q))
     done
   with
   | Done -> ());
  Set.length !best_paths
;;

let%expect_test _ =
  let t1 =
    parse
      (String.concat_lines
         [ "###############"
         ; "#.......#....E#"
         ; "#.#.###.#.###.#"
         ; "#.....#.#...#.#"
         ; "#.###.#####.#.#"
         ; "#.#.#.......#.#"
         ; "#.#.#####.###.#"
         ; "#...........#.#"
         ; "###.#.#####.#.#"
         ; "#...#.....#.#.#"
         ; "#.#.#.###.#.#.#"
         ; "#.....#...#.#.#"
         ; "#.###.#.#.#.#.#"
         ; "#S..#.....#...#"
         ; "###############"
         ])
  in
  let t2 =
    parse
      (String.concat_lines
         [ "#################"
         ; "#...#...#...#..E#"
         ; "#.#.#.#.#.#.#.#.#"
         ; "#.#.#.#...#...#.#"
         ; "#.#.#.#.###.#.#.#"
         ; "#...#.#.#.....#.#"
         ; "#.#.#.#.#.#####.#"
         ; "#.#...#.#.#.....#"
         ; "#.#.#####.#.###.#"
         ; "#.#.#.......#...#"
         ; "#.#.###.#####.###"
         ; "#.#.#...#.....#.#"
         ; "#.#.#.#####.###.#"
         ; "#.#.#.........#.#"
         ; "#.#.#.#########.#"
         ; "#S#.............#"
         ; "#################"
         ])
  in
  print_s [%message (p1 t1 : int) (p1 t2 : int) (p2 t1 : int) (p2 t2 : int)];
  [%expect {| (("p1 t1" 7036) ("p1 t2" 11048) ("p2 t1" 45) ("p2 t2" 64)) |}]
;;

let f1 s = parse s |> p1
let f2 s = parse s |> p2
let run () = Run.run ~f1 ~f2 Day16_input.data
