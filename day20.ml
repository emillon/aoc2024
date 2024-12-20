type t =
  { paths : Set.M(Vec).t
  ; start : Vec.t
  ; end_ : Vec.t
  }

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
  { paths; start = Option.value_exn start_o; end_ = Option.value_exn end_o }
;;

let shortest_map t start =
  let q = Queue.create () in
  Queue.enqueue q (start, 0);
  let is_valid pos = Set.mem t.paths pos in
  let dist_map = ref (Map.empty (module Vec)) in
  let is_seen pos = Map.mem !dist_map pos in
  let mark_as_seen pos dist = dist_map := Map.add_exn !dist_map ~key:pos ~data:dist in
  while not (Queue.is_empty q) do
    let pos, dist = Queue.dequeue_exn q in
    if is_valid pos && not (is_seen pos)
    then (
      mark_as_seen pos dist;
      Vec.neighbours4 pos
      |> List.map ~f:(fun pos' -> pos', dist + 1)
      |> Queue.enqueue_all q)
  done;
  !dist_map
;;

let shortest t =
  let map = shortest_map t t.start in
  Map.find map t.end_
;;

let l1_ball r =
  let range = List.range ~start:`inclusive ~stop:`inclusive (-r) r in
  let open List.Let_syntax in
  let%bind x = range in
  let%bind y = range in
  let p = x, y in
  if Vec.l1_norm p <= r then return p else []
;;

let all_tunnels t ~skip =
  let open List.Let_syntax in
  let%bind a = Set.to_list t.paths in
  let%bind d = l1_ball skip in
  let b = Vec.add a d in
  if Set.mem t.paths b then return (a, b) else []
;;

let shortcuts t ~skip =
  let score_without_cheating = shortest t |> Option.value_exn in
  let dist_from_s = shortest_map t t.start in
  let dist_from_e = shortest_map t t.end_ in
  all_tunnels t ~skip
  |> List.filter_map ~f:(fun (a, b) ->
    let a_score = Map.find_exn dist_from_s a in
    let b_score = Map.find_exn dist_from_e b in
    let score_with_cheating = a_score + b_score + Vec.l1_norm (Vec.sub b a) in
    let savings = score_without_cheating - score_with_cheating in
    Option.some_if (savings > 0) savings)
;;

type 'a tally =
  { value : 'a
  ; count : int
  }
[@@deriving sexp]

let tally l ~compare =
  List.sort_and_group l ~compare
  |> List.map ~f:(fun g ->
    let value = List.hd_exn g in
    let count = List.length g in
    { value; count })
;;

let%expect_test _ =
  let t =
    parse
      (String.concat_lines
         [ "###############"
         ; "#...#...#.....#"
         ; "#.#.#.#.#.###.#"
         ; "#S#...#.#.#...#"
         ; "#######.#.#.###"
         ; "#######.#.#...#"
         ; "#######.#.###.#"
         ; "###..E#...#...#"
         ; "###.#######.###"
         ; "#...###...#...#"
         ; "#.#####.#.###.#"
         ; "#.#...#.#.#...#"
         ; "#.#.#.#.#.#.###"
         ; "#...#...#...###"
         ; "###############"
         ])
  in
  let info n = shortcuts t ~skip:n |> tally ~compare:Int.compare in
  print_s [%message (info 2 : int tally list)];
  [%expect
    {|
    ("info 2"
     (((value 2) (count 14)) ((value 4) (count 14)) ((value 6) (count 2))
      ((value 8) (count 4)) ((value 10) (count 2)) ((value 12) (count 3))
      ((value 20) (count 1)) ((value 36) (count 1)) ((value 38) (count 1))
      ((value 40) (count 1)) ((value 64) (count 1))))
    |}];
  let p2 = info 20 |> List.filter ~f:(fun t -> t.count >= 50) in
  print_s [%message (p2 : int tally list)];
  [%expect
    {|
    (p2
     (((value 2) (count 138)) ((value 4) (count 329)) ((value 6) (count 122))
      ((value 8) (count 224)) ((value 10) (count 109)) ((value 12) (count 252))
      ((value 14) (count 101)) ((value 16) (count 263)) ((value 18) (count 94))
      ((value 20) (count 217)) ((value 22) (count 76)) ((value 24) (count 129))
      ((value 26) (count 66)) ((value 28) (count 80)) ((value 30) (count 61))
      ((value 32) (count 61)) ((value 34) (count 58)) ((value 36) (count 57))
      ((value 38) (count 51)) ((value 40) (count 93)) ((value 44) (count 99))))
    |}]
;;

let f1 s = parse s |> shortcuts ~skip:2 |> List.count ~f:(fun n -> n >= 100)
let f2 s = parse s |> shortcuts ~skip:20 |> List.count ~f:(fun n -> n >= 100)
let run () = Run.run ~f1 ~f2 Day20_input.data
