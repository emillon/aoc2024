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

let f1 s = parse s |> p1

let%expect_test _ =
  let e1 =
    String.concat_lines
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
      ]
  in
  let e2 =
    String.concat_lines
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
      ]
  in
  print_s [%message (f1 e1 : int) (f1 e2 : int)];
  [%expect {| (("f1 e1" 7036) ("f1 e2" 11048)) |}]
;;

let f2 _ = 0
let run () = Run.run ~f1 ~f2 Day16_input.data
