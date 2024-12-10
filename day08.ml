type t =
  { antennas : char Map.M(Vec).t
  ; bounds : Vec.bounding_box
  }
[@@deriving sexp]

let extend_bounds t (x, y) =
  let xmax, ymax = t.bounds.max in
  { t with bounds = { t.bounds with max = Int.max xmax x, Int.max ymax y } }
;;

let parse s =
  Vec.parse_2d
    s
    ~init:
      { antennas = Map.empty (module Vec); bounds = { min = Vec.zero; max = Vec.zero } }
    ~f:(fun pos acc c ->
      let acc = extend_bounds acc pos in
      match c with
      | '.' -> acc
      | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' ->
        { acc with antennas = Map.add_exn acc.antennas ~key:pos ~data:c }
      | _ -> raise_s [%message (c : char) (pos : Vec.t)])
;;

let pairs t =
  let l = Map.to_alist t.antennas in
  let open List.Let_syntax in
  let%bind pa, fa = l in
  let%bind pb, fb = l in
  let%bind () = Algo.guard (Char.equal fa fb) in
  let%bind () = Algo.guard (not (Vec.equal pa pb)) in
  return (pa, pb)
;;

let antinode_location (a, b) = Vec.add b (Vec.sub b a)

let p1 t =
  let is_in_bounds v = Vec.in_bounds t.bounds v in
  pairs t
  |> List.map ~f:antinode_location
  |> List.dedup_and_sort ~compare:Vec.compare
  |> List.count ~f:is_in_bounds
;;

let all_antinode_locations bounds (a, b) =
  let d = Vec.sub b a in
  let rec go i =
    let l = Vec.add b (Vec.smul d i) in
    if Vec.in_bounds bounds l then l :: go (i + 1) else []
  in
  go 0
;;

let p2 t =
  pairs t
  |> List.concat_map ~f:(all_antinode_locations t.bounds)
  |> Set.of_list (module Vec)
  |> Set.length
;;

let%expect_test _ =
  let t =
    String.concat_lines
      [ "............"
      ; "........0..."
      ; ".....0......"
      ; ".......0...."
      ; "....0......."
      ; "......A....."
      ; "............"
      ; "............"
      ; "........A..."
      ; ".........A.."
      ; "............"
      ; "............"
      ]
    |> parse
  in
  print_s [%message (t : t) (p1 t : int) (p2 t : int)];
  [%expect
    {|
    ((t
      ((antennas
        (((4 4) 0) ((5 2) 0) ((6 5) A) ((7 3) 0) ((8 1) 0) ((8 8) A) ((9 9) A)))
       (bounds ((min (0 0)) (max (11 11))))))
     ("p1 t" 14) ("p2 t" 34))
    |}]
;;

let f1 s = parse s |> p1
let f2 s = parse s |> p2
let run () = Run.run ~f1 ~f2 Day08_input.data
