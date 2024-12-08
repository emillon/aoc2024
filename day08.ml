type t =
  { antennas : char Map.M(Vec).t
  ; bounds : Vec.bounding_box
  }
[@@deriving sexp]

let parse s =
  let lines = String.split_lines s in
  let antennas =
    List.foldi
      lines
      ~init:(Map.empty (module Vec))
      ~f:(fun j acc s ->
        String.foldi s ~init:acc ~f:(fun i acc c ->
          let pos = i, j in
          match c with
          | '.' -> acc
          | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' -> Map.add_exn acc ~key:pos ~data:c
          | _ -> raise_s [%message (c : char) (pos : Vec.t)]))
  in
  let bounds =
    { Vec.min = Vec.zero
    ; max = String.length (List.hd_exn lines) - 1, List.length lines - 1
    }
  in
  { antennas; bounds }
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
  print_s [%message (t : t) (p1 t : int)];
  [%expect
    {|
    ((t
      ((antennas
        (((4 4) 0) ((5 2) 0) ((6 5) A) ((7 3) 0) ((8 1) 0) ((8 8) A) ((9 9) A)))
       (bounds ((min (0 0)) (max (11 11))))))
     ("p1 t" 14))
    |}]
;;

let f1 s = parse s |> p1
let f2 _ = 0
let run () = Run.run ~f1 ~f2 Day08_input.data
