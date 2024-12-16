type tile =
  | Wall
  | Box
[@@deriving sexp]

type t =
  { tiles : tile Map.M(Vec).t
  ; pos : Vec.t
  ; moves : Vec.t list
  }
[@@deriving sexp]

let parse_tiles l =
  let map, pos_opt =
    String.concat_lines l
    |> Vec.parse_2d
         ~init:(Map.empty (module Vec), None)
         ~f:(fun pos (acc_map, acc_pos) c ->
           match c with
           | '#' -> Map.add_exn acc_map ~key:pos ~data:Wall, acc_pos
           | 'O' -> Map.add_exn acc_map ~key:pos ~data:Box, acc_pos
           | '@' ->
             assert (Option.is_none acc_pos);
             acc_map, Some pos
           | '.' -> acc_map, acc_pos
           | _ -> raise_s [%message "parse_tiles" (c : char)])
  in
  map, Option.value_exn pos_opt
;;

let parse_moves l =
  String.concat_lines l
  |> String.to_list
  |> List.filter_map ~f:(function
    | '^' -> Some (0, -1)
    | 'v' -> Some (0, 1)
    | '<' -> Some (-1, 0)
    | '>' -> Some (1, 0)
    | '\n' -> None
    | c -> raise_s [%message "parse_moves" (c : char)])
;;

let parse s =
  let lines = String.split_lines s in
  let tile_lines, other_lines = List.split_while lines ~f:(Fn.non String.is_empty) in
  let move_lines =
    match other_lines with
    | "" :: l -> l
    | _ -> assert false
  in
  let tiles, pos = parse_tiles tile_lines in
  { tiles; pos; moves = parse_moves move_lines }
;;

let rec push_box tiles pos dir =
  let pos' = Vec.add pos dir in
  match Map.find tiles pos' with
  | None -> Some (Map.add_exn tiles ~key:pos' ~data:Box)
  | Some Wall -> None
  | Some Box -> push_box tiles pos' dir
;;

let move t dir =
  let pos = Vec.add t.pos dir in
  match Map.find t.tiles pos with
  | None -> { t with pos }
  | Some Wall -> t
  | Some Box ->
    (let open Option.Let_syntax in
     let%map tiles = push_box t.tiles pos dir in
     let tiles = Map.remove tiles pos in
     { t with pos; tiles })
    |> Option.value ~default:t
;;

let gps (x, y) = (100 * y) + x

let p1 t =
  let final = List.fold t.moves ~init:t ~f:move in
  Map.fold final.tiles ~init:0 ~f:(fun ~key ~data acc ->
    match data with
    | Box -> acc + gps key
    | Wall -> acc)
;;

let%expect_test _ =
  let t =
    parse
      (String.concat_lines
         [ "########"
         ; "#..O.O.#"
         ; "##@.O..#"
         ; "#...O..#"
         ; "#.#.O..#"
         ; "#...O..#"
         ; "#......#"
         ; "########"
         ; ""
         ; "<^^>>>vv<v>>v<<"
         ])
  in
  print_s [%message (t : t) (p1 t : int)];
  [%expect
    {|
    ((t
      ((tiles
        (((0 0) Wall) ((0 1) Wall) ((0 2) Wall) ((0 3) Wall) ((0 4) Wall)
         ((0 5) Wall) ((0 6) Wall) ((0 7) Wall) ((1 0) Wall) ((1 2) Wall)
         ((1 7) Wall) ((2 0) Wall) ((2 4) Wall) ((2 7) Wall) ((3 0) Wall)
         ((3 1) Box) ((3 7) Wall) ((4 0) Wall) ((4 2) Box) ((4 3) Box)
         ((4 4) Box) ((4 5) Box) ((4 7) Wall) ((5 0) Wall) ((5 1) Box)
         ((5 7) Wall) ((6 0) Wall) ((6 7) Wall) ((7 0) Wall) ((7 1) Wall)
         ((7 2) Wall) ((7 3) Wall) ((7 4) Wall) ((7 5) Wall) ((7 6) Wall)
         ((7 7) Wall)))
       (pos (2 2))
       (moves
        ((-1 0) (0 -1) (0 -1) (1 0) (1 0) (1 0) (0 1) (0 1) (-1 0) (0 1)
         (1 0) (1 0) (0 1) (-1 0) (-1 0)))))
     ("p1 t" 2028))
    |}];
  let t2 =
    parse
      (String.concat_lines
         [ "##########"
         ; "#..O..O.O#"
         ; "#......O.#"
         ; "#.OO..O.O#"
         ; "#..O@..O.#"
         ; "#O#..O...#"
         ; "#O..O..O.#"
         ; "#.OO.O.OO#"
         ; "#....O...#"
         ; "##########"
         ; ""
         ; "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^"
         ; "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v"
         ; "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<"
         ; "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^"
         ; "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><"
         ; "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^"
         ; ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^"
         ; "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>"
         ; "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>"
         ; "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"
         ])
  in
  print_s [%message (p1 t2 : int)];
  [%expect {| ("p1 t2" 10092) |}]
;;

let f1 s = parse s |> p1
let f2 _ = 0
let run () = Run.run ~f1 ~f2 Day15_input.data
