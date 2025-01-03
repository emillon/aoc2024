type tile =
  | Wall
  | Box
  | BoxL
  | BoxR
[@@deriving sexp]

type dir =
  | U
  | D
  | L
  | R
[@@deriving sexp]

let dir_vec = function
  | U -> 0, -1
  | D -> 0, 1
  | L -> -1, 0
  | R -> 1, 0
;;

type t =
  { tiles : tile Map.M(Vec).t
  ; pos : Vec.t
  ; moves : dir list
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
           | '[' -> Map.add_exn acc_map ~key:pos ~data:BoxL, acc_pos
           | ']' -> Map.add_exn acc_map ~key:pos ~data:BoxR, acc_pos
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
    | '^' -> Some U
    | 'v' -> Some D
    | '<' -> Some L
    | '>' -> Some R
    | '\n' -> None
    | c -> raise_s [%message "parse_moves" (c : char)])
;;

let expand =
  String.concat_map ~f:(function
    | '#' -> "##"
    | '.' -> ".."
    | 'O' -> "[]"
    | '@' -> "@."
    | ('\n' | '^' | 'v' | '<' | '>') as c -> String.of_char c
    | c -> raise_s [%message "expand" (c : char)])
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

let move_all t l dir =
  let open Option.Let_syntax in
  let rec move_one src =
    let dir_vec = dir_vec dir in
    let dst = Vec.add src dir_vec in
    match Map.find t.tiles dst, dir with
    | None, _ -> Some (Set.singleton (module Vec) src)
    | Some Wall, _ -> None
    | Some Box, _ | Some BoxL, L | Some BoxR, R ->
      let%map m = move_one dst in
      Set.add m src
    | Some BoxL, (U | D | R) -> move_3 src dst (Vec.add dst (1, 0))
    | Some BoxR, (U | D | L) -> move_3 src dst (Vec.add dst (-1, 0))
  and move_3 src s1 s2 =
    let%map m1 = move_one s1
    and m2 = move_one s2 in
    Set.union m1 (Set.add m2 src)
  in
  List.fold
    l
    ~init:(Some (Set.empty (module Vec)))
    ~f:(fun acc_o pos ->
      let open Option.Let_syntax in
      let%map acc = acc_o
      and l = move_one pos in
      Set.union l acc)
;;

let order set dir =
  let x = Comparable.lift Int.compare ~f:fst in
  let y = Comparable.lift Int.compare ~f:snd in
  let ascending cmp = cmp in
  let descending cmp = Comparable.reverse cmp in
  let compare =
    match dir with
    | R -> ascending x
    | D -> ascending y
    | L -> descending x
    | U -> descending y
  in
  Set.to_list set |> List.sort ~compare
;;

let try_move t dir =
  let dir_vec = dir_vec dir in
  let pos = Vec.add t.pos dir_vec in
  let move l =
    match move_all t l dir with
    | None -> None
    | Some set ->
      let tiles =
        order set dir
        |> List.fold_right ~init:t.tiles ~f:(fun src acc ->
          let data = Map.find_exn acc src in
          let dst = Vec.add src dir_vec in
          let acc = Map.add_exn acc ~key:dst ~data in
          Map.remove acc src)
      in
      Some { t with pos; tiles }
  in
  match Map.find t.tiles pos with
  | None -> Some { t with pos }
  | Some Wall -> None
  | Some Box -> move [ pos ]
  | Some BoxL -> move [ pos; Vec.add pos (1, 0) ]
  | Some BoxR -> move [ pos; Vec.add pos (-1, 0) ]
;;

let move t dir = try_move t dir |> Option.value ~default:t
let gps (x, y) = (100 * y) + x

let p1 t =
  let final = List.fold t.moves ~init:t ~f:move in
  Map.fold final.tiles ~init:0 ~f:(fun ~key ~data acc ->
    match data with
    | Box | BoxL -> acc + gps key
    | Wall | BoxR -> acc)
;;

let parse2 s = s |> expand |> parse

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
       (pos (2 2)) (moves (L U U R R R D D L D R R D L L))))
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

let display t =
  let { Vec.min = xmin, ymin; max = xmax, ymax } = Vec.bounding_box_map t.tiles in
  for y = ymin to ymax do
    for x = xmin to xmax do
      let pos = x, y in
      let c =
        match Map.find t.tiles pos with
        | _ when Vec.equal pos t.pos -> '@'
        | Some Wall -> '#'
        | Some Box -> 'O'
        | Some BoxL -> '['
        | Some BoxR -> ']'
        | None -> '.'
      in
      printf "%c" c
    done;
    printf "\n"
  done
;;

let%expect_test _ =
  let r =
    ref
      (parse2
         (String.concat_lines
            [ "#######"
            ; "#...#.#"
            ; "#.....#"
            ; "#..OO@#"
            ; "#..O..#"
            ; "#.....#"
            ; "#######"
            ; ""
            ; "<vv<<^^<<^^"
            ]))
  in
  let q = Queue.of_list !r.moves in
  let go () =
    let m = Queue.dequeue_exn q in
    r := move !r m;
    display !r
  in
  display !r;
  [%expect
    {|
    ##############
    ##......##..##
    ##..........##
    ##....[][]@.##
    ##....[]....##
    ##..........##
    ##############
    |}];
  go ();
  [%expect
    {|
    ##############
    ##......##..##
    ##..........##
    ##...[][]@..##
    ##....[]....##
    ##..........##
    ##############
    |}];
  go ();
  [%expect
    {|
    ##############
    ##......##..##
    ##..........##
    ##...[][]...##
    ##....[].@..##
    ##..........##
    ##############
    |}];
  go ();
  [%expect
    {|
    ##############
    ##......##..##
    ##..........##
    ##...[][]...##
    ##....[]....##
    ##.......@..##
    ##############
        |}];
  go ();
  [%expect
    {|
    ##############
    ##......##..##
    ##..........##
    ##...[][]...##
    ##....[]....##
    ##......@...##
    ##############
    |}];
  go ();
  [%expect
    {|
    ##############
    ##......##..##
    ##..........##
    ##...[][]...##
    ##....[]....##
    ##.....@....##
    ##############
    |}];
  go ();
  [%expect
    {|
    ##############
    ##......##..##
    ##...[][]...##
    ##....[]....##
    ##.....@....##
    ##..........##
    ##############
    |}]
;;

let f1 s = parse s |> p1
let f2 s = parse2 s |> p1
let run () = Run.run ~f1 ~f2 Day15_input.data
