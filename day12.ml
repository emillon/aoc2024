type t = char Map.M(Vec).t [@@deriving sexp]

let parse =
  Vec.parse_2d
    ~init:(Map.empty (module Vec))
    ~f:(fun pos acc c ->
      assert (Char.is_uppercase c);
      Map.add_exn acc ~key:pos ~data:c)
;;

let find_region_at t pos0 =
  let c = Map.find_exn t pos0 in
  let q = Queue.create () in
  Queue.enqueue q pos0;
  let region = ref (Set.empty (module Vec)) in
  while not (Queue.is_empty q) do
    let pos = Queue.dequeue_exn q in
    if not (Set.mem !region pos)
    then (
      match Map.find t pos with
      | Some c' when Char.equal c c' ->
        region := Set.add !region pos;
        Queue.enqueue_all q (Vec.neighbours4 pos)
      | Some _ | None -> ())
  done;
  c, !region
;;

let corners r pos =
  let get v = Set.mem r (Vec.add pos v) in
  let base, f =
    match
      ( get (1, 0)
      , get (1, 1)
      , get (0, 1)
      , get (-1, 1)
      , get (-1, 0)
      , get (-1, -1)
      , get (0, -1)
      , get (1, -1) )
    with
    | true, _, false, _, false, _, false, _
    | false, _, false, _, false, _, true, _
    | false, _, false, _, true, _, false, _
    | false, _, true, _, false, _, false, _ -> 2, []
    | false, _, true, f1, true, f2, true, _
    | true, f1, true, f2, true, _, false, _
    | true, f2, true, _, false, _, true, f1
    | true, _, false, _, true, f1, true, f2 -> 0, [ f1; f2 ]
    | false, _, true, _, false, _, true, _ | true, _, false, _, true, _, false, _ -> 0, []
    | false, _, true, f, true, _, false, _
    | true, f, true, _, false, _, false, _
    | true, _, false, _, false, _, true, f
    | false, _, false, _, true, f, true, _ -> 1, [ f ]
    | true, f1, true, f2, true, f3, true, f4 -> 0, [ f1; f2; f3; f4 ]
    | false, _, false, _, false, _, false, _ -> 4, []
  in
  base + List.count f ~f:not
;;

type info =
  { char : char
  ; area : int
  ; perimeter : int
  ; sides : int
  }
[@@deriving sexp]

let region_info (char, r) =
  let area = Set.length r in
  let perimeter =
    Set.fold r ~init:0 ~f:(fun acc pos ->
      acc + List.count (Vec.neighbours4 pos) ~f:(fun pos -> not (Set.mem r pos)))
  in
  let sides = Set.fold r ~init:0 ~f:(fun acc pos -> acc + corners r pos) in
  { char; area; perimeter; sides }
;;

let regions t =
  let remaining = Map.keys t |> Set.of_list (module Vec) |> ref in
  let regions = Queue.create () in
  while not (Set.is_empty !remaining) do
    let pos = Set.choose_exn !remaining in
    let region = find_region_at t pos in
    Queue.enqueue regions (region_info region);
    remaining := Set.diff !remaining (snd region)
  done;
  Queue.to_list regions
;;

let price { area; perimeter; _ } = area * perimeter
let price_with_discount { area; sides; _ } = area * sides
let p1 t = regions t |> List.fold ~init:0 ~f:(fun acc i -> acc + price i)
let p2 t = regions t |> List.fold ~init:0 ~f:(fun acc i -> acc + price_with_discount i)

let%expect_test _ =
  let t =
    [ "RRRRIICCFF"
    ; "RRRRIICCCF"
    ; "VVRRRCCFFF"
    ; "VVRCCCJFFF"
    ; "VVVVCJJCFE"
    ; "VVIVCCJJEE"
    ; "VVIIICJJEE"
    ; "MIIIIIJJEE"
    ; "MIIISIJEEE"
    ; "MMMISSJEEE"
    ]
    |> String.concat_lines
    |> parse
  in
  let info = regions t in
  print_s [%message (info : info list) (p1 t : int) (p2 t : int)];
  [%expect
    {|
    ((info
      (((char J) (area 11) (perimeter 20) (sides 12))
       ((char C) (area 14) (perimeter 28) (sides 22))
       ((char F) (area 10) (perimeter 18) (sides 12))
       ((char C) (area 1) (perimeter 4) (sides 4))
       ((char E) (area 13) (perimeter 18) (sides 8))
       ((char R) (area 12) (perimeter 18) (sides 10))
       ((char V) (area 13) (perimeter 20) (sides 10))
       ((char I) (area 14) (perimeter 22) (sides 16))
       ((char I) (area 4) (perimeter 8) (sides 4))
       ((char S) (area 3) (perimeter 8) (sides 6))
       ((char M) (area 5) (perimeter 12) (sides 6))))
     ("p1 t" 1930) ("p2 t" 1206))
    |}]
;;

let f1 s = parse s |> p1
let f2 s = parse s |> p2
let run () = Run.run ~f1 ~f2 Day12_input.data
