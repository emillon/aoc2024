type tile =
  | Empty
  | Full
[@@deriving sexp]

module Pair = struct
  module T = struct
    type t =
      { pos : Vec.t
      ; dir : Vec.t
      }
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

type t =
  { tiles : tile Map.M(Vec).t
  ; pos : Vec.t
  ; dir : Vec.t
  ; visited : Set.M(Pair).t
  }
[@@deriving sexp]

let parse s =
  let dir = 0, -1 in
  let init =
    { tiles = Map.empty (module Vec)
    ; pos = Vec.zero
    ; dir
    ; visited = Set.empty (module Pair)
    }
  in
  Vec.parse_2d s ~init ~f:(fun pos acc c ->
    let tile, set_pos =
      match c with
      | '^' -> Empty, true
      | '.' -> Empty, false
      | '#' -> Full, false
      | _ -> raise_s [%message "parse" (pos : Vec.t) (c : char)]
    in
    let tiles = Map.add_exn acc.tiles ~key:pos ~data:tile in
    if set_pos
    then { acc with tiles; pos; visited = Set.singleton (module Pair) { pos; dir } }
    else { acc with tiles })
;;

type next_result =
  | Continue of t
  | Cycle
  | Exit

let next t =
  let pos = Vec.add t.pos t.dir in
  match Map.find t.tiles pos with
  | Some Empty ->
    if Set.mem t.visited { pos; dir = t.dir }
    then Cycle
    else Continue { t with pos; visited = Set.add t.visited { pos; dir = t.dir } }
  | Some Full -> Continue { t with dir = Vec.cmul t.dir Vec.i }
  | None -> Exit
;;

let rec path t =
  match next t with
  | Continue t' -> path t'
  | Exit -> Some t
  | Cycle -> None
;;

let visited_pos t = t.visited |> Set.map (module Vec) ~f:(fun p -> p.pos)
let p1 t = path t |> Option.value_exn |> visited_pos |> Set.length

let p2 t =
  path t
  |> Option.value_exn
  |> visited_pos
  |> Set.count ~f:(fun pos ->
    if Vec.equal pos t.pos
    then false
    else { t with tiles = Map.set t.tiles ~key:pos ~data:Full } |> path |> Option.is_none)
;;

let%expect_test _ =
  let t =
    parse
      (String.concat_lines
         [ "....#....."
         ; ".........#"
         ; ".........."
         ; "..#......."
         ; ".......#.."
         ; ".........."
         ; ".#..^....."
         ; "........#."
         ; "#........."
         ; "......#..."
         ])
  in
  print_s [%message (p1 t : int) (p2 t : int)];
  [%expect {| (("p1 t" 41) ("p2 t" 6)) |}]
;;

let f1 s = parse s |> p1
let f2 s = parse s |> p2
let run () = Run.run ~f1 ~f2 Day06_input.data
