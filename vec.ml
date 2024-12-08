module T = struct
  type t = int * int [@@deriving compare, hash, sexp]
end

include T
include Comparable.Make (T)

let l1_norm (x, y) = abs x + abs y
let zero = 0, 0
let one = 1, 0
let i = 0, 1
let cmul (ax, ay) (bx, by) = (ax * bx) - (ay * by), (ay * bx) + (ax * by)
let add (ax, ay) (bx, by) = ax + bx, ay + by
let sub (ax, ay) (bx, by) = ax - bx, ay - by
let neighbours4 (x, y) = [ x - 1, y; x + 1, y; x, y - 1; x, y + 1 ]
let smul v n = cmul v (n, 0)

type bounding_box =
  { min : t
  ; max : t
  }
[@@deriving sexp]

let in_bounds { min = xmin, ymin; max = xmax, ymax } (x, y) =
  let open Int in
  xmin <= x && x <= xmax && ymin <= y && y <= ymax
;;

let bounding_box_map m =
  Map.fold
    m
    ~init:{ min = Int.max_value, Int.max_value; max = Int.min_value, Int.min_value }
    ~f:(fun ~key:(x, y) ~data:_ { min = xmin, ymin; max = xmax, ymax } ->
      let xmin' = Int.min x xmin in
      let ymin' = Int.min y ymin in
      let xmax' = Int.max x xmax in
      let ymax' = Int.max y ymax in
      { min = xmin', ymin'; max = xmax', ymax' })
;;
