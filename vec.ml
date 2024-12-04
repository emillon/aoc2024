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
let neighbours4 (x, y) = [ x - 1, y; x + 1, y; x, y - 1; x, y + 1 ]
let smul v n = cmul v (n, 0)
