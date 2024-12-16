type particle =
  { p : Vec.t
  ; v : Vec.t
  }
[@@deriving sexp]

type t = particle list [@@deriving sexp]

let parse =
  let open Parsing_util in
  let open Angstrom in
  let vec = both (signed_number <* string ",") signed_number in
  parse_lines_using
    (let+ p = string "p=" *> vec
     and+ v = string " v=" *> vec in
     { p; v })
;;

let danger l ~dimx ~dimy =
  let midx = (dimx - 1) / 2 in
  let midy = (dimy - 1) / 2 in
  let low_x (x, _) = x < midx in
  let high_x (x, _) = x > midx in
  let low_y (_, y) = y < midy in
  let high_y (_, y) = y > midy in
  let q1 = List.count l ~f:(fun v -> low_x v && high_y v) in
  let q2 = List.count l ~f:(fun v -> high_x v && high_y v) in
  let q3 = List.count l ~f:(fun v -> low_x v && low_y v) in
  let q4 = List.count l ~f:(fun v -> high_x v && low_y v) in
  q1 * q2 * q3 * q4
;;

let sim100 t ~dimx ~dimy =
  List.map t ~f:(fun { p; v } ->
    let x, y = Vec.add p (Vec.smul v 100) in
    x % dimx, y % dimy)
  |> danger ~dimx ~dimy
;;

let p1 = sim100 ~dimx:101 ~dimy:103

let p2 t0 =
  let dimx = 101 in
  let dimy = 103 in
  let r = ref t0 in
  let lowest_danger = ref Int.max_value in
  let lowest_danger_i = ref Int.max_value in
  for i = 0 to dimx * dimy do
    let t = !r in
    let d = danger (List.map t ~f:(fun p -> p.p)) ~dimx ~dimy in
    if d < !lowest_danger
    then (
      lowest_danger := d;
      lowest_danger_i := i);
    r
    := List.map t ~f:(fun { p; v } ->
         let x, y = Vec.add p v in
         let p = x % dimx, y % dimy in
         { p; v })
  done;
  !lowest_danger_i
;;

let f1 s = parse s |> p1
let f2 s = parse s |> p2
let run () = Run.run ~f1 ~f2 Day14_input.data
