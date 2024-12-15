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

let sim100 t ~dimx ~dimy =
  let l =
    List.map t ~f:(fun { p; v } ->
      let x, y = Vec.add p (Vec.smul v 100) in
      x % dimx, y % dimy)
  in
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

let p1 = sim100 ~dimx:101 ~dimy:103
let f1 s = parse s |> p1
let f2 _ = 0
let run () = Run.run ~f1 ~f2 Day14_input.data
