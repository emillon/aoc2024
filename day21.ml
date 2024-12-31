(* XXX
https://observablehq.com/@jwolondon/advent-of-code-2024-day-21
*)
type numeric =
  | N0
  | N1
  | N2
  | N3
  | N4
  | N5
  | N6
  | N7
  | N8
  | N9
  | NA
[@@deriving equal, sexp]

let to_digit = function
  | N0 -> Some '0'
  | N1 -> Some '1'
  | N2 -> Some '2'
  | N3 -> Some '3'
  | N4 -> Some '4'
  | N5 -> Some '5'
  | N6 -> Some '6'
  | N7 -> Some '7'
  | N8 -> Some '8'
  | N9 -> Some '9'
  | NA -> None
;;

let numeric_value l =
  l |> List.filter_map ~f:to_digit |> String.of_char_list |> Int.of_string
;;

type dir =
  | U
  | D
  | L
  | R
  | A
[@@deriving compare, equal, sexp]

(**
+---+---+---+
| 7 | 8 | 9 |
+---+---+---+
| 4 | 5 | 6 |
+---+---+---+
| 1 | 2 | 3 |
+---+---+---+
    | 0 | A |
    +---+---+
    *)
let numeric_leg ~src ~dst =
  match src, dst with
  (* around blank *)
  | NA, N1 -> [ U; L; L ]
  | NA, N4 -> [ U; U; L; L ]
  | NA, N7 -> [ U; U; U; L; L ]
  (* not around blank *)
  | NA, N0 -> [ L ]
  | N0, N2 -> [ U ]
  | N9, N8 -> [ L ]
  | N0, NA -> [ R ]
  | N4, N5 -> [ R ]
  | N5, N6 -> [ R ]
  | NA, N3 -> [ U ]
  | N2, N9 -> [ U; U; R ]
  | N9, NA -> [ D; D; D ]
  | NA, N9 -> [ U; U; U ]
  | N8, N0 -> [ D; D; D ]
  | N1, N7 -> [ U; U ]
  | N7, N9 -> [ R; R ]
  | N6, NA -> [ D; D ]
  | N3, N7 -> [ L; L; U; U ]
  | NA, N6 -> [ U; U ]
  | N6, N3 -> [ D ]
  | N9, N6 -> [ D ]
  | N3, NA -> [ D ]
  | N6, N5 -> [ L ]
  | N7, N8 -> [ R ]
  | N4, N6 -> [ R; R ]
  | N0, N3 -> [ U; R ]
  | N3, N8 -> [ L; U; U ]
  | N8, NA -> [ D; D; D; R ]
  | N5, NA -> [ D; D; R ]
  | NA, N2 -> [ L; U ]
  | N2, N4 -> [ L; U ]
  | NA, N8 -> [ L; U; U; U ]
  | _ -> raise_s [%message "numeric_leg" (src : numeric) (dst : numeric)]
;;

let numeric l =
  (* XXX dedup *)
  let last_dst, rev_legs =
    List.fold l ~init:(NA, []) ~f:(fun (src, rev_legs) dst ->
      let src' = dst in
      let rev_legs' = [ A ] :: numeric_leg ~src ~dst :: rev_legs in
      src', rev_legs')
  in
  assert ([%equal: numeric] last_dst NA);
  rev_legs |> List.rev |> List.concat
;;

(**
    +---+---+
    | U | A |
+---+---+---+
| L | D | R |
+---+---+---+
*)
let directional_leg ~src ~dst =
  match src, dst with
  (* around blank *)
  | A, L -> [ D; L; L; A ]
  | L, A -> [ R; R; U; A ]
  | U, L -> [ D; L; A ]
  | L, U -> [ R; U; A ]
  (* not around blank *)
  | U, U -> [ A ]
  | D, D -> [ A ]
  | L, L -> [ A ]
  | R, R -> [ A ]
  | A, A -> [ A ]
  | R, D -> [ L; A ]
  | A, U -> [ L; A ]
  | D, L -> [ L; A ]
  | D, R -> [ R; A ]
  | U, A -> [ R; A ]
  | L, D -> [ R; A ]
  | R, A -> [ U; A ]
  | A, R -> [ D; A ]
  | U, R -> [ D; R; A ]
  | A, D -> [ L; D; A ]
  | D, A -> [ U; R; A ]
  | R, U -> [ L; U; A ]
  | _ -> raise_s [%message "directional_leg" (src : dir) (dst : dir)]
;;

module Key = struct
  module T = struct
    type t = dir * dir [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Dmap = struct
  type t = int Map.M(Key).t [@@deriving sexp]

  let of_dir_list l =
    (* XXX dedup with directional ?*)
    let _, final =
      List.fold l ~init:(A, []) ~f:(fun (src, acc) dst -> dst, (src, dst) :: acc)
    in
    let keys = List.rev final in
    (* XXX just one fold *)
    List.fold
      keys
      ~init:(Map.empty (module Key))
      ~f:(Map.update ~f:(fun vo -> vo |> Option.value ~default:0 |> Int.succ))
  ;;

  let length = Map.fold ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data)

  let expand_key = function
    | U, U -> [ A, A ]
    | D, D -> [ A, A ]
    | U, R -> [ A, D; D, R; R, A ]
    | U, A -> [ A, R; R, A ]
    | D, A -> [ A, U; U, R; R, A ]
    | L, A -> [ A, R; R, R; R, U; U, A ]
    | R, A -> [ A, U; U, A ]
    | A, U -> [ A, L; L, A ]
    | A, D -> [ A, L; L, D; D, A ]
    | A, L -> [ A, D; D, L; L, L; L, A ]
    | k -> raise_s [%message "expand_key" (k : Key.t)]
  ;;

  let expand (t : t) : t =
    Map.mapi t ~f:(fun ~key ~data ->
      data
      * List.fold (expand_key key) ~init:0 ~f:(fun acc other_key ->
        acc + (Map.find t other_key |> Option.value ~default:0)))
  ;;

  let%expect_test _ =
    let l = numeric [ N0; N2; N9; NA ] in
    let d1 = of_dir_list l in
    let d2 = expand d1 in
    let d3 = expand d2 in
    print_s
      [%message
        (l : dir list)
          (d1 : t)
          (length d1 : int)
          (d2 : t)
          (length d2 : int)
          (d3 : t)
          (length d3 : int)];
    [%expect
      {|
      ((l (L A U A U U R A D D D A))
       (d1
        (((U U) 1) ((U R) 1) ((U A) 1) ((D D) 2) ((D A) 1) ((L A) 1) ((R A) 1)
         ((A U) 2) ((A D) 1) ((A L) 1)))
       ("length d1" 12)
       (d2
        (((U U) 0) ((U R) 2) ((U A) 1) ((D D) 0) ((D A) 4) ((L A) 1) ((R A) 3)
         ((A U) 4) ((A D) 2) ((A L) 2)))
       ("length d2" 19)
       (d3
        (((U U) 0) ((U R) 10) ((U A) 3) ((D D) 0) ((D A) 36) ((L A) 1) ((R A) 15)
         ((A U) 12) ((A D) 12) ((A L) 6)))
       ("length d3" 95))
      |}]
  ;;
end

let directional l =
  (* XXX dedup with numeric *)
  let last_dst, rev_legs =
    List.fold l ~init:(A, []) ~f:(fun (src, rev_legs) dst ->
      let src' = dst in
      let rev_legs' = directional_leg ~src ~dst :: rev_legs in
      src', rev_legs')
  in
  assert ([%equal: dir] last_dst A);
  rev_legs |> List.rev |> List.concat
;;

let parse_numeric s =
  String.to_list s
  |> List.map ~f:(function
    | '0' -> N0
    | '1' -> N1
    | '2' -> N2
    | '3' -> N3
    | '4' -> N4
    | '5' -> N5
    | '6' -> N6
    | '7' -> N7
    | '8' -> N8
    | '9' -> N9
    | 'A' -> NA
    | c -> raise_s [%message "parse_line" (c : char)])
;;

let parse s = String.split_lines s |> List.map ~f:parse_numeric

let p1 l =
  List.map l ~f:(fun n ->
    let len = numeric n |> directional |> directional |> List.length in
    let value = numeric_value n in
    len * value)
  |> Algo.sum
;;

let%expect_test _ =
  let t = parse (String.concat_lines [ "029A"; "980A"; "179A"; "456A"; "379A" ]) in
  List.iter t ~f:(fun n ->
    let dir1 = numeric n in
    let dir2 = directional dir1 in
    let dir3 = directional dir2 in
    let len1 = List.length dir1 in
    let len2 = List.length dir2 in
    let len3 = List.length dir3 in
    let value = numeric_value n in
    print_s
      [%message
        (n : numeric list)
          (dir1 : dir list)
          (len1 : int)
          (dir2 : dir list)
          (len2 : int)
          (dir3 : dir list)
          (len3 : int)
          (value : int)]);
  [%expect
    {|
    ((n (N0 N2 N9 NA)) (dir1 (L A U A U U R A D D D A)) (len1 12)
     (dir2 (D L L A R R U A L A R A L A A D R A U A L D A A A U R A)) (len2 28)
     (dir3
      (L D A L A A R R U A D A A L U A R A D L L A R R U A D A U A D L L A R R U
       A A L D A R A U A L A R A D L L A R A U R A A A L A D R A U A))
     (len3 68) (value 29))
    ((n (N9 N8 N0 NA)) (dir1 (U U U A L A D D D A R A)) (len1 12)
     (dir2 (L A A A R A D L L A R R U A L D A A A U R A D A U A)) (len2 26)
     (dir3
      (D L L A R R U A A A D A U A L D A L A A R R U A D A A L U A R A D L L A R
       A U R A A A L A D R A U A L D A U R A L A R A))
     (len3 60) (value 980))
    ((n (N1 N7 N9 NA)) (dir1 (U L L A U U A R R A D D D A)) (len1 14)
     (dir2 (L A D L A A R R U A L A A R A D A A U A L D A A A U R A)) (len2 28)
     (dir3
      (D L L A R R U A L D A L A R R U A A D A A L U A R A D L L A R R U A A D A
       U A L D A U R A A L A R A D L L A R A U R A A A L A D R A U A))
     (len3 68) (value 179))
    ((n (N4 N5 N6 NA)) (dir1 (U U L L A R A R A D D A)) (len1 12)
     (dir2 (L A A D L A A R R U A D A U A D A U A L D A A U R A)) (len2 26)
     (dir3
      (D L L A R R U A A L D A L A R R U A A D A A L U A R A L D A U R A L A R A
       L D A U R A L A R A D L L A R A U R A A L A D R A U A))
     (len3 64) (value 456))
    ((n (N3 N7 N9 NA)) (dir1 (U A L L U U A R R A D D D A)) (len1 14)
     (dir2 (L A R A D L L A A R U A A R A D A A U A L D A A A U R A)) (len2 28)
     (dir3
      (D L L A R R U A D A U A L D A L A A R R U A A D A L U A R A A D A U A L D
       A U R A A L A R A D L L A R A U R A A A L A D R A U A))
     (len3 64) (value 379))
    |}];
  print_s [%message (p1 t : int)];
  [%expect {| ("p1 t" 126384) |}]
;;

let f1 s = parse s |> p1
let f2 _ = 0
let run () = Run.run ~f1 ~f2 Day21_input.data
