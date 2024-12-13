type machine =
  { a : Vec.t
  ; b : Vec.t
  ; prize : Vec.t
  }
[@@deriving sexp]

type t = machine list [@@deriving sexp]

let parse =
  let open Parsing_util in
  let open Angstrom in
  let button name =
    both
      (string "Button " *> string name *> string ": X+" *> number)
      (string ", Y+" *> number <* end_of_line)
  in
  let prize =
    both (string "Prize: X=" *> number) (string ", Y=" *> number <* end_of_line)
  in
  let machine =
    let+ a = button "A"
    and+ b = button "B"
    and+ prize in
    { a; b; prize }
  in
  parse_using (sep_by1 end_of_line machine)
;;

let exact_div a b = if Int.rem a b = 0 then Some (a / b) else None

let result { a = ax, ay; b = bx, by; prize = px, py } =
  let det = (ax * by) - (ay * bx) in
  assert (det <> 0);
  let open Option.Let_syntax in
  let%bind na = exact_div ((by * px) - (bx * py)) det in
  let%map nb = exact_div ((-ay * px) + (ax * py)) det in
  (3 * na) + nb
;;

let p1 t = List.filter_map t ~f:result |> Algo.sum

let%expect_test _ =
  let t =
    [ "Button A: X+94, Y+34"
    ; "Button B: X+22, Y+67"
    ; "Prize: X=8400, Y=5400"
    ; ""
    ; "Button A: X+26, Y+66"
    ; "Button B: X+67, Y+21"
    ; "Prize: X=12748, Y=12176"
    ; ""
    ; "Button A: X+17, Y+86"
    ; "Button B: X+84, Y+37"
    ; "Prize: X=7870, Y=6450"
    ; ""
    ; "Button A: X+69, Y+23"
    ; "Button B: X+27, Y+71"
    ; "Prize: X=18641, Y=10279"
    ]
    |> String.concat_lines
    |> parse
  in
  print_s [%message (t : t) (p1 t : int)];
  [%expect
    {|
    ((t
      (((a (94 34)) (b (22 67)) (prize (8400 5400)))
       ((a (26 66)) (b (67 21)) (prize (12748 12176)))
       ((a (17 86)) (b (84 37)) (prize (7870 6450)))
       ((a (69 23)) (b (27 71)) (prize (18641 10279)))))
     ("p1 t" 480))
    |}]
;;

let adjust =
  List.map ~f:(fun m ->
    let px, py = m.prize in
    let d = 10_000_000_000_000 in
    { m with prize = px + d, py + d })
;;

let f1 s = parse s |> p1
let f2 s = parse s |> adjust |> p1
let run () = Run.run ~f1 ~f2 Day13_input.data
