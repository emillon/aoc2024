let sample =
  String.concat_lines
    [ "190: 10 19"
    ; "3267: 81 40 27"
    ; "83: 17 5"
    ; "156: 15 6"
    ; "7290: 6 8 6 15"
    ; "161011: 16 10 13"
    ; "192: 17 8 14"
    ; "21037: 9 7 18 13"
    ; "292: 11 6 16 20"
    ]
;;

let parse =
  let open Parsing_util in
  let open Angstrom in
  parse_lines_using (both (number <* string ": ") (sep_by1 (string " ") number))
;;

let rec combinations ops = function
  | [ x ] -> [ x ]
  | x :: xs ->
    let open List.Let_syntax in
    let%bind r = combinations ops xs in
    let%map op = ops in
    op x r
  | [] -> assert false
;;

let is_ok ops (r, nums) = List.mem (combinations ops (List.rev nums)) r ~equal:Int.equal

let solve ops s =
  parse s |> List.map ~f:(fun l -> if is_ok ops l then fst l else 0) |> Algo.sum
;;

let f1 = solve [ ( * ); ( + ) ]

let%expect_test "f1" =
  f1 sample |> printf "%d";
  [%expect {| 3749 |}]
;;

(* TODO slow, use log10 to shift *)
let join a b = Printf.sprintf "%d%d" b a |> Int.of_string
let f2 = solve [ ( * ); ( + ); join ]

let%expect_test "f2" =
  f2 sample |> printf "%d";
  [%expect {| 11387 |}]
;;

let run () = Run.run ~f1 ~f2 Day07_input.data
