let sum = List.fold ~init:0 ~f:( + )
let product = List.fold ~init:1 ~f:( * )

let rec insert x l =
  match l with
  | [] -> [ [ x ] ]
  | h :: t -> (x :: l) :: List.map ~f:(fun l -> h :: l) (insert x t)
;;

let rec permutations = function
  | [] -> [ [] ]
  | x :: xs -> List.concat_map ~f:(insert x) (permutations xs)
;;

let%expect_test "permutations" =
  permutations [ 1; 2; 3; 4 ] |> [%sexp_of: int list list] |> print_s;
  [%expect
    {|
    ((1 2 3 4) (2 1 3 4) (2 3 1 4) (2 3 4 1) (1 3 2 4) (3 1 2 4) (3 2 1 4)
     (3 2 4 1) (1 3 4 2) (3 1 4 2) (3 4 1 2) (3 4 2 1) (1 2 4 3) (2 1 4 3)
     (2 4 1 3) (2 4 3 1) (1 4 2 3) (4 1 2 3) (4 2 1 3) (4 2 3 1) (1 4 3 2)
     (4 1 3 2) (4 3 1 2) (4 3 2 1)) |}]
;;

let rec sublists = function
  | [] -> [ [] ]
  | x :: xs -> List.concat_map (sublists xs) ~f:(fun l -> [ l; List.cons x l ])
;;

let%expect_test "sublists" =
  sublists [ 'a'; 'b'; 'c' ] |> [%sexp_of: char list list] |> print_s;
  [%expect {| (() (a) (b) (a b) (c) (a c) (b c) (a b c)) |}]
;;

let rec legs_after x = function
  | a :: l -> (x, a) :: legs_after a l
  | [] -> []
;;

let legs = function
  | [] -> assert false
  | x :: xs -> legs_after x xs
;;

exception Overflow

let char_succ_exn c = Char.to_int c |> Int.succ |> Char.of_int_exn

let rec incr_buf_at_point ~min ~max b i =
  if i < 0 then raise Overflow;
  let c = Bytes.get b i in
  if Char.equal c max
  then (
    Bytes.set b i min;
    incr_buf_at_point ~min ~max b (i - 1))
  else Bytes.set b i (char_succ_exn c)
;;

let incr_buf ~min ~max b = incr_buf_at_point ~min ~max b (Bytes.length b - 1)

let resize_b ~min b =
  let r = Bytes.make (Bytes.length b + 1) min in
  Bytes.set r 0 (char_succ_exn min);
  r
;;

let iter_bytes ~f ~start ~min ~max =
  let rec go b =
    if f b
    then Bytes.to_string b
    else (
      match incr_buf ~min ~max b with
      | () -> go b
      | exception Overflow -> go (resize_b ~min b))
  in
  go (Bytes.of_string start)
;;

let%expect_test "iter_bytes" =
  iter_bytes
    ~f:(fun b ->
      let s = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b in
      printf "%s\n" s;
      Int.of_string s >= 12)
    ~start:"8"
    ~min:'0'
    ~max:'9'
  |> printf "result: %s";
  [%expect
    {|
    8
    9
    10
    11
    12
    result: 12 |}]
;;

let guard = function
  | true -> [ () ]
  | false -> []
;;

let rec egcd a b =
  if b = 0
  then a, 1, 0
  else (
    let d, s, t = egcd b (a % b) in
    d, t, s - (a / b * t))
;;

let%expect_test "egcd" =
  let test a b = egcd a b |> [%sexp_of: int * int * int] |> print_s in
  test 5 3;
  [%expect {| (1 -1 2) |}]
;;

let gcd a b =
  let g, _, _ = egcd a b in
  g
;;

let lcm a b =
  let g = gcd a b in
  a / g * b
;;

let sole = function
  | [ x ] -> x
  | [] -> invalid_arg "sole: []"
  | _ :: _ -> invalid_arg "sole: _::_"
;;

let digits10 n =
  if n = 0
  then 1
  else n |> Int.to_float |> Float.log10 |> Float.round_down |> Int.of_float |> Int.succ
;;

let%expect_test "digits10" =
  let test n = print_s [%message (n : int) (digits10 n : int)] in
  test 3;
  [%expect {| ((n 3) ("digits10 n" 1)) |}];
  test 9;
  [%expect {| ((n 9) ("digits10 n" 1)) |}];
  test 10;
  [%expect {| ((n 10) ("digits10 n" 2)) |}];
  test 1234;
  [%expect {| ((n 1234) ("digits10 n" 4)) |}];
  test 0;
  [%expect {| ((n 0) ("digits10 n" 1)) |}]
;;
