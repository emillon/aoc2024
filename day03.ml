type instr =
  | Do
  | Don't
  | N of int

let parse =
  let s_do = "do()" in
  let s_don't = "don't()" in
  let re =
    let open Re in
    let num = repn digit 1 (Some 3) in
    compile
      (alt
         [ str s_do
         ; str s_don't
         ; seq [ str "mul("; group num; str ","; group num; str ")" ]
         ])
  in
  fun s ->
    Re.all re s
    |> List.map ~f:(fun g ->
      let g0 = Re.Group.get g 0 in
      if String.equal g0 s_do
      then Do
      else if String.equal g0 s_don't
      then Don't
      else (
        let int i = Int.of_string (Re.Group.get g i) in
        N (int 1 * int 2)))
;;

let f1 s =
  parse s
  |> List.filter_map ~f:(function
    | N n -> Some n
    | Do -> None
    | Don't -> None)
  |> Algo.sum
;;

let%expect_test "f1" =
  f1 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  |> printf "%d";
  [%expect {| 161 |}]
;;

let f2 s =
  parse s
  |> List.fold ~init:(0, true) ~f:(fun (n, active) i ->
    match i with
    | Do -> n, true
    | Don't -> n, false
    | N a -> (if active then n + a else n), active)
  |> fst
;;

let%expect_test "f2" =
  f2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
  |> printf "%d";
  [%expect {| 48 |}]
;;

let run () = Run.run ~f1 ~f2 Day03_input.data
