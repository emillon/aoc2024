let try_split n =
  let digits = Printf.ksprintf String.length "%d" n in
  if digits % 2 = 0
  then (
    let d = Int.pow 10 (digits / 2) in
    Some (n / d, n % d))
  else None
;;

let%expect_test "try_split" =
  let test n = print_s [%message (n : int) (try_split n : (int * int) option)] in
  test 1234;
  [%expect {| ((n 1234) ("try_split n" ((12 34)))) |}];
  test 12345;
  [%expect {| ((n 12345) ("try_split n" ())) |}];
  test 1000;
  [%expect {| ((n 1000) ("try_split n" ((10 0)))) |}]
;;

let blink1 blink1_rec ~n x =
  assert (n >= 0);
  if n = 0
  then 1
  else if x = 0
  then blink1_rec ~n:(n - 1) 1
  else (
    match try_split x with
    | Some (a, b) -> blink1_rec ~n:(n - 1) a + blink1_rec ~n:(n - 1) b
    | None -> blink1_rec ~n:(n - 1) (x * 2024))
;;

let blinkn ~n l =
  let cache = Hashtbl.create (module Vec) in
  let rec blink1_cached ~n x =
    let key = n, x in
    Hashtbl.find cache key
    |> Option.value_or_thunk ~default:(fun () ->
      let data = blink1 blink1_cached ~n x in
      Hashtbl.add_exn cache ~key ~data;
      data)
  in
  List.map l ~f:(blink1_cached ~n) |> Algo.sum
;;

let%expect_test "blinkn" =
  let test l n = blinkn ~n l |> printf "%d" in
  let l = [ 125; 17 ] in
  test l 6;
  [%expect {| 22 |}];
  test l 25;
  [%expect {| 55312 |}]
;;

let parse =
  let open Parsing_util in
  let open Angstrom in
  parse_using (sep_by1 (string " ") number <* end_of_line)
;;

let f1 s = parse s |> blinkn ~n:25
let f2 s = parse s |> blinkn ~n:75
let run () = Run.run ~f1 ~f2 Day11_input.data
