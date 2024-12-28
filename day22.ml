let mix = Int.bit_xor
let prune n = n % 16777216

let%expect_test _ =
  mix 42 15 |> printf "%d";
  [%expect {| 37 |}];
  prune 100000000 |> printf "%d";
  [%expect {| 16113920 |}]
;;

let step1 n = mix n (n * 64) |> prune

let step2 n =
  let n32 = if true then n / 32 else Float.round_down (n // 32) |> Float.to_int in
  mix n n32 |> prune
;;

let step3 n = mix n (n * 2048) |> prune
let next n = n |> step1 |> step2 |> step3

let%expect_test "next" =
  let n = ref 123 in
  let go () =
    Ref.replace n next;
    printf "%d" !n
  in
  go ();
  [%expect {| 15887950 |}];
  go ();
  [%expect {| 16495136 |}];
  go ();
  [%expect {| 527345 |}];
  go ();
  [%expect {| 704524 |}];
  go ();
  [%expect {| 1553684 |}];
  go ();
  [%expect {| 12683156 |}];
  go ();
  [%expect {| 11100544 |}];
  go ();
  [%expect {| 12249484 |}];
  go ();
  [%expect {| 7753432 |}];
  go ();
  [%expect {| 5908254 |}]
;;

let n2000 = Fn.apply_n_times ~n:2000 next

let%expect_test "n200" =
  [ 1; 10; 200; 2024 ] |> List.iter ~f:(fun n -> printf "%d: %d\n" n (n2000 n));
  [%expect
    {|
    1: 8685429
    10: 4700978
    200: 5641584
    2024: 8667524
    |}]
;;

let parse s = String.split_lines s |> List.map ~f:Int.of_string
let f1 s = parse s |> List.map ~f:n2000 |> Algo.sum
let f2 _ = 0
let run () = Run.run ~f1 ~f2 Day22_input.data
