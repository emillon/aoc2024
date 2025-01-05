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

module Deltas = struct
  module T = struct
    type t = int * int * int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let shift (_d0, d1, d2, d3) d = d1, d2, d3, d
end

type state =
  | L0
  | L1 of int
  | L2 of int * int
  | L3 of int * int * int
  | D of Deltas.t * int Map.M(Deltas).t
[@@deriving sexp]

let best_prices ~start ~n =
  let rec go i secret st =
    if i = 0
    then (
      match st with
      | D (_, m) -> m
      | _ -> raise_s [%message "final" (st : state)])
    else (
      let secret' = next secret in
      let price = secret % 10 in
      let price' = secret' % 10 in
      let d = price' - price in
      let st' =
        match st with
        | L0 -> L1 d
        | L1 d0 -> L2 (d0, d)
        | L2 (d0, d1) -> L3 (d0, d1, d)
        | L3 (d0, d1, d2) -> D ((d0, d1, d2, d), Map.empty (module Deltas))
        | D (key, m) ->
          let m' =
            Map.update m key ~f:(function
              | None -> price
              | Some prev_max -> Int.max d prev_max)
          in
          D (Deltas.shift key d, m')
      in
      go (i - 1) secret' st')
  in
  go n start L0
;;

let%expect_test "best_prices" =
  [ 1; 2; 3; 2024 ]
  |> List.iter ~f:(fun start ->
    let n = 2000 in
    let k = -2, 1, -1, 3 in
    let best = best_prices ~start ~n in
    let best_for_k = Map.find best k in
    print_s [%message (start : int) (best_for_k : int option)]);
  [%expect
    {|
    ((start 1) (best_for_k (7)))
    ((start 2) (best_for_k (7)))
    ((start 3) (best_for_k ()))
    ((start 2024) (best_for_k (9)))
    |}]
;;

let best_key l ~n =
  let r =
    List.fold
      l
      ~init:(Map.empty (module Deltas))
      ~f:(fun acc start ->
        let best = best_prices ~start ~n in
        Map.merge_skewed acc best ~combine:(fun ~key:_ a b -> a + b))
  in
  r
  |> Map.to_alist
  |> List.max_elt ~compare:(Comparable.lift Int.compare ~f:snd)
  |> Option.value_exn
;;

let%expect_test _ =
  let l = [ 1; 2; 3; 2024 ] in
  let n = 2000 in
  let best = best_key l ~n in
  print_s [%message (best : Deltas.t * int)];
  [%expect {| (best ((-2 1 -1 3) 23)) |}]
;;

let parse s = String.split_lines s |> List.map ~f:Int.of_string
let f1 s = parse s |> List.map ~f:n2000 |> Algo.sum
let f2 s = parse s |> best_key ~n:2000 |> snd
let run () = Run.run ~f1 ~f2 Day22_input.data
