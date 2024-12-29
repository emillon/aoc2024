type lock_or_key =
  { is_lock : bool
  ; pins : int list
  }
[@@deriving sexp]

type t = lock_or_key list [@@deriving sexp]

let parse_pins l =
  l |> List.transpose_exn |> List.map ~f:(fun row -> List.count ~f:Fn.id row - 1)
;;

let parse =
  let open Parsing_util in
  let open Angstrom in
  let lock_or_key_char = char '.' *> return false <|> char '#' *> return true in
  let lock_or_key_line = many1 lock_or_key_char <* end_of_line in
  let lock_or_key = many1 lock_or_key_line in
  parse_using
    (let+ l = sep_by1 end_of_line lock_or_key in
     List.map l ~f:(fun l ->
       let is_lock = List.hd_exn (List.hd_exn l) in
       let pins = parse_pins l in
       { is_lock; pins }))
;;

let p1 t =
  let locks, keys =
    List.partition_map t ~f:(function
      | { is_lock = true; pins } -> First pins
      | { is_lock = false; pins } -> Second pins)
  in
  List.cartesian_product locks keys
  |> List.count ~f:(fun (lock, key) ->
    List.for_all2_exn lock key ~f:(fun l k -> l + k <= 5))
;;

let%expect_test _ =
  let t =
    parse
      (String.concat_lines
         [ "#####"
         ; ".####"
         ; ".####"
         ; ".####"
         ; ".#.#."
         ; ".#..."
         ; "....."
         ; ""
         ; "#####"
         ; "##.##"
         ; ".#.##"
         ; "...##"
         ; "...#."
         ; "...#."
         ; "....."
         ; ""
         ; "....."
         ; "#...."
         ; "#...."
         ; "#...#"
         ; "#.#.#"
         ; "#.###"
         ; "#####"
         ; ""
         ; "....."
         ; "....."
         ; "#.#.."
         ; "###.."
         ; "###.#"
         ; "###.#"
         ; "#####"
         ; ""
         ; "....."
         ; "....."
         ; "....."
         ; "#...."
         ; "#.#.."
         ; "#.#.#"
         ; "#####"
         ])
  in
  print_s [%message (t : t) (p1 t : int)];
  [%expect
    {|
    ((t
      (((is_lock true) (pins (0 5 3 4 3))) ((is_lock true) (pins (1 2 0 5 3)))
       ((is_lock false) (pins (5 0 2 1 3))) ((is_lock false) (pins (4 3 4 0 2)))
       ((is_lock false) (pins (3 0 2 0 1)))))
     ("p1 t" 3))
    |}]
;;

let f1 s = parse s |> p1
let f2 _ = 0
let run () = Run.run ~f1 ~f2 Day25_input.data
