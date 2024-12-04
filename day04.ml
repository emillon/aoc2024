let parse s = String.split_lines s |> List.map ~f:String.to_array |> List.to_array

let get t (i, j) =
  match t.(j).(i) with
  | c -> Some c
  | exception Invalid_argument _ -> None
;;

let fold_pos t ~f ~init =
  Array.foldi t ~init ~f:(fun j acc row ->
    Array.foldi row ~init:acc ~f:(fun i acc _ -> f acc (i, j)))
;;

let count t ~f = fold_pos t ~init:0 ~f:(fun acc pos -> acc + f pos)

let matches t pos dir ~pat =
  List.for_all pat ~f:(fun (i, c) ->
    match get t (Vec.add pos (Vec.smul dir i)) with
    | Some c' -> Char.equal c c'
    | None -> false)
;;

let xmas = matches ~pat:[ 0, 'X'; 1, 'M'; 2, 'A'; 3, 'S' ]
let xmas2_branch = matches ~pat:[ -1, 'M'; 0, 'A'; 1, 'S' ]

let xmas2 t pos dir =
  let dir' = Vec.cmul dir Vec.i in
  xmas2_branch t pos dir && xmas2_branch t pos dir'
;;

let count_xmas t =
  let dirs = [ 1, 0; 1, 1; 0, 1; -1, 1; -1, 0; -1, -1; 0, -1; 1, -1 ] in
  count t ~f:(fun pos -> List.count dirs ~f:(xmas t pos))
;;

let count_xmas2 t =
  let dirs = [ 1, 1; -1, 1; -1, -1; 1, -1 ] in
  count t ~f:(fun pos -> List.count dirs ~f:(xmas2 t pos))
;;

let%expect_test _ =
  let t =
    [ "MMMSXXMASM"
    ; "MSAMXMSMSA"
    ; "AMXSXMAAMM"
    ; "MSAMASMSMX"
    ; "XMASAMXAMM"
    ; "XXAMMXXAMA"
    ; "SMSMSASXSS"
    ; "SAXAMASAAA"
    ; "MAMMMXMMMM"
    ; "MXMXAXMASX"
    ]
    |> String.concat_lines
    |> parse
  in
  print_s [%message (count_xmas t : int) (count_xmas2 t : int)];
  [%expect {| (("count_xmas t" 18) ("count_xmas2 t" 9)) |}]
;;

let f1 s = parse s |> count_xmas
let f2 s = parse s |> count_xmas2
let run () = Run.run ~f1 ~f2 Day04_input.data
