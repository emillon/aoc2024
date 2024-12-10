let parse s : int Map.M(Vec).t =
  Vec.parse_2d
    s
    ~init:(Map.empty (module Vec))
    ~f:(fun pos acc c ->
      match c with
      | '0' .. '9' ->
        let data = Char.to_int c - Char.to_int '0' in
        Map.add_exn acc ~key:pos ~data
      | _ -> raise_s [%message (pos : Vec.t) (c : char)])
;;

let rec trails t pos ~start =
  match Map.find t pos with
  | Some current_height when start = current_height ->
    if start = 9
    then [ pos ]
    else Vec.neighbours4 pos |> List.concat_map ~f:(trails t ~start:(start + 1))
  | _ -> []
;;

let solve t ~unique =
  Map.fold t ~init:0 ~f:(fun ~key:pos ~data:_ acc ->
    let l = trails t pos ~start:0 in
    let score =
      if unique then l |> Set.of_list (module Vec) |> Set.length else List.length l
    in
    acc + score)
;;

let f1 s = parse s |> solve ~unique:true
let f2 s = parse s |> solve ~unique:false

let%expect_test _ =
  let sample =
    String.concat_lines
      [ "89010123"
      ; "78121874"
      ; "87430965"
      ; "96549874"
      ; "45678903"
      ; "32019012"
      ; "01329801"
      ; "10456732"
      ]
  in
  print_s [%message (f1 sample : int) (f2 sample : int)];
  [%expect {| (("f1 sample" 36) ("f2 sample" 81)) |}]
;;

let run () = Run.run ~f1 ~f2 Day10_input.data
