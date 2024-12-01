let parse s =
  let open Parsing_util in
  let open Angstrom in
  parse_lines_using
    (let+ a = number
     and+ _ = take_while1 Char.is_whitespace
     and+ b = number in
     a, b)
    s
  |> List.unzip
;;

let diff (a, b) = abs (a - b)

let f1 s =
  let la, lb = parse s in
  List.zip_exn (List.sort ~compare:Int.compare la) (List.sort ~compare:Int.compare lb)
  |> List.map ~f:diff
  |> Algo.sum
;;

let f2 s =
  let la, lb = parse s in
  List.fold la ~init:0 ~f:(fun acc x -> acc + (x * List.count lb ~f:(Int.equal x)))
;;

let run () = Run.run ~f1 ~f2 Day01_input.data
