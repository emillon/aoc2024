let rec all_but_last = function
  | [] -> assert false
  | [ _ ] -> []
  | x :: xs -> x :: all_but_last xs
;;

let deltas l =
  List.zip_exn (List.tl_exn l) (all_but_last l) |> List.map ~f:(fun (a, b) -> abs (a - b))
;;

let safe l =
  (List.is_sorted l ~compare:Int.compare
   || List.is_sorted l ~compare:(Comparable.reverse Int.compare))
  && List.for_all (deltas l) ~f:(fun d -> d >= 1 && d <= 3)
;;

let parse =
  let open Parsing_util in
  let open Angstrom in
  parse_lines_using (sep_by1 (char ' ') number)
;;

let f1 s = parse s |> List.count ~f:safe

let rec removed = function
  | [] -> []
  | [ _ ] -> [ [] ]
  | x :: xs -> [ xs ] @ List.map (removed xs) ~f:(fun t -> x :: t)
;;

let safe2 l = List.exists (removed l) ~f:safe
let f2 s = parse s |> List.count ~f:safe2
let run () = Run.run ~f1 ~f2 Day02_input.data
