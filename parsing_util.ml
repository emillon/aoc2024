let number =
  let open Angstrom in
  let+ s = take_while1 Char.is_digit in
  Int.of_string s
;;

let signed_number =
  let open Angstrom in
  let+ is_negative = option false (char '-' *> return true)
  and+ n = number in
  if is_negative then -n else n
;;

let word =
  let open Angstrom in
  take_while1 Char.is_alpha
;;

let parse_using p s =
  match Angstrom.parse_string ~consume:All p s with
  | Ok x -> x
  | Error e -> Printf.ksprintf failwith "While parsing %S: %s" s e
;;

let parse_lines_using p =
  let open Angstrom in
  parse_using (many (p <* end_of_line))
;;

let enum l =
  let open Angstrom in
  List.map l ~f:(fun (s, r) -> string s *> return r) |> choice
;;
