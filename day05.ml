module Pair = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

type t =
  { rules : Set.M(Pair).t
  ; updates : int list list
  }
[@@deriving sexp]

let parse =
  let open Parsing_util in
  let open Angstrom in
  let rule = both (number <* string "|") (number <* end_of_line) in
  let update = sep_by1 (string ",") number <* end_of_line in
  parse_using
    (let+ rule_list = many_till rule end_of_line
     and+ updates = many1 update in
     let rules = Set.of_list (module Pair) rule_list in
     { rules; updates })
;;

let middle l =
  let n = List.length l in
  assert (n % 2 = 1);
  List.nth_exn l (n / 2)
;;

let compare_from_rules rules a b =
  match () with
  | _ when Set.mem rules (a, b) -> -1
  | _ when Set.mem rules (b, a) -> 1
  | _ -> assert false
;;

let f1 s =
  let { rules; updates } = parse s in
  let compare = compare_from_rules rules in
  List.fold updates ~init:0 ~f:(fun acc update ->
    if List.is_sorted update ~compare then acc + middle update else acc)
;;

let f2 s =
  let { rules; updates } = parse s in
  let compare = compare_from_rules rules in
  List.fold updates ~init:0 ~f:(fun acc update ->
    if List.is_sorted update ~compare
    then acc
    else acc + middle (List.sort ~compare update))
;;

let run () = Run.run ~f1 ~f2 Day05_input.data
