type t =
  { fragments : string list
  ; words : string list
  }
[@@deriving sexp]

let parse =
  let open Parsing_util in
  let open Angstrom in
  parse_using
    (let+ fragments = sep_by1 (string ", ") word <* end_of_line <* end_of_line
     and+ words = many1 (word <* end_of_line) in
     { fragments; words })
;;

let p1 { fragments; words } =
  let open Re in
  let re = compile (seq [ bos; rep1 (alt (List.map ~f:str fragments)); eos ]) in
  List.count words ~f:(execp re)
;;

let p2 { fragments; words } =
  let set = Set.of_list (module String) fragments in
  let ways_exp ways_rec s =
    List.fold
      fragments
      ~init:(if Set.mem set s then 1 else 0)
      ~f:(fun acc prefix ->
        acc
        +
        match String.chop_prefix s ~prefix with
        | None -> 0
        | Some suffix -> ways_rec suffix)
  in
  let cache = Hashtbl.create (module String) in
  let rec ways s =
    Hashtbl.find cache s
    |> Option.value_or_thunk ~default:(fun () ->
      let r = ways_exp ways s in
      Hashtbl.set cache ~key:s ~data:r;
      r)
  in
  List.map words ~f:ways |> Algo.sum
;;

let%expect_test _ =
  let t =
    parse
      (String.concat_lines
         [ "r, wr, b, g, bwu, rb, gb, br"
         ; ""
         ; "brwrr"
         ; "bggr"
         ; "gbbr"
         ; "rrbgbr"
         ; "ubwu"
         ; "bwurrg"
         ; "brgr"
         ; "bbrgwb"
         ])
  in
  print_s [%message (t : t) (p1 t : int) (p2 t : int)];
  [%expect
    {|
    ((t
      ((fragments (r wr b g bwu rb gb br))
       (words (brwrr bggr gbbr rrbgbr ubwu bwurrg brgr bbrgwb))))
     ("p1 t" 6) ("p2 t" 16))
    |}]
;;

let f1 s = parse s |> p1
let f2 s = parse s |> p2
let run () = Run.run ~f1 ~f2 Day19_input.data
