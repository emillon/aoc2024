type op =
  | AND
  | OR
  | XOR
[@@deriving sexp]

let eval_op = function
  | AND -> ( && )
  | OR -> ( || )
  | XOR -> Bool.( <> )
;;

type expr =
  { src1 : string
  ; op : op
  ; src2 : string
  }
[@@deriving sexp]

type t =
  { values : (string * bool) list
  ; eqns : expr Map.M(String).t
  }
[@@deriving sexp]

let eval { values; eqns } =
  let r = ref (Map.of_alist_exn (module String) values) in
  let q = Queue.of_list (Map.keys eqns) in
  while not (Queue.is_empty q) do
    let signal = Queue.dequeue_exn q in
    if Map.mem !r signal
    then ()
    else (
      let { src1; op; src2 } = Map.find_exn eqns signal in
      match Map.find !r src1, Map.find !r src2 with
      | Some v1, Some v2 ->
        let v = eval_op op v1 v2 in
        r := Map.add_exn !r ~key:signal ~data:v
      | vo1, vo2 ->
        if Option.is_none vo1 then Queue.enqueue q src1;
        if Option.is_none vo2 then Queue.enqueue q src2;
        Queue.enqueue q signal)
  done;
  !r
;;

let parse =
  let open Parsing_util in
  let open Angstrom in
  let signal = take_while Char.is_alphanum in
  let bool = enum [ "0", false; "1", true ] in
  let value = both signal (string ": " *> bool) in
  let op = enum [ "AND", AND; "OR", OR; "XOR", XOR ] in
  let eqn =
    let+ src1 = signal
    and+ _ = string " "
    and+ op
    and+ _ = string " "
    and+ src2 = signal
    and+ _ = string " -> "
    and+ dst = signal in
    dst, { src1; op; src2 }
  in
  parse_using
    (let+ values = many_till (value <* end_of_line) end_of_line
     and+ eqns_alist = many1 (eqn <* end_of_line) in
     let eqns = Map.of_alist_exn (module String) eqns_alist in
     { values; eqns })
;;

let of_bools l = l |> List.fold ~init:0 ~f:(fun acc b -> (2 * acc) + Bool.to_int b)

let p1 t =
  eval t
  |> Map.filter_keys ~f:(String.is_prefix ~prefix:"z")
  |> Map.to_alist ~key_order:`Decreasing
  |> List.map ~f:snd
  |> of_bools
;;

let%expect_test _ =
  let t =
    parse
      (String.concat_lines
         [ "x00: 1"
         ; "x01: 0"
         ; "x02: 1"
         ; "x03: 1"
         ; "x04: 0"
         ; "y00: 1"
         ; "y01: 1"
         ; "y02: 1"
         ; "y03: 1"
         ; "y04: 1"
         ; ""
         ; "ntg XOR fgs -> mjb"
         ; "y02 OR x01 -> tnw"
         ; "kwq OR kpj -> z05"
         ; "x00 OR x03 -> fst"
         ; "tgd XOR rvg -> z01"
         ; "vdt OR tnw -> bfw"
         ; "bfw AND frj -> z10"
         ; "ffh OR nrd -> bqk"
         ; "y00 AND y03 -> djm"
         ; "y03 OR y00 -> psh"
         ; "bqk OR frj -> z08"
         ; "tnw OR fst -> frj"
         ; "gnj AND tgd -> z11"
         ; "bfw XOR mjb -> z00"
         ; "x03 OR x00 -> vdt"
         ; "gnj AND wpb -> z02"
         ; "x04 AND y00 -> kjc"
         ; "djm OR pbm -> qhw"
         ; "nrd AND vdt -> hwm"
         ; "kjc AND fst -> rvg"
         ; "y04 OR y02 -> fgs"
         ; "y01 AND x02 -> pbm"
         ; "ntg OR kjc -> kwq"
         ; "psh XOR fgs -> tgd"
         ; "qhw XOR tgd -> z09"
         ; "pbm OR djm -> kpj"
         ; "x03 XOR y03 -> ffh"
         ; "x00 XOR y04 -> ntg"
         ; "bfw OR bqk -> z06"
         ; "nrd XOR fgs -> wpb"
         ; "frj XOR qhw -> z04"
         ; "bqk OR frj -> z07"
         ; "y03 OR x01 -> nrd"
         ; "hwm AND bqk -> z03"
         ; "tgd XOR rvg -> z12"
         ; "tnw OR pbm -> gnj"
         ])
  in
  print_s [%message (t : t) (eval t : bool Map.M(String).t) (p1 t : int)];
  [%expect
    {|
    ((t
      ((values
        ((x00 true) (x01 false) (x02 true) (x03 true) (x04 false) (y00 true)
         (y01 true) (y02 true) (y03 true) (y04 true)))
       (eqns
        ((bfw ((src1 vdt) (op OR) (src2 tnw)))
         (bqk ((src1 ffh) (op OR) (src2 nrd)))
         (djm ((src1 y00) (op AND) (src2 y03)))
         (ffh ((src1 x03) (op XOR) (src2 y03)))
         (fgs ((src1 y04) (op OR) (src2 y02)))
         (frj ((src1 tnw) (op OR) (src2 fst)))
         (fst ((src1 x00) (op OR) (src2 x03)))
         (gnj ((src1 tnw) (op OR) (src2 pbm)))
         (hwm ((src1 nrd) (op AND) (src2 vdt)))
         (kjc ((src1 x04) (op AND) (src2 y00)))
         (kpj ((src1 pbm) (op OR) (src2 djm)))
         (kwq ((src1 ntg) (op OR) (src2 kjc)))
         (mjb ((src1 ntg) (op XOR) (src2 fgs)))
         (nrd ((src1 y03) (op OR) (src2 x01)))
         (ntg ((src1 x00) (op XOR) (src2 y04)))
         (pbm ((src1 y01) (op AND) (src2 x02)))
         (psh ((src1 y03) (op OR) (src2 y00)))
         (qhw ((src1 djm) (op OR) (src2 pbm)))
         (rvg ((src1 kjc) (op AND) (src2 fst)))
         (tgd ((src1 psh) (op XOR) (src2 fgs)))
         (tnw ((src1 y02) (op OR) (src2 x01)))
         (vdt ((src1 x03) (op OR) (src2 x00)))
         (wpb ((src1 nrd) (op XOR) (src2 fgs)))
         (z00 ((src1 bfw) (op XOR) (src2 mjb)))
         (z01 ((src1 tgd) (op XOR) (src2 rvg)))
         (z02 ((src1 gnj) (op AND) (src2 wpb)))
         (z03 ((src1 hwm) (op AND) (src2 bqk)))
         (z04 ((src1 frj) (op XOR) (src2 qhw)))
         (z05 ((src1 kwq) (op OR) (src2 kpj)))
         (z06 ((src1 bfw) (op OR) (src2 bqk)))
         (z07 ((src1 bqk) (op OR) (src2 frj)))
         (z08 ((src1 bqk) (op OR) (src2 frj)))
         (z09 ((src1 qhw) (op XOR) (src2 tgd)))
         (z10 ((src1 bfw) (op AND) (src2 frj)))
         (z11 ((src1 gnj) (op AND) (src2 tgd)))
         (z12 ((src1 tgd) (op XOR) (src2 rvg)))))))
     ("eval t"
      ((bfw true) (bqk true) (djm true) (ffh false) (fgs true) (frj true)
       (fst true) (gnj true) (hwm true) (kjc false) (kpj true) (kwq false)
       (mjb true) (nrd true) (ntg false) (pbm true) (psh true) (qhw true)
       (rvg false) (tgd false) (tnw true) (vdt true) (wpb false) (x00 true)
       (x01 false) (x02 true) (x03 true) (x04 false) (y00 true) (y01 true)
       (y02 true) (y03 true) (y04 true) (z00 false) (z01 false) (z02 false)
       (z03 true) (z04 false) (z05 true) (z06 true) (z07 true) (z08 true)
       (z09 true) (z10 true) (z11 false) (z12 false)))
     ("p1 t" 2024))
    |}]
;;

let f1 s = parse s |> p1
let f2 _ = 0
let run () = Run.run ~f1 ~f2 Day24_input.data
