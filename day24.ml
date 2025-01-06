type op =
  | AND
  | OR
  | XOR
[@@deriving compare, equal, sexp]

let eval_op_bool = function
  | AND -> ( && )
  | OR -> ( || )
  | XOR -> Bool.( <> )
;;

type expr =
  { src1 : string
  ; op : op
  ; src2 : string
  }
[@@deriving equal, sexp]

type t =
  { values : (string * bool) list
  ; eqns : expr Map.M(String).t
  }
[@@deriving equal, sexp]

let eval { values; eqns } eval_op literal =
  let r =
    values
    |> List.map ~f:(fun (n, b) -> n, literal n b)
    |> Map.of_alist_exn (module String)
    |> ref
  in
  let q = Queue.of_list (Map.keys eqns) in
  while not (Queue.is_empty q) do
    let signal = Queue.dequeue_exn q in
    if Map.mem !r signal
    then ()
    else (
      let { src1; op; src2 } =
        Map.find eqns signal
        |> Option.value_or_thunk ~default:(fun () ->
          raise_s [%message "eval: not found" (signal : string)])
      in
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
  eval t eval_op_bool (fun _ b -> b)
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
  print_s [%message (p1 t : int)];
  [%expect {| ("p1 t" 2024) |}]
;;

let f1 s = parse s |> p1 |> Int.to_string

type sym =
  | S_op of op * sym * sym
  | S_sym of string
[@@deriving compare]

let rec sexp_of_sym = function
  | S_op (op, a, b) -> Sexp.List [ [%sexp_of: op] op; sexp_of_sym a; sexp_of_sym b ]
  | S_sym s -> [%sexp_of: string] s
;;

let eval_sym op a b =
  let a', b' = if [%compare: sym] a b < 0 then a, b else b, a in
  S_op (op, a', b')
;;

let prune ~is_root t =
  let rec go s =
    let s' =
      Map.fold t.eqns ~init:s ~f:(fun ~key:dst ~data:{ src1; src2; op = _ } acc ->
        if is_root dst || (Set.mem s src1 && Set.mem s src2) then Set.add acc dst else acc)
    in
    if Set.equal s s' then s else go s'
  in
  let alive = go (List.map ~f:fst t.values |> Set.of_list (module String)) in
  let eqns = Map.filter_keys t.eqns ~f:(Set.mem alive) in
  { t with eqns }
;;

let filter t ~f =
  let values = List.filter t.values ~f:(fun (s, _) -> f s) in
  prune ~is_root:f { t with values }
;;

let sinks eqns =
  let inner_nodes =
    Map.fold
      eqns
      ~init:(Set.empty (module String))
      ~f:(fun ~key:_ ~data:{ src1; src2; op = _ } acc -> Set.add (Set.add acc src1) src2)
  in
  let all_nodes = Map.keys eqns |> Set.of_list (module String) in
  Set.diff all_nodes inner_nodes
;;

type adder =
  { c_out : string
  ; s : string
  ; nodes : string list
  }

let isolate_adder t x_i y_i o =
  let prev_outputs =
    match o with
    | None -> Set.empty (module String)
    | Some (a, b) -> Set.of_list (module String) [ a; b ]
  in
  let t' =
    filter t ~f:(fun s ->
      String.equal s x_i || String.equal s y_i || Set.mem prev_outputs s)
  in
  let r = Set.diff (sinks t'.eqns) prev_outputs |> Set.to_list in
  match r with
  | [ a'; b' ] ->
    let c_out, s = if String.is_prefix a' ~prefix:"z" then b', a' else a', b' in
    Some { c_out; s; nodes = Map.keys t'.eqns }
  | [] -> None
  | l -> raise_s [%message "isolate_adder" (l : string list)]
;;

let rename_sym ~rename n _ =
  S_sym
    (match rename n with
     | Some n' -> n'
     | None -> n)
;;

let partial_eval t roots rename =
  let t =
    filter
      { t with values = List.map roots ~f:(fun s -> s, false) }
      ~f:(fun s -> List.mem roots s ~equal:String.equal)
  in
  eval t eval_sym (rename_sym ~rename)
;;

let ok_out = function
  | S_op
      ( OR
      , S_op (AND, S_op (XOR, S_sym "x", S_sym "y"), S_sym "c_in")
      , S_op (AND, S_sym "x", S_sym "y") ) -> true
  | S_op (OR, _, S_op (XOR, _, _)) -> false
  | S_op (XOR, _, _) -> false
  | S_op (AND, _, _) -> false
  | s -> raise_s [%message "ok_out" (s : sym)]
;;

let ok_s = function
  | S_op (XOR, S_op (XOR, S_sym "x", S_sym "y"), S_sym "c_in") -> true
  | S_op (AND, _, _) -> false
  | S_op (XOR, S_op (AND, _, _), _) -> false
  | S_op (OR, _, _) -> false
  | s -> raise_s [%message "ok_s" (s : sym)]
;;

let is_full_adder t ~x ~y ~c_in ~c_out ~s =
  let m =
    partial_eval t [ x; y; c_in ] (fun s ->
      match () with
      | _ when String.equal s x -> Some "x"
      | _ when String.equal s y -> Some "y"
      | _ when String.equal s c_in -> Some "c_in"
      | _ -> None)
  in
  match Map.find m c_out with
  | None -> false
  | Some sym_c_out -> ok_out sym_c_out && ok_s (Map.find_exn m s)
;;

let swap a b t =
  let eqns =
    Map.map_keys_exn
      (module String)
      t.eqns
      ~f:(fun s ->
        match () with
        | _ when String.equal s a -> b
        | _ when String.equal s b -> a
        | _ -> s)
  in
  { t with eqns }
;;

let all_swaps l =
  let open List.Let_syntax in
  let%bind a = l in
  let%bind b = l in
  let%bind () = Algo.guard String.(a < b) in
  return (a, b)
;;

let find_swap t nodes ~x ~y ~c_in ~c_out ~s =
  all_swaps nodes
  |> List.find_exn ~f:(fun (a, b) ->
    let t' = swap a b t in
    is_full_adder t' ~x ~y ~c_in ~c_out ~s)
;;

let f2 s =
  let t = parse s in
  let swaps =
    let rec go i o acc =
      let x = Printf.sprintf "x%02d" i in
      let y = Printf.sprintf "y%02d" i in
      match isolate_adder t x y o with
      | None -> acc
      | Some { c_out; s; nodes } ->
        let acc' =
          match o with
          | None -> acc
          | Some (c_in, _) ->
            if is_full_adder t ~x ~y ~c_in ~c_out ~s
            then acc
            else (
              let a, b = find_swap t nodes ~x ~y ~c_in ~c_out ~s in
              Set.add (Set.add acc a) b)
        in
        go (i + 1) (Some (c_out, s)) acc'
    in
    go 0 None (Set.empty (module String))
  in
  swaps |> Set.to_list |> String.concat ~sep:","
;;

let run () = Run.run_string ~f1 ~f2 Day24_input.data
