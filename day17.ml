type 'a t =
  { a : 'a
  ; b : 'a
  ; c : 'a
  ; program : int Map.M(Int).t
  ; ip : int
  }
[@@deriving sexp]

let parse =
  let open Parsing_util in
  let open Angstrom in
  let register name =
    string "Register " *> string name *> string ": " *> number <* end_of_line
  in
  let program = string "Program: " *> sep_by1 (char ',') number <* end_of_line in
  parse_using
    (let+ a = register "A"
     and+ b = register "B"
     and+ c = register "C"
     and+ program_l = end_of_line *> program in
     let program =
       program_l |> List.mapi ~f:(fun i x -> i, x) |> Map.of_alist_exn (module Int)
     in
     { a; b; c; program; ip = 0 })
;;

let literal_arg t = Map.find_exn t.program (t.ip + 1)

let combo_arg t =
  match literal_arg t with
  | 1 -> 1
  | 3 -> 3
  | 4 -> t.a
  | 5 -> t.b
  | n -> raise_s [%message "combo_arg" (n : int)]
;;

let divide t =
  let a = t.a in
  let v = combo_arg t in
  let b = 2 ** v in
  a / b
;;

let interpret t0 =
  let out = Queue.create () in
  let rec go t =
    match Map.find t.program t.ip with
    | Some 0 ->
      let r = divide t in
      go { t with a = r; ip = t.ip + 2 }
    | Some 1 ->
      let v = Map.find_exn t.program (t.ip + 1) in
      let r = t.b lxor v in
      go { t with b = r; ip = t.ip + 2 }
    | Some 2 ->
      let v = combo_arg t in
      let b = v % 8 in
      go { t with b; ip = t.ip + 2 }
    | Some 3 ->
      let ip = if t.a = 0 then t.ip + 2 else literal_arg t in
      go { t with ip }
    | Some 4 ->
      let r = t.b lxor t.c in
      go { t with b = r; ip = t.ip + 2 }
    | Some 5 ->
      let v = combo_arg t in
      Queue.enqueue out (v % 8);
      go { t with ip = t.ip + 2 }
    | Some 7 ->
      let r = divide t in
      go { t with c = r; ip = t.ip + 2 }
    | None -> ()
    | n -> raise_s [%message "interpret" (n : int option)]
  in
  go t0;
  Queue.to_list out
;;

let f1 s = parse s |> interpret |> List.map ~f:Int.to_string |> String.concat ~sep:","

let%expect_test "f1" =
  [ "Register A: 729"; "Register B: 0"; "Register C: 0"; ""; "Program: 0,1,5,4,3,0" ]
  |> String.concat_lines
  |> f1
  |> print_endline;
  [%expect {| 4,6,3,5,6,3,5,2,1,0 |}]
;;

let rec from_int3s = function
  | [] -> assert false
  | [ x ] -> x
  | x :: xs -> (from_int3s xs * 8) + x
;;

let%expect_test "from_int3s" =
  [ [ 2 ]; [ 0; 1 ]; [ 1; 1 ]; [ 0; 0; 1 ]; [ 0 ] ]
  |> List.iter ~f:(fun l -> print_s [%message (l : int list) (from_int3s l : int)]);
  [%expect
    {|
    ((l (2)) ("from_int3s l" 2))
    ((l (0 1)) ("from_int3s l" 8))
    ((l (1 1)) ("from_int3s l" 9))
    ((l (0 0 1)) ("from_int3s l" 64))
    ((l (0)) ("from_int3s l" 0))
    |}]
;;

let f2 s =
  let t = parse s in
  let interp a = interpret { t with a } in
  let pad l =
    let target_length = Map.length t.program in
    let n = List.length l in
    List.init (target_length - n) ~f:(fun _ -> 1) @ l
  in
  let find_next ~target ~prefix =
    List.range 0 8
    |> List.filter_map ~f:(fun i ->
      let input = i :: prefix |> pad in
      let r = interp (from_int3s input) in
      let ok = List.is_suffix ~suffix:target r ~equal:Int.equal in
      Option.some_if ok i)
  in
  let last_off, _ = Map.max_elt_exn t.program in
  let out i = Map.find_exn t.program i in
  let r_out i = out (last_off - i) in
  let dto n = List.range 0 n ~stop:`inclusive |> List.rev |> List.map ~f:r_out in
  let rec go i prefix =
    if i = 16
    then [ from_int3s prefix ]
    else
      let open List.Let_syntax in
      let%bind a = find_next ~target:(dto i) ~prefix in
      let prefix = a :: prefix in
      go (i + 1) prefix
  in
  let solutions = go 0 [] in
  List.min_elt solutions ~compare:Int.compare |> Option.value_exn |> Int.to_string
;;

let run () = Run.run_string ~f1 ~f2 Day17_input.data
