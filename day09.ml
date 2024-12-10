type block =
  { offset : int
  ; length : int
  ; id : int
  }
[@@deriving sexp]

type t = int option list * block list [@@deriving sexp]

let parse s =
  let chars = Queue.create () in
  let blocks = Queue.create () in
  let emit ~n x =
    Option.iter x ~f:(fun id ->
      let offset = Queue.length chars in
      let length = n in
      let block = { offset; length; id } in
      Queue.enqueue blocks block);
    for _ = 1 to n do
      Queue.enqueue chars x
    done
  in
  let id = ref 0 in
  let file = ref true in
  String.strip s
  |> String.iter ~f:(fun c ->
    assert (Char.('0' <= c && c <= '9'));
    let n = Char.to_int c - Char.to_int '0' in
    if !file
    then (
      emit ~n (Some !id);
      Int.incr id)
    else emit ~n None;
    file := not !file);
  Queue.to_list chars, Queue.to_list blocks
;;

let checksum = List.foldi ~init:0 ~f:(fun i acc n -> acc + (i * n))

let p1 (l, _) =
  let out = Queue.create () in
  let to_write = List.count l ~f:Option.is_some in
  let from_end = Queue.of_list (List.rev l) in
  let rec next_from_end () =
    match Queue.dequeue_exn from_end with
    | Some n -> n
    | None -> next_from_end ()
  in
  let exception Done in
  (try
     List.iter l ~f:(fun n ->
       n |> Option.value_or_thunk ~default:next_from_end |> Queue.enqueue out;
       if Queue.length out = to_write then raise Done)
   with
   | Done -> ());
  Queue.to_list out |> checksum
;;

let find_first_none a start =
  let a_len = Array.length a in
  let i = ref start in
  let exception Found of int in
  try
    while !i < a_len do
      if Option.is_none (Array.get a !i) then raise (Found !i);
      Int.incr i
    done;
    None
  with
  | Found i -> Some i
;;

let find_hole a min_length =
  let a_len = Array.length a in
  let rec go start =
    let first_empty = find_first_none a start in
    match first_empty with
    | None -> None
    | Some i ->
      let r = ref 0 in
      let j = ref i in
      while !j < a_len && Option.is_none (Array.get a !j) do
        Int.incr j;
        Int.incr r
      done;
      let hole_size = !r in
      if hole_size >= min_length then Some i else go (i + hole_size)
  in
  go 0
;;

let try_move a block =
  match find_hole a block.length with
  | Some hole_offset when hole_offset < block.offset ->
    Array.blit ~src:a ~dst:a ~src_pos:block.offset ~dst_pos:hole_offset ~len:block.length;
    for i = block.offset to block.offset + block.length - 1 do
      a.(i) <- None
    done
  | _ -> ()
;;

let p2 t =
  let l, blocks = t in
  let ids_to_move = List.rev blocks in
  let a = Array.of_list l in
  List.iter ids_to_move ~f:(fun block -> try_move a block);
  Array.to_list a |> List.map ~f:(Option.value ~default:0) |> checksum
;;

let%expect_test _ =
  let sample = "2333133121414131402" in
  let t = parse sample in
  print_s [%message (t : t) (p1 t : int) (p2 t : int)];
  [%expect
    {|
    ((t
      (((0) (0) () () () (1) (1) (1) () () () (2) () () () (3) (3) (3) ()
        (4) (4) () (5) (5) (5) (5) () (6) (6) (6) (6) () (7) (7) (7) () (8)
        (8) (8) (8) (9) (9))
       (((offset 0) (length 2) (id 0)) ((offset 5) (length 3) (id 1))
        ((offset 11) (length 1) (id 2)) ((offset 15) (length 3) (id 3))
        ((offset 19) (length 2) (id 4)) ((offset 22) (length 4) (id 5))
        ((offset 27) (length 4) (id 6)) ((offset 32) (length 3) (id 7))
        ((offset 36) (length 4) (id 8)) ((offset 40) (length 2) (id 9)))))
     ("p1 t" 1928) ("p2 t" 2858))
    |}]
;;

let f1 s = parse s |> p1
let f2 s = parse s |> p2
let run () = Run.run ~f1 ~f2 Day09_input.data
