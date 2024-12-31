module Edge = struct
  module T = struct
    type t = string * string [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)
end

type t =
  { nodes : Set.M(String).t
  ; edges : Set.M(Edge).t
  }
[@@deriving sexp]

let sorted (a, b) = if String.(a < b) then a, b else b, a

let parse s =
  let open Parsing_util in
  let open Angstrom in
  let edge_list = parse_lines_using (both (word <* char '-') word) s in
  let edges =
    List.fold
      edge_list
      ~init:(Set.empty (module Edge))
      ~f:(fun acc (a, b) ->
        let a, b = sorted (a, b) in
        Set.add acc (a, b))
  in
  let nodes =
    Set.fold
      edges
      ~init:(Set.empty (module String))
      ~f:(fun acc (a, b) -> Set.add (Set.add acc a) b)
  in
  { nodes; edges }
;;

let cliques3 { nodes; edges } =
  let open List.Let_syntax in
  let nodes_l = Set.to_list nodes in
  let%bind a = nodes_l in
  let%bind b = nodes_l in
  let%bind () = Algo.guard (Set.mem edges (a, b)) in
  let%bind c = nodes_l in
  let%bind () = Algo.guard (Set.mem edges (b, c)) in
  let%bind () = Algo.guard (Set.mem edges (a, c)) in
  return (a, b, c)
;;

let is_t = String.is_prefix ~prefix:"t"
let p1 t = List.count (cliques3 t) ~f:(fun (a, b, c) -> is_t a || is_t b || is_t c)

module G = struct
  type nonrec t = t

  module V = String

  let succ t s =
    Set.filter_map
      (module String)
      t.edges
      ~f:(fun (a, b) ->
        if String.equal a s then Some b else if String.equal b s then Some a else None)
    |> Set.to_list
  ;;

  let fold_vertex f t init = Set.fold t.nodes ~init ~f:(fun acc s -> f s acc)
end

module Maximal_clique = Graph.Clique.Bron_Kerbosch (G)

let p2 t =
  t
  |> Maximal_clique.maximalcliques
  |> List.max_elt ~compare:Stdlib.List.compare_lengths
  |> Option.value_exn
  |> String.concat ~sep:","
;;

let%expect_test _ =
  let t =
    parse
      (String.concat_lines
         [ "kh-tc"
         ; "qp-kh"
         ; "de-cg"
         ; "ka-co"
         ; "yn-aq"
         ; "qp-ub"
         ; "cg-tb"
         ; "vc-aq"
         ; "tb-ka"
         ; "wh-tc"
         ; "yn-cg"
         ; "kh-ub"
         ; "ta-co"
         ; "de-co"
         ; "tc-td"
         ; "tb-wq"
         ; "wh-td"
         ; "ta-ka"
         ; "td-qp"
         ; "aq-cg"
         ; "wq-ub"
         ; "ub-vc"
         ; "de-ta"
         ; "wq-aq"
         ; "wq-vc"
         ; "wh-yn"
         ; "ka-de"
         ; "kh-ta"
         ; "co-tc"
         ; "wh-qp"
         ; "tb-vc"
         ; "td-yn"
         ])
  in
  print_s [%message (p1 t : int) (p2 t : string)];
  [%expect {| (("p1 t" 7) ("p2 t" co,de,ka,ta)) |}]
;;

let f1 s = parse s |> p1 |> Int.to_string
let f2 s = parse s |> p2
let run () = Run.run_string ~f1 ~f2 Day23_input.data
