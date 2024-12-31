module G = Graph.Persistent.Graph.Concrete (String)

let parse s =
  let open Parsing_util in
  let open Angstrom in
  parse_lines_using (both (word <* char '-') word) s
  |> List.fold ~init:G.empty ~f:(fun acc (a, b) -> G.add_edge acc a b)
;;

let is_t = String.is_prefix ~prefix:"t"

let cliques3 t =
  let open List.Let_syntax in
  let nodes_l = G.fold_vertex (fun a acc -> a :: acc) t [] in
  let%bind a = nodes_l in
  let%bind b = nodes_l in
  let%bind () = Algo.guard String.(a < b) in
  let%bind () = Algo.guard (G.mem_edge t a b) in
  let%bind c = nodes_l in
  let%bind () = Algo.guard String.(b < c) in
  let%bind () = Algo.guard (G.mem_edge t b c) in
  let%bind () = Algo.guard (G.mem_edge t a c) in
  return [ a; b; c ]
;;

let p1 t = t |> cliques3 |> List.count ~f:(List.exists ~f:is_t)

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
