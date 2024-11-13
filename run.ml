let run_gen ~f1 ~f2 data ~print =
  let info = Cmdliner.Cmd.info "AOC" in
  let term =
    let open Cmdliner.Term.Syntax in
    let+ part =
      let open Cmdliner.Arg in
      required & opt (some (enum [ "1", `One; "2", `Two ])) None & info [ "part" ]
    in
    let f =
      match part with
      | `One -> f1
      | `Two -> f2
    in
    print (f data)
  in
  let cmd = Cmdliner.Cmd.v info term in
  Cmdliner.Cmd.eval cmd |> Stdlib.exit
;;

let run ~f1 ~f2 data = run_gen ~f1 ~f2 data ~print:(printf "%d\n")
let run_string ~f1 ~f2 data = run_gen ~f1 ~f2 data ~print:(printf "%s\n")
