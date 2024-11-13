open Stdio

let input_file =
  let open Cmdliner.Arg in
  required & pos 0 (some string) None & info []
;;

module Encode = struct
  let info = Cmdliner.Cmd.info "encode"

  let term =
    let open Cmdliner.Term.Syntax in
    let+ input_file in
    let data = In_channel.read_all input_file in
    printf "%s\n" (Hex_encode.to_hex data)
  ;;

  let cmd = Cmdliner.Cmd.v info term
end

module Decode = struct
  let info = Cmdliner.Cmd.info "decode"

  let term =
    let open Cmdliner.Term.Syntax in
    let+ input_file in
    let data = In_channel.read_all input_file |> String.trim in
    printf "let data = {|%s|}\n" (Hex_encode.from_hex data)
  ;;

  let cmd = Cmdliner.Cmd.v info term
end

let info = Cmdliner.Cmd.info "input"
let cmd = Cmdliner.Cmd.group info [ Encode.cmd; Decode.cmd ]
let () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
