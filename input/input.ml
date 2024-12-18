open Base
open Stdio

let input_file =
  let open Cmdliner.Arg in
  value & pos 0 (some string) None & info []
;;

let read_input_file = function
  | Some path -> In_channel.read_all path
  | None -> In_channel.input_all stdin
;;

type format =
  | Hex
  | Dots

module Dots = struct
  let base = 0x2800

  let encode s =
    let line_len = 80 in
    let b = Buffer.create 0 in
    let flush () =
      printf "%s\n" (Stdlib.Buffer.contents b);
      Stdlib.Buffer.clear b
    in
    String.iteri s ~f:(fun i c ->
      Stdlib.Buffer.add_utf_8_uchar b (Uchar.of_scalar_exn (base + Char.to_int c));
      if (i + 1) % line_len = 0 then flush ());
    flush ()
  ;;

  let decode s =
    let low = Uchar.of_scalar_exn base in
    let high = Uchar.of_scalar_exn (base + 0xff) in
    s
    |> String.Utf8.of_string
    |> String.Utf8.filter_map ~f:(fun c ->
      if Uchar.between c ~low ~high
      then Some (Uchar.of_scalar_exn (Uchar.to_scalar c - base))
      else if Uchar.to_scalar c = 0x0a
      then None
      else raise_s [%message "decode" (c : Uchar.t)])
    |> String.Utf8.to_string
  ;;
end

let format =
  let open Cmdliner.Arg in
  required & opt (some & enum [ "hex", Hex; "dots", Dots ]) None & info [ "format" ]
;;

module Encode = struct
  let info = Cmdliner.Cmd.info "encode"

  let term =
    let open Cmdliner.Term.Syntax in
    let+ input_file
    and+ format in
    let data = read_input_file input_file in
    match format with
    | Hex -> printf "%s\n" (Hex_encode.to_hex data)
    | Dots -> Dots.encode data
  ;;

  let cmd = Cmdliner.Cmd.v info term
end

module Decode = struct
  let info = Cmdliner.Cmd.info "decode"

  let term =
    let open Cmdliner.Term.Syntax in
    let+ input_file
    and+ format
    and+ raw =
      let open Cmdliner.Arg in
      value & flag & info [ "raw" ]
    in
    let encoded = read_input_file input_file in
    let data =
      match format with
      | Hex -> String.strip encoded |> Hex_encode.from_hex
      | Dots -> Dots.decode encoded
    in
    if raw then printf "%s" data else printf "let data = {|%s|}\n" data
  ;;

  let cmd = Cmdliner.Cmd.v info term
end

let info = Cmdliner.Cmd.info "input"
let cmd = Cmdliner.Cmd.group info [ Encode.cmd; Decode.cmd ]
let () = Cmdliner.Cmd.eval cmd |> Stdlib.exit
