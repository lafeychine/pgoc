
(* Programme principal *)

open Format
open Lexing
open Lexer
open Parser

let usage = "usage: ./pgoc [options] file.go"

let debug = ref false
let parse_only = ref false
let type_only = ref false

let spec =
  [ "--debug", Arg.Set debug, "        runs in debug mode";
    "--parse-only", Arg.Set parse_only, "   stops after parsing";
    "--type-only", Arg.Set type_only, "    stops after typing";
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".go") then
      raise (Arg.Bad "no .go extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let debug = !debug
let type_only = !type_only

let report_loc loc =
  match loc with
  | Some (b, e) ->
    let l = b.pos_lnum in
    let fc = b.pos_cnum - b.pos_bol in
    let lc = e.pos_cnum - b.pos_bol in
    eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc
  | None -> eprintf "File \"%s\":\n" file

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = Parser.file Lexer.next_token lb in
    close_in c;

    if debug then begin
      let ast_dot_file = open_out (Filename.chop_suffix file ".go" ^ "_ast.dot") in
      Printf.fprintf ast_dot_file "%s" (Pretty.get_dot_ast f);
      close_out ast_dot_file
    end;

    if !parse_only then exit 0;

    let f = Typing.file f in

    if debug then begin
      let ast_dot_file = open_out (Filename.chop_suffix file ".go" ^ "_tast.dot") in
      Printf.fprintf ast_dot_file "%s" (Pretty.get_dot_tast f);
      close_out ast_dot_file
    end;

    if type_only then exit 0;

    let code = Compile.file f in

    let c = open_out (Filename.chop_suffix file ".go" ^ ".s") in
    let fmt = formatter_of_out_channel c in
    X86_64.print_program fmt code;
    close_out c
  with
  | Lexer.Lexing_error s ->
    report_loc (Some (lexeme_start_p lb, lexeme_end_p lb));
    eprintf "lexical error: %s\n@." s;
    exit 1

  | Parser.Error ->
    report_loc (Some (lexeme_start_p lb, lexeme_end_p lb));
    eprintf "syntax error\n@.";
    exit 1

  | Typing.Error (l, msg) ->
    report_loc l;
    eprintf "error: %s\n@." msg;
    exit 1

  | e ->
    eprintf "Anomaly: %s\n@." (Printexc.to_string e);
    exit 2
