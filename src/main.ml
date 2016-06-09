open Format
open Lexing

let parse_only = ref false

let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s

let options =
  ["--parse-only", Arg.Set parse_only,
   "  Make only the parsing stage.";
   "--debug", Arg.Set Typing.debug,
   "  Debug mode.";]

let usage = "usage: mini-pascal [options] file.pas"

let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

let () =
  Arg.parse options (set_file ifile) usage;

  if !ifile="" then begin eprintf "No file to compile\n@?"; exit 1 end;

  if not (Filename.check_suffix !ifile ".pas") then begin
    eprintf "The input file must have the .pas extension\n@?";
    Arg.usage options usage;
    exit 1
  end;

  
  let f = open_in !ifile in

  let buf = Lexing.from_channel f in

  try
    let p = Parser.prog Lexer.token buf in
    close_in f;

    if !parse_only then exit 0;

    let p = Typing.prog p in
    let c = open_out (Filename.chop_suffix !ifile ".pas" ^ ".s") in
    let fmt = formatter_of_out_channel c in
    X86_64.print_program fmt (Compilex86.prog p);
    close_out c
  with
  | Lexer.Lexing_error c ->
    localisation (Lexing.lexeme_start_p buf);
    eprintf "Lexical error : %s@." c;
    exit 1
  | Parser.Error ->
    localisation (Lexing.lexeme_start_p buf);
    eprintf "Syntax error@.";
    exit 1
  | Typing.Error s->
    eprintf "Type error : %s@." s;
    exit 1
  | e ->
    eprintf "Anomaly: %s\n@." (Printexc.to_string e);
    exit 2
