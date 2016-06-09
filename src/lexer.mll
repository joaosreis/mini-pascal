{
  open Lexing
  open Parser

  exception Lexing_error of string

  let kwd_tbl =
    ["and", AND; "or", OR; "not", NOT;
     "if", IF; "then", THEN; "else", ELSE;
     "program", PROGRAM; "var", VAR; "begin", BEGIN; "end", END;
     "integer", INTEGER; "procedure", PROCEDURE;
     "while", WHILE; "do", DO;
    ]

  let id_or_kwd =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,t) -> Hashtbl.add h s t) kwd_tbl;
    fun s ->
      let s = String.lowercase s in
      try List.assoc s kwd_tbl with _ -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter+ digit*
let integer = ['0'-'9']+
let space = [' ' '\t']

rule token = parse
  | '\n'    { newline lexbuf; token lexbuf }
  | space+  { token lexbuf }
  | ident as id { id_or_kwd id }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '/'     { DIV }
  | '='     { EQ }
  | "<>"     { NEQ }
  | '<'     { LT }
  | "<="     { LE }
  | '>'     { GT }
  | ">="     { GE }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '.'     { DOT }
  | ','     { COMMA }
  | ':'     { COLON }
  | ":="    { COLONEQ }
  | ";"     { SEMICOLON }
  | "(*" | "{"
            { comment lexbuf }
  | integer as s { CST (int_of_string s) }
  | eof     { raise (Lexing_error "reached end of file") }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

and comment = parse
  | "*)" | "}"
            { token lexbuf }
  | _       { comment lexbuf }
  | eof     { raise (Lexing_error ("unterminated comment")) }
