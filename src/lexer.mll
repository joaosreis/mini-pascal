{
open Lexing
open Parser

exception SyntaxError of string

let kwd_tbl =
  ["and", AND; "or", OR; "not", NOT;
   "if", IF; "then", THEN; "else", ELSE;
   "program", PROGRAM; "var", VAR; "begin", BEGIN; "end", END;
   "integer", INTEGER; "real", REAL; "char", CHARACTER; "string", TSTRING;
   "procedure", PROCEDURE; "while", WHILE; "do", DO;
  ]

let id_or_kwd =
  let h = Hashtbl.create 17 in
  List.iter (fun (s,t) -> Hashtbl.add h s t) kwd_tbl;
  fun s ->
    let s = String.lowercase s in
    try List.assoc s kwd_tbl with _ -> IDENT s

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter+ digit*
let character = "''" letter "'"
let integer = ['0'-'9']+
let frac = '.' digit*
let real = digit* frac?
let space = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | newline       { next_line lexbuf; read lexbuf }
  | space+        { read lexbuf }
  | ident as id   { id_or_kwd id }
  | "**"          { EXP }
  | '+'           { PLUS }
  | '-'           { MINUS }
  | '*'           { TIMES }
  | '/'           { DIV }
  | '='           { EQ }
  | "<>"          { NEQ }
  | '<'           { LT }
  | "<="          { LE }
  | '>'           { GT }
  | ">="          { GE }
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | '['           { LBRACKET }
  | ']'           { RBRACKET }
  | '.'           { DOT }
  | ','           { COMMA }
  | ':'           { COLON }
  | ":="          { COLONEQ }
  | ";"           { SEMICOLON }
  | "(*" | "{"    { comment lexbuf }
  | '"'           { read_string (Buffer.create 17) lexbuf }
  | "true"        { TRUE }
  | "false"       { FALSE }
  | real as s     { FLOAT (float_of_string s) }
  | integer as s  { INT (int_of_string s) }

  | eof           { raise (SyntaxError "reached end of file") }
  | _ as c        { raise (SyntaxError ("illegal character: " ^ String.make 1 c)) }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

and comment = parse
  | "*)" | "}"  { token lexbuf }
  | _           { comment lexbuf }
  | eof         { raise (SyntaxError ("unterminated comment")) }
