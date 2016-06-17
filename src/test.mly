%{
  open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STRING
%token <string> IDENT
%token COMMA DOT IF THEN PROGRAM COLONEQ BEGIN END ELSE PROCEDURE
%token LPAREN RPAREN LBRACKET RBRACKET INTEGER REAL CHARACTER TSTRING VAR COLON SEMICOLON WHILE DO /*TYPE RECORD*/
%token EXP PLUS MINUS TIMES DIV CONCAT AND OR NOT
%token EQ NEQ LT LE GT GE

%left OR
%left AND
%nonassoc NOT
%left EXP
%left MINUS PLUS
%left TIMES DIV
%left CONCAT
%left uminus
%nonassoc THEN
%nonassoc ELSE

%start expression

%type <Ast.parsed_program> expression

%%

expression:
  | c=constant                          { PEconst (c, Position($startpos, $endpos)) }
  | id=IDENT                            { PEvar (id, Position($startpos, $endpos)) }
  | e1=expression o=binop e2=expression { PEbinop (o, (e1, Position($startpos(e1), $endpos(e1))), (e2, Position($startpos(e2), $endpos(e2)))) }
  | op=unop e=expression %prec uminus   { PEunop (op, (e, Position($startpos(e), $endpos(e)))) }
  | LPAREN e=expression RPAREN          { e }
;

%inline constant:
  | c=INT     { Cint c }
  | c=FLOAT   { Cfloat c }
  | c=CHAR    { Cchar c }
  | c=STRING  { Cstring c }

binop:
  | o=num_binop     { Nbinop(o) }
  | o=int_binop     { Ibinop(o) }
  | o=literal_binop { Lbinop(o) }

%inline num_binop:
  | PLUS  { Nadd }
  | MINUS { Nsub }
  | TIMES { Nmul }
  | DIV   { Ndiv }
;

%inline int_binop:
  | EXP  { Ipow }
;

%inline literal_binop:
  | CONCAT  { Lconcat }

%inline unop:
  | o=num_unop   { Nunop(o) }

%inline num_unop:
  | MINUS { Nneg }
