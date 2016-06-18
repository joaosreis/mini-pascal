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
%token EXP PLUS MINUS TIMES DIV MOD CONCAT AND OR NOT
%token EQ NEQ LT LE GT GE

%left OR
%left AND
%nonassoc NOT
%left MINUS PLUS
%left TIMES DIV MOD
%left EXP
%left CONCAT
%nonassoc uminus
%nonassoc THEN
%nonassoc ELSE

%start prog

%type <Ast.parsed_program> prog

%%

prog: | PROGRAM p=IDENT SEMICOLON decls=decl* main=block DOT
  {{ pname = p; pformals = []; plocals = decls; pbody = main }};

procedure:
  | PROCEDURE f=IDENT
      LPAREN formals=formals RPAREN SEMICOLON
      locals=decl* body=block SEMICOLON
      {{ pname = f; pformals = formals; plocals = locals; pbody = body }}

stmt:
  | id=IDENT LPAREN actuals=separated_list(COMMA, expression) RPAREN  { PScall (id, actuals) }
  | id=IDENT COLONEQ e=expression                                     { PSassign (id, e) }
  | IF c=condition THEN b=stmt_or_block                               { PSif (c, b, PSblock []) }
  | IF c=condition THEN b1=stmt_or_block ELSE b2=stmt_or_block        { PSif (c, b1, b2) }
  | WHILE c=condition DO b=stmt_or_block                              { PSwhile (c, b) }

block:
  | BEGIN is=separated_list(SEMICOLON, stmt) END { PSblock is }

stmt_or_block:
  | i=stmt  { i }
  | b=block { b }

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

%inline binop:
  | o=num_binop     { Nbinop(o) }
  | o=int_binop     { Ibinop(o) }
  | o=literal_binop { Lbinop(o) }

%inline num_binop:
  | PLUS  { Nadd }
  | MINUS { Nsub }
  | TIMES { Nmul }
  | DIV   { Ndiv }
  | EXP   { Npow }
;

%inline int_binop:
  | MOD { Imod }
;

%inline literal_binop:
  | CONCAT  { Lconcat }

%inline bool_binop:
  | AND { Band }
  | OR  { Bor }

%inline cmp:
  | EQ  { Beq }
  | NEQ { Bneq }
  | LT  { Blt }
  | LE  { Ble }
  | GT  { Bgt }
  | GE  { Bge }
;

%inline unop:
  | o=num_unop   { Nunop(o) }

%inline num_unop:
  | MINUS { Nneg }

%inline bool_unop:
    | NOT { Bnot }

condition:
  | e1=expression c=cmp e2=expression       { PBcmp (c, e1, e2) }
  | c1=condition op=bool_binop c2=condition { PBbinop (op, c1, c2) }
  | op=bool_unop c1=condition               { PBunop (op, c1) }
  | LPAREN c=condition RPAREN               { c }
;

formals:
  | bindings=separated_list(SEMICOLON, formal) (* list can be empty *){ bindings }

formal:
  | id=IDENT COLON t=types { id, false, t }
  | VAR id=IDENT COLON t=types { id, true, t }

binding:
  | ids=separated_nonempty_list(COMMA, IDENT) COLON t=types { List.map (fun a -> (a,t)) ids }

terminated_bindings:
  | bindings=terminated(binding, SEMICOLON)+ (* list is nonempty *){ List.flatten bindings }

decl:
  | VAR vars=terminated_bindings  { PVar vars }
  | p=procedure                   { PProcedure p }

types:
  | t=standard_types LBRACKET s=INT RBRACKET  { Array (TArray (t,s)) }
  | t=standard_types                          { Standard t }

standard_types:
  | s=string                        { String s }
  | INTEGER                         { Integer }
  | REAL                            { Real }
  | CHARACTER                       { Character }

%inline string:
  | TSTRING                         { NString 255 }
  | TSTRING s=INT                   { NString s }
