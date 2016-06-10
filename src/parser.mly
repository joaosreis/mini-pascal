%{
  open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STRING
%token <string> IDENT
%token TRUE
%token FALSE
%token COMMA DOT IF THEN PROGRAM COLONEQ BEGIN END ELSE PROCEDURE
%token LPAREN RPAREN INTEGER REAL CHARACTER TSTRING VAR COLON SEMICOLON WHILE DO
%token EXP PLUS MINUS TIMES DIV AND OR NOT
%token EQ NEQ LT LE GT GE

%left OR
%left AND
%nonassoc NOT
%left EXP
%left MINUS PLUS
%left TIMES DIV
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
  | i=int_expression    { PEint i }
  | f=float_expression  { PEfloat f }
  | c=char_expression   { PEchar c }
  | s=string_expression { PEstring s }

int_expression:
  | c=INT                                           { PIconst c }
  | id=IDENT                                        { PIvar id }
  | e1=int_expression o=int_binop e2=int_expression { PIbinop (o, e1, e2) }
  | op=num_unop e=int_expression %prec uminus       { PIunop (op, e) }
  | LPAREN e=int_expression RPAREN                  { e }
;

%inline int_binop:
  | PLUS  { Iadd }
  | MINUS { Isub }
  | TIMES { Imul }
  | DIV   { Idiv }
  | EXP   { Ipow }
;

float_expression:
  | c=FLOAT                                             { PFconst c }
  | id=IDENT                                            { PFvar id }
  | e1=float_expression o=float_binop e2=float_expression { PFbinop (o, e1, e2) }
  | op=num_unop e=float_expression %prec uminus         { PFunop (op, e) }
  | LPAREN e=float_expression RPAREN                    { e }
;

%inline float_binop:
  | PLUS  { Fadd }
  | MINUS { Fsub }
  | TIMES { Fmul }
  | DIV   { Fdiv }
;

%inline num_unop:
  | MINUS { Nneg }

char_expression:
  | c=CHAR                                                { PCconst c }
  | id=IDENT                                              { PCvar id }
  | e1=char_expression o=literal_binop e2=char_expression { PCbinop (o, e1, e2) }
  | LPAREN e=char_expression RPAREN                       { e }
;

string_expression:
  | c=STRING                                                  { PSconst c }
  | id=IDENT                                                  { PSvar id }
  | e1=string_expression o=literal_binop e2=string_expression { PSbinop (o, e1, e2) }
  | LPAREN e=string_expression RPAREN                         { e }
;

%inline literal_binop:
  | PLUS  { Lconcat }

condition:
  | e1=int_expression c=cmp e2=int_expression     { PBintcmp (c, e1, e2) }
  | e1=float_expression c=cmp e2=float_expression { PBfloatcmp (c, e1, e2) }
  | e1=char_expression c=cmp e2=char_expression   { PBcharcmp (c, e1, e2) }
  | c1=condition op=bool_binop c2=condition       { PBbinop (op, c1, c2) }
  | op=bool_unop c1=condition                     { PBunop (op, c1) }
  | LPAREN c=condition RPAREN                     { c }
;

%inline bool_unop:
  | NOT { Bnot }

%inline cmp:
  | EQ  { Beq }
  | NEQ { Bneq }
  | LT  { Blt }
  | LE  { Ble }
  | GT  { Bgt }
  | GE  { Bge }
;

%inline bool_binop:
  | AND { Band }
  | OR  { Bor }

formals:
  | bindings=separated_list(SEMICOLON, formal) (* list can be empty *){ bindings }

formal:
  | id=IDENT COLON t=types { id, false, t }
  | VAR id=IDENT COLON t=types { id, true, t }

binding:
  | ids=separated_nonempty_list(COMMA, IDENT) COLON t=types { ids, t }

terminated_bindings:
  | bindings=terminated(binding, SEMICOLON)+ (* list is nonempty *){ bindings }

decl:
  | VAR vars=terminated_bindings SEMICOLON  { PVar vars }
  | p=procedure                             { PProcedure p }

%inline types:
  | t=standard_types  { Standard t }

%inline standard_types:
  | INTEGER   { Integer }
  | REAL      { Real }
  | CHARACTER { Character }
  | TSTRING   { String }
