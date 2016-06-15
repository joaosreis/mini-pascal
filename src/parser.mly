%{
  open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STRING
%token <bool> BOOL
%token <string> IDENT
%token TRUE
%token FALSE
%token COMMA DOT IF THEN PROGRAM COLONEQ BEGIN END ELSE PROCEDURE
%token LPAREN RPAREN LBRACKET RBRACKET INTEGER REAL CHARACTER TSTRING BOOLEAN VAR COLON SEMICOLON WHILE DO TYPE RECORD
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
  | c=constant                          { PEconst c }
  | id=IDENT                            { PEvar id }
  | e1=expression o=binop e2=expression { PEbinop (o, e1, e2) }
  | op=unop e=expression %prec uminus   { PEunop (op, e) }
  | LPAREN e=expression RPAREN          { e }
;

constant:
  | c=INT     { Cint c }
  | c=FLOAT   { Cfloat c }
  | c=CHAR    { Cchar c }
  | c=STRING  { Cstring c }
  | c=BOOL    { Cbool c }

binop:
  | o=num_binop     { Nbinop(o) }
  | o=int_binop     { Ibinop(o) }
  | o=literal_binop { Lbinop(o) }
  | o=bool_binop    { Bbinop(o) }
  | o=cmp           { Cmpbinop(o) }

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
  | PLUS  { Lconcat }

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
  | o=bool_unop  { Bunop(o) }

%inline num_unop:
  | MINUS { Nneg }

%inline bool_unop:
    | NOT { Bnot }

condition:
  | e1=expression c=cmp e2=expression       { PEbinop (Cmpbinop c, e1, e2) }
  | c1=condition op=bool_binop c2=condition { PEbinop (Bbinop op, c1, c2) }
  | op=bool_unop c1=condition               { PEunop (Bunop op, c1) }
  | LPAREN c=condition RPAREN               { c }
;

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

types:
  | t=standard_types LBRACKET s=INT RBRACKET  { Array (TArray (t,s)) }
  | t=standard_types                          { Standard t }

%inline standard_types:
  | INTEGER                         { Integer }
  | REAL                            { Real }
  | CHARACTER                       { Character }
  | TSTRING LBRACKET s=INT RBRACKET { String (NString s) }
  | TSTRING                         { String (NString 255) }
  | BOOLEAN                         { Boolean }
