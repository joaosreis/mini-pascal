
/* Analyseur syntaxique pour Arith */

%{
  open Ast
%}

%token <int> CST
%token <string> IDENT
%token COMMA DOT IF THEN PROGRAM COLONEQ BEGIN END ELSE PROCEDURE
%token LPAREN RPAREN INTEGER VAR COLON SEMICOLON WHILE DO
%token PLUS MINUS TIMES DIV AND OR NOT
%token EQ NEQ LT LE GT GE

/* D�finitions des priorit�s et associativit�s des tokens */

%left OR
%left AND
%nonassoc NOT
%left MINUS PLUS
%left TIMES DIV
%nonassoc uminus
%nonassoc THEN
%nonassoc ELSE

/* Point d'entr�e de la grammaire */
%start prog

/* Type des valeurs retourn�es par l'analyseur syntaxique */
%type <Ast.parsed_program> prog

%%

prog:
  PROGRAM p = IDENT SEMICOLON
  decls = decl*
  main = block
  DOT
    { { pname = p;
	pformals = [];
        plocals = decls;
        pbody = main } }

procedure:
| PROCEDURE f = IDENT
  LPAREN formals = formals RPAREN SEMICOLON
  locals = decl* body = block SEMICOLON
    { { pname = f;
	pformals = formals;
        plocals = locals;
        pbody = body } }

stmt:
| id = IDENT LPAREN actuals = separated_list(COMMA, expression) RPAREN
    { PScall (id, actuals) }
| id = IDENT COLONEQ e = expression
    { PSassign (id, e) }
| IF c = condition THEN b = stmt_or_block
    { PSif (c, b, PSblock []) }
| IF c = condition THEN b1 = stmt_or_block ELSE b2 = stmt_or_block
    { PSif (c, b1, b2) }
| WHILE c = condition DO b = stmt_or_block
    { PSwhile (c, b) }

block:
| BEGIN is = separated_list(SEMICOLON, stmt) END
    { PSblock is }

stmt_or_block:
| i = stmt
    { i }
| b = block
    { b }

expression:
| c = CST                        { PEconst c }
| id = IDENT                     { PEvar id }
| e1 = expression o = op e2 = expression { PEbinop (o, e1, e2) }
| MINUS e = expression %prec uminus    { PEbinop (Bsub, PEconst 0, e) } 
| LPAREN e = expression RPAREN         { e }
;

%inline op:
| PLUS  { Badd }
| MINUS { Bsub }
| TIMES { Bmul }
| DIV   { Bdiv }
;

condition:
| e1 = expression c = cmp e2 = expression { PBcmp (c, e1, e2) }
| c1 = condition AND c2 = condition       { PBand (c1, c2) }
| c1 = condition OR  c2 = condition       { PBor  (c1, c2) }
| NOT c1 = condition                      { PBnot c1 }
| LPAREN c = condition RPAREN             { c }
;

%inline cmp:
| EQ  { Beq }
| NEQ { Bneq }
| LT  { Blt }
| LE  { Ble }
| GT  { Bgt }
| GE  { Bge }
;

formals:
| bindings = separated_list(SEMICOLON, formal) (* list can be empty *)
    { bindings }

formal:
| id = IDENT COLON INTEGER
    { id, false }
| VAR id = IDENT COLON INTEGER
    { id, true }

binding:
| ids = separated_nonempty_list(COMMA, IDENT) COLON INTEGER
    { ids }

terminated_bindings:
| bindings = terminated(binding, SEMICOLON)+    (* list is nonempty *)
    { List.flatten bindings }

decl:
| VAR vars = terminated_bindings
    { PVar vars }
| p = procedure
    { PProcedure p }

