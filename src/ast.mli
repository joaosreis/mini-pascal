type binop = Badd | Bsub | Bmul | Bdiv

type pint_expr =
  | PEconst of int
  | PEvar   of string
  | PEbinop of binop * pint_expr * pint_expr

type cmp = Beq | Bneq | Blt | Ble | Bgt | Bge

type pbool_expr =
  | PBcmp of cmp * pint_expr * pint_expr
  | PBand of pbool_expr * pbool_expr
  | PBor  of pbool_expr * pbool_expr
  | PBnot of pbool_expr

type pstmt =
  | PSassign of string * pint_expr
  | PSif     of pbool_expr * pstmt * pstmt
  | PSwhile  of pbool_expr * pstmt
  | PSblock  of pstmt list
  | PScall   of string * pint_expr list

type by_reference = bool

type formals = (string * by_reference) list

type pprocedure =
  { pname    : string;
    pformals : formals;
    plocals  : pdecl list;
    pbody    : pstmt; }

and pdecl =
  | PVar of string list
  | PProcedure of pprocedure

type parsed_program = pprocedure

type ident =
    { ident : string; level : int; offset : int; by_reference : by_reference }

type int_expr =
  | Econst of int
  | Evar   of ident
  | Ebinop of binop * int_expr * int_expr
  | Eaddr  of ident

type bool_expr =
  | Bcmp of cmp * int_expr * int_expr
  | Band of bool_expr * bool_expr
  | Bor  of bool_expr * bool_expr
  | Bnot of bool_expr

type pident = { proc_name : string; proc_level : int }

type stmt =
  | Sassign  of ident * int_expr
  | Sif      of bool_expr * stmt * stmt
  | Swhile   of bool_expr * stmt
  | Sblock   of stmt list
  | Scall    of pident * int_expr list
  | Swriteln of int_expr

type procedure =
  { pident  : pident;
    formals : formals;
    locals  : decl list;
    body    : stmt; }

and decl =
  | Var       of string list
  | Procedure of procedure

type typed_program = procedure
