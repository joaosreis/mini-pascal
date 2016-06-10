type standard_type = Integer | Real | Character | String of int

type ttype = Array of standard_type * int | Standard of standard_type

type num_unop = Nneg

type int_binop = Iadd | Isub | Imul | Idiv | Ipow

type pint_expr =
  | PIconst of int
  | PIvar   of string
  | PIbinop of int_binop * pint_expr * pint_expr
  | PIunop  of num_unop * pint_expr

type float_binop = Fadd | Fsub | Fmul | Fdiv

type pfloat_expr =
  | PFconst of float
  | PFvar   of string
  | PFbinop of float_binop * pfloat_expr * pfloat_expr
  | PFunop  of num_unop * pfloat_expr

type literal_binop = Lconcat

type pchar_expr =
  | PCconst of char
  | PCvar   of string
  | PCbinop of literal_binop * pchar_expr * pchar_expr

type pstring_expr =
  | PSconst of string
  | PSvar   of string
  | PSbinop of literal_binop * pstring_expr * pstring_expr

type cmp = Beq | Bneq | Blt | Ble | Bgt | Bge

type bool_unop = Bnot

type bool_binop = Band | Bor

type pbool_expr =
  | PBbinop     of bool_binop * pbool_expr * pbool_expr
  | PBunop      of bool_unop * pbool_expr
  | PBintcmp    of cmp * pint_expr * pint_expr
  | PBfloatcmp  of cmp * pfloat_expr * pfloat_expr
  | PBcharcmp   of cmp * pchar_expr * pchar_expr

type pexpr =
  | PEint     of pint_expr
  | PEfloat   of pfloat_expr
  | PEchar    of pchar_expr
  | PEstring  of pstring_expr

type pstmt =
  | PSassign of string * pexpr
  | PSif     of pbool_expr * pstmt * pstmt
  | PSwhile  of pbool_expr * pstmt
  | PSblock  of pstmt list
  | PScall   of string * pexpr list

type by_reference = bool

type formals = (string * by_reference * ttype) list

type pprocedure =
  { pname    : string;
    pformals : formals;
    plocals  : pdecl list;
    pbody    : pstmt; }

and pdecl =
  | PVar        of (string list * ttype) list
  | PProcedure  of pprocedure

type parsed_program = pprocedure

type ident =
  { ident : string; level : int; offset : int; by_reference : by_reference }

type int_expr =
  | Iconst  of int
  | Ivar    of ident
  | Ibinop  of int_binop * int_expr * int_expr
  | Iunop   of num_unop * int_expr
  | Iaddr   of ident

type float_expr =
  | Fconst  of float
  | Fvar    of ident
  | Fbinop  of float_binop * float_expr * float_expr
  | Funop   of num_unop * float_expr
  | Faddr   of ident

type char_expr =
  | Cconst  of char
  | Cvar    of ident
  | Cbinop  of literal_binop * char_expr * char_expr
  | Caddr   of ident

type string_expr =
  | Sconst  of string
  | Svar    of ident
  | Sbinop  of literal_binop * string_expr * string_expr
  | Saddr   of ident

type bool_expr =
  | Bbinop    of bool_binop * bool_expr * bool_expr
  | Bunop     of bool_unop * bool_expr
  | Bintcmp   of cmp * int_expr * int_expr
  | Bfloatcmp of cmp * float_expr * float_expr
  | Bcharcmp  of cmp * char_expr * char_expr

type expr =
  | Eint    of int_expr
  | Efloat  of float_expr
  | Echar   of char_expr
  | Estring of string_expr

type pident = { proc_name : string; proc_level : int }

type stmt =
  | Sassign  of ident * expr
  | Sif      of bool_expr * stmt * stmt
  | Swhile   of bool_expr * stmt
  | Sblock   of stmt list
  | Scall    of pident * expr list
  | Swriteln of expr

type procedure =
  { pident  : pident;
    formals : formals;
    locals  : decl list;
    body    : stmt; }

and decl =
  | Var       of (string list * ttype) list
  | Procedure of procedure

type typed_program = procedure
