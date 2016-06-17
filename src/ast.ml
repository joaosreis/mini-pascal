open Lexing

type tposition = Position of position * position

type tstring = TString | NString of int

type standard_type = Integer | Real | Character | String of tstring

type tarray = TArray of standard_type * int

type ttype = Array of tarray | Standard of standard_type

type const =
    Cint of int
  | Cfloat of float
  | Cchar of char
  | Cstring of string
  | Carray of tarray

type num_unop = Nneg

type num_binop = Nadd | Nsub | Nmul | Ndiv

type int_binop = Ipow

type literal_binop = Lconcat

type bool_unop = Bnot

type bool_binop = Band | Bor

type cmp = Beq | Bneq | Blt | Ble | Bgt | Bge

type unop =
  | Nunop of num_unop

type binop =
    Nbinop    of num_binop
  | Ibinop    of int_binop
  | Lbinop    of literal_binop

type op = Binop of binop | Unop of unop

type pexpr =
    PEconst of const * tposition
  | PEvar of string * tposition
  | PEbinop of binop * (pexpr * tposition) * (pexpr * tposition)
  | PEunop of unop * (pexpr * tposition)

  type pbool_expr =
      PBcmp    of cmp * pexpr * pexpr
    | PBbinop  of bool_binop * pbool_expr * pbool_expr
    | PBunop   of bool_unop * pbool_expr

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
  | PVar        of (string * ttype) list
  | PProcedure  of pprocedure

type parsed_program = pprocedure

type ident =
  { ident : string; level : int; offset : int; by_reference : by_reference }

type expr =
    Econst of const * ttype * tposition
  | Evar of ident * ttype * tposition
  | Ebinop of binop * (expr * tposition) * (expr * tposition) * ttype
  | Eunop of unop * (expr * tposition) * ttype
  | Eaddr of ident * ttype * tposition

  type bool_expr =
      Bcmp    of cmp * expr * expr
    | Bbinop  of bool_binop * bool_expr * bool_expr
    | Bunop   of bool_unop * bool_expr

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
  | Var       of (string * ttype) list
  | Procedure of procedure

type typed_program = procedure

let type_of_expr = function
    Econst (_, t, _) -> t
  | Evar (_, t, _) -> t
  | Ebinop (_, _, _, t) -> t
  | Eunop (_, _, t) -> t
  | Eaddr (_, t, _) -> t
