open Lexing

type tposition = Position of position * position

type tstring = TString | NString of int

type standard_type = Integer | Real | Character | String of tstring | Boolean

type tarray = TArray of standard_type * int

type ttype = Array of tarray | Standard of standard_type

type const =
    Cint of int
  | Cfloat of float
  | Cchar of char
  | Cstring of string
  | Cbool of bool
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
  | Bunop of bool_unop

type binop =
    Nbinop    of num_binop
  | Ibinop    of int_binop
  | Lbinop    of literal_binop
  | Bbinop    of bool_binop
  | Cmpbinop  of cmp

type op = Binop of binop | Unop of unop

type pexpr =
    PEconst of const * tposition
  | PEvar of string * tposition
  | PEbinop of binop * (pexpr * tposition) * (pexpr * tposition)
  | PEunop of unop * (pexpr * tposition)


type pstmt =
  | PSassign of string * pexpr
  | PSif     of pexpr * pstmt * pstmt
  | PSwhile  of pexpr * pstmt
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

type expr =
    Econst of const * tposition
  | Evar of ident * tposition
  | Ebinop of binop * (expr * tposition) * (expr * tposition)
  | Eunop of unop * (expr * tposition)
  | Eaddr of ident * tposition

type texpr = {
  etype: ttype;
  eexpr: expr;
}

type pident = { proc_name : string; proc_level : int }

type stmt =
  | Sassign  of ident * texpr
  | Sif      of texpr * stmt * stmt
  | Swhile   of texpr * stmt
  | Sblock   of stmt list
  | Scall    of pident * texpr list
  | Swriteln of texpr

type procedure =
  { pident  : pident;
    formals : formals;
    locals  : decl list;
    body    : stmt; }

and decl =
  | Var       of (string list * ttype) list
  | Procedure of procedure

type typed_program = procedure
