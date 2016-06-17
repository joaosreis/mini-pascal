open Ast
open Lexing

let string_of_std_type = function
    Integer ->    "integer"
  | Real ->       "real"
  | Character ->  "char"
  | String(_) ->  "string"

let string_of_type = function
    Array (TArray (t, _)) ->  string_of_std_type t ^ " array"
  | Standard t ->             string_of_std_type t

let string_of_expr_pos e = let Position(s, e) = match e with
      Econst(_, _, pos) -> pos
    | Evar(_, _, pos) -> pos
    | Ebinop(_, (_, Position(start_pos1, _)), (_, Position(_, end_pos2)), _) ->
      Position(start_pos1, end_pos2)
    | Eunop(_, (_, pos), _) -> pos
    | Eaddr(_, _, pos) -> pos
  in Printf.sprintf "File \"%s\", line %d, characters %d-%d" s.pos_fname s.pos_lnum (s.pos_cnum - s.pos_bol) (e.pos_cnum - e.pos_bol)

let string_of_operator = function
    Binop op -> (match op with
      Nbinop o -> (match o with
        Nadd -> "+"
      | Nsub -> "-"
      | Nmul -> "*"
      | Ndiv -> "/")
    | Ibinop o -> (match o with
          Ipow -> "**")
    | Lbinop o -> (match o with
          Lconcat -> "+"))
  | Unop op -> (match op with
        Nunop o -> (match o with
            Nneg -> "-"))
