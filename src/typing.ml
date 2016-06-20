open Ast
open PrettyPrint

let debug = ref false

exception Error of string

let error s = raise (Error s)
let unbound_var v = error ("unbound variable " ^ v)
let unbound_proc p = error ("unbound procedure " ^ p)
let reference_expected x = error ("expecting a left value for formal " ^ x)
let bad_arity p a =
  error ("bad arity: procedure p expects " ^ string_of_int a ^ " arguments")
let invalid_operand op t e =
  error ("operator " ^ string_of_operator op ^ " expects operand of type " ^ t ^ " but got " ^ string_of_type (type_of_expr e) ^ "\n" ^ string_of_expr_pos e)
let incompatible_types t1 t2 e =
  error ("expected expression of type " ^ t1 ^ " but expression is of type " ^ t2 ^ "\n" ^ (string_of_expr_pos e))
let invalid_argument m e = error (m ^ "\n" ^ string_of_expr_pos e)

let unique =
  let r = ref 0 in fun s -> incr r; s ^ "__" ^ string_of_int !r

module Env = Map.Make(String)

type env =
  { vars : (ident * ttype) Env.t;
    procs : (pident * formals) Env.t }

let resolve_binop e1 e2 op =
  let t1 = type_of_expr e1 in
  let t2 = type_of_expr e2 in
  match op with
    Nbinop op ->
    if t1 <> Standard(Integer) && t1 <> Standard(Real) then
      invalid_operand (Binop (Nbinop op)) "numerical" e1
    else if t2 <> Standard(Integer) && t2 <> Standard(Real) then
      invalid_operand (Binop (Nbinop op)) "numerical" e2
    else if t1 <> t2 then
      incompatible_types (string_of_type t1) (string_of_type t2) e2
    else
      t1
  | Ibinop op ->
    if t1 <> Standard(Integer) then
      invalid_operand (Binop (Ibinop op)) "integer" e1
    else if t2 <> Standard(Integer) then
      invalid_operand (Binop (Ibinop op)) "integer" e2
    else
      Standard(Integer)
  | Lbinop op ->
    (match t1, t2 with
     | Standard(Character),Standard(Character) -> Standard(Character)
     | Standard(String(_)),Standard(String(_)) -> Standard(String(TString))
     | Standard(Character),_ -> incompatible_types (string_of_type t1) (string_of_type t2) e2
     | Standard(String(_)),_ -> incompatible_types (string_of_type t1) (string_of_type t2) e2
     | _ -> invalid_operand (Binop (Lbinop op)) "literal" e1)

let resolve_unop e1 op =
  let t1 = type_of_expr e1 in
  match op with
    Nunop op ->
    if t1 <> Standard(Integer) && t1 <> Standard(Real) then
      invalid_operand (Unop (Nunop op)) "numerical" e1
    else
      t1

let rec expression env = function
  | PEconst (c, pos) -> (match c with
      | Cint _ -> Econst (c, Standard(Integer), pos)
      | Cfloat _ -> Econst (c, Standard(Real), pos)
      | Cchar _ -> Econst (c, Standard(Character), pos)
      | Cstring _ -> Econst (c, Standard(String(TString)), pos)
      | Carray a -> Econst (c, Array a, pos))
  | PEvar (v, pos) -> (try let s,t = Env.find v env.vars in Evar (s, t, pos) with Not_found -> unbound_var v)
  | PEbinop (o, (pe1, pos1), (pe2, pos2)) ->
    let e1 = expression env pe1 in
    let e2 = expression env pe2 in
    let t = resolve_binop e1 e2 o in
    Ebinop(o, (e1, pos1), (e2, pos2), t)
  | PEunop (o, (pe, pos)) ->
    let e = expression env pe in
    let t = resolve_unop e o in
    Eunop(o, (e, pos), t)

let rec bool_expr env = function
    PBunop (op, e) -> Bunop (op, bool_expr env e)
  | PBbinop (op, e0, e1) -> Bbinop (op, bool_expr env e0, bool_expr env e1)
  | PBcmp (o, e0, e1) -> Bcmp (o, expression env e0, expression env e1)

let formal env (x,br,t) e =
  let te = expression env e in
  let tt = type_of_expr te in
  if tt <> t then
    error ("Expected parameter of type " ^ string_of_type t ^ " but got " ^ string_of_type tt)
  else if br then match te with
    | Evar (x, _, pos) -> Eaddr (x, tt, pos)
    | _ -> reference_expected x
  else
    te

let rec stmt env = function
  | PSassign (x, e) ->
    let x,t = try Env.find x env.vars with Not_found -> unbound_var x in
    let e1 = expression env e in
    let t2 = type_of_expr e1 in
    if t <> t2 then
      incompatible_types (string_of_type t) (string_of_type t2) e1
    else
      Sassign (x, e1)
  | PSif (b, s1, s2) -> Sif (bool_expr env b, stmt env s1, stmt env s2)
  | PSwhile (b, s1) -> Swhile (bool_expr env b, stmt env s1)
  | PSblock sl -> Sblock (List.map (stmt env) sl)
  | PScall ("writeln", [e]) -> Swriteln (expression env e)
  | PScall ("write", [e]) -> Swrite (expression env e)
  | PScall ("readint", [e]) ->
    Sread
      (let e1 = expression env e in
       match e1 with
         Evar (i, a, b) -> Evar ({i with by_reference=true; }, a, b)
       | _ -> invalid_argument "Argument must be a single variable." e1)
  | PScall ("readreal", [e]) ->
    Sread
      (let e1 = expression env e in
       match e1 with
         Evar (i, a, b) -> Evar ({i with by_reference=true; }, a, b)
       | _ -> invalid_argument "Argument must be a single variable." e1)
  | PScall ("readchar", [e]) ->
    Sread
      (let e1 = expression env e in
       match e1 with
         Evar (i, a, b) -> Evar ({i with by_reference=true; }, a, b)
       | _ -> invalid_argument "Argument must be a single variable." e1)
  | PScall (p, el) ->
    let p,fl = try Env.find p env.procs with Not_found -> unbound_proc p in
    let a = List.length fl in
    if a <> List.length el then bad_arity p a;
    Scall (p, List.map2 (formal env) fl el)

let add_var x t lvl ofs br env =
  if !debug then Format.eprintf "add_var %s lvl=%d ofs=%d@." x lvl ofs;
  let id = { ident = x; level = lvl; offset = ofs; by_reference = br } in
  { env with vars = Env.add x (id,t) env.vars }

(* lofs = offset pour les variables locales *)

let rec decl lvl lofs env = function
  | PVar sl ->
    let add_var env (x,t) = lofs := !lofs - 8; add_var x t lvl !lofs false env in
    Var sl, List.fold_left add_var env sl
  | PProcedure p ->
    let id = { proc_name = unique p.pname; proc_level = lvl } in
    let lvl = lvl + 1 in
    let env =
      { env with procs = Env.add p.pname (id, p.pformals) env.procs }
    in
    let ofs = ref (24 + 8 * List.length p.pformals) in
    let add_formal env (x,br,t) = ofs := !ofs - 8; add_var x t lvl !ofs br env in
    let envp = List.fold_left add_formal env p.pformals in
    let decls, envp = decls lvl envp p.plocals in
    let p = { pident = id; formals = p.pformals;
              locals = decls; body = stmt envp p.pbody }
    in
    Procedure p, env

and decls lvl env dl =
  let lofs = ref 0 in
  let env, dl =
    List.fold_left
      (fun (env,dl) d -> let d, env = decl lvl lofs env d in env, d :: dl)
      (env, []) dl
  in
  List.rev dl, env

let initial_env = { vars = Env.empty; procs = Env.empty }

let prog p =
  let decls, env = decls 0 initial_env p.plocals in
  { pident = { proc_name = p.pname; proc_level = 0 };
    formals = []; locals = decls; body = stmt env p.pbody }
