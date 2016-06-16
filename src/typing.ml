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
  error ("operator " ^ string_of_operator op ^ " expects operand of type " ^ t ^ "\n" ^ (string_of_expr_pos e.eexpr))
let incompatible_types t1 t2 e =
  error ("expected expression of type " ^ t1 ^ " but expression is of type " ^ t2 ^ "\n" ^ (string_of_expr_pos e))

let unique =
  let r = ref 0 in fun s -> incr r; s ^ "__" ^ string_of_int !r

module Env = Map.Make(String)

type env =
  { vars : (ident * ttype) Env.t;
    procs : (pident * formals) Env.t }

let resolve_binop e1 e2 = function
  | Nbinop op ->
    if e1.etype <> Standard(Integer) && e1.etype <> Standard(Real) then
      invalid_operand (Binop (Nbinop op)) "numerical" e1
    else if e2.etype <> Standard(Integer) && e2.etype <> Standard(Real) then
      invalid_operand (Binop (Nbinop op)) "numerical" e2
    else if e1.etype <> e2.etype then
      incompatible_types (string_of_type e1.etype) (string_of_type e2.etype) e2.eexpr
    else
      e1.etype
  | Ibinop op ->
    if e1.etype <> Standard(Integer) then
      invalid_operand (Binop (Ibinop op)) "integer" e1
    else if e2.etype <> Standard(Integer) then
      invalid_operand (Binop (Ibinop op)) "integer" e2
    else
      Standard(Integer)
  | Lbinop op ->
    (match e1.etype, e2.etype with
     | Standard(Character),Standard(Character) -> Standard(Character)
     | Standard(String(_)),Standard(String(_)) -> Standard(String(TString))
     | Standard(Character),_ -> incompatible_types (string_of_type e1.etype) (string_of_type e2.etype) e2.eexpr
     | Standard(String(_)),_ -> incompatible_types (string_of_type e1.etype) (string_of_type e2.etype) e2.eexpr
     | _ -> invalid_operand (Binop (Lbinop op)) "literal" e1)
  | Bbinop op ->
    if e1.etype <> Standard(Boolean) then
      invalid_operand (Binop (Bbinop op)) "bool" e1
    else if e2.etype <> Standard(Boolean) then
      invalid_operand (Binop (Bbinop op)) "bool" e2
    else
      Standard(Boolean)
  | Cmpbinop op ->
    if e1.etype <> Standard(Integer) && e1.etype <> Standard(Real) &&
       e1.etype <> Standard(Character) then
      invalid_operand (Binop (Cmpbinop op)) "numerical or character" e1
    else if e1.etype <> Standard(Integer) && e1.etype <> Standard(Real) &&
            e1.etype <> Standard(Character) then
      invalid_operand (Binop (Cmpbinop op)) "numerical or character" e2
    else if e1.etype <> e2.etype then
      incompatible_types (string_of_type e1.etype) (string_of_type e2.etype) e2.eexpr
    else
      Standard(Boolean)

let resolve_unop e1 = function
  | Nunop op ->
    if e1.etype <> Standard(Integer) && e1.etype <> Standard(Real) then
      invalid_operand (Unop (Nunop op)) "numerical" e1
    else
      e1.etype
  | Bunop op ->
  if e1.etype <> Standard(Boolean) then
    invalid_operand (Unop (Bunop op)) "bool" e1
  else
    Standard(Boolean)

let rec expression env = function
  | PEconst (c, pos) -> (match c with
      | Cint _ -> { etype=Standard(Integer); eexpr=Econst (c, pos) }
      | Cfloat _ -> { etype=Standard(Real); eexpr=Econst (c, pos) }
      | Cchar _ -> { etype=Standard(Character); eexpr=Econst (c, pos) }
      | Cstring _ -> { etype=Standard(String(TString)); eexpr=Econst (c, pos) }
      | Cbool _ -> { etype=Standard(Boolean); eexpr=Econst (c, pos) }
      | Carray a -> { etype=Array a; eexpr=Econst (c, pos); })
  | PEvar (v, pos) -> (try let s,t = Env.find v env.vars in { etype=t; eexpr=Evar (s, pos); } with Not_found -> unbound_var v)
  | PEbinop (o, (pe1, pos1), (pe2, pos2)) ->
    let e1 = expression env pe1 in
    let e2 = expression env pe2 in
    { etype=resolve_binop e1 e2 o; eexpr=Ebinop(o, (e1.eexpr, pos1), (e2.eexpr, pos2)); }
  | PEunop (o, (pe, pos)) ->
    let e = expression env pe in
    { etype=resolve_unop e o; eexpr=Eunop(o, (e.eexpr, pos)) }

let formal env (x,br,t) e =
  let te = expression env e in
  if te.etype <> t then
    error "Expected parameter of type t but got x"
  else if br then match te.eexpr with
    | Evar (x, pos) -> { etype=te.etype; eexpr=Eaddr (x, pos); }
    | _ -> reference_expected x
  else
    te

let rec stmt env = function
  | PSassign (x, e) ->
    let x,_ = try Env.find x env.vars with Not_found -> unbound_var x in
    Sassign (x, expression env e)
  | PSif (b, s1, s2) ->
    Sif (expression env b, stmt env s1, stmt env s2)
  | PSwhile (b, s1) ->
    Swhile (expression env b, stmt env s1)
  | PSblock sl ->
    Sblock (List.map (stmt env) sl)
  | PScall ("writeln", [e]) ->
    Swriteln (expression env e)
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
    let flatten l = List.flatten (List.map (fun (a,b) -> List.map (fun x -> (x,b)) a) l) in
    Var sl, List.fold_left add_var env (flatten sl)
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
