open Ast

let debug = ref false

exception Error of string

let error s = raise (Error s)
let unbound_var x = error ("unbound variable " ^ x)
let unbound_proc x = error ("unbound procedure " ^ x)
let reference_expected x = error ("expecting a left value for formal " ^ x)
let bad_arity p a =
  error ("bad arity: procedure p expects " ^ string_of_int a ^ " arguments")

let unique =
  let r = ref 0 in fun s -> incr r; s ^ "__" ^ string_of_int !r

module Env = Map.Make(String)

type env =
  { vars : ident Env.t;
    procs : (pident * formals) Env.t }

let rec int_expr env = function
  | PEconst n -> Econst n
  | PEvar x -> Evar (try Env.find x env.vars with Not_found -> unbound_var x)
  | PEbinop (o, e1, e2) -> Ebinop (o, int_expr env e1, int_expr env e2)

let formal env (x,br) e =
  let te = int_expr env e in
  if br then match te with
    | Evar x -> Eaddr x
    | _ -> reference_expected x
  else
    te

let rec bool_expr env = function
  | PBcmp (c, e1, e2) -> Bcmp (c, int_expr env e1, int_expr env e2)
  | PBand (e1, e2) -> Band (bool_expr env e1, bool_expr env e2)
  | PBor (e1, e2) -> Bor (bool_expr env e1, bool_expr env e2)
  | PBnot e1 -> Bnot (bool_expr env e1)

let rec stmt env = function
  | PSassign (x, e) ->
      let x = try Env.find x env.vars with Not_found -> unbound_var x in
      Sassign (x, int_expr env e)
  | PSif (b, s1, s2) ->
      Sif (bool_expr env b, stmt env s1, stmt env s2)
  | PSwhile (b, s1) ->
      Swhile (bool_expr env b, stmt env s1)
  | PSblock sl ->
      Sblock (List.map (stmt env) sl)
  | PScall ("writeln", [e]) ->
      Swriteln (int_expr env e)
  | PScall (p, el) ->
      let p,fl = try Env.find p env.procs with Not_found -> unbound_proc p in
      let a = List.length fl in
      if a <> List.length el then bad_arity p a;
      Scall (p, List.map2 (formal env) fl el)

let add_var x lvl ofs br env =
  if !debug then Format.eprintf "add_var %s lvl=%d ofs=%d@." x lvl ofs;
  let id = { ident = x; level = lvl; offset = ofs; by_reference = br } in
  { env with vars = Env.add x id env.vars }

(* lofs = offset pour les variables locales *)

let rec decl lvl lofs env = function
  | PVar sl ->
      let add_var env x = lofs := !lofs - 8; add_var x lvl !lofs false env in
      Var sl, List.fold_left add_var env sl
  | PProcedure p ->
      let id = { proc_name = unique p.pname; proc_level = lvl } in
      let lvl = lvl + 1 in
      let env =
	{ env with procs = Env.add p.pname (id, p.pformals) env.procs }
      in
      let ofs = ref (24 + 8 * List.length p.pformals) in
      let add_formal env (x,br) = ofs := !ofs - 8; add_var x lvl !ofs br env in
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
