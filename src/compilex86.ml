open X86_64
open Ast

let rec iter n code = if n = 0 then nop else code ++ iter (n - 1) code

let symb x = "S__" ^ x

let fresh_label =
  let r = ref 0 in
  fun () -> incr r; "Label_" ^ string_of_int !r

let rec int_expr lvl = function
  | Econst (n, _) ->
    (match n with
     | Cint x -> movq (imm x) (reg rdi)
     | _ -> assert false)
  | Evar ({ level = l; offset = ofs; by_reference = br }, _) ->
    movq (reg rbp) (reg rsi) ++
    iter (lvl - l) (movq (ind ~ofs:16 rsi) (reg rsi)) ++
    movq (ind ~ofs rsi) (reg rdi) ++
    if br then movq (ind rdi) (reg rdi) else nop
  | Eaddr ({ level = l; offset = ofs; by_reference = br }, _) ->
    movq (reg rbp) (reg rdi) ++
    iter (lvl - l) (movq (ind ~ofs:16 rdi) (reg rdi)) ++
    addq (imm ofs) (reg rdi) ++
    if br then movq (ind rdi) (reg rdi) else nop
  | Eunop (op, (e, _)) ->
    int_expr lvl e (* TODO implement*)
  | Ebinop (op, (e0, _), (e1, _)) ->
    int_expr lvl e1 ++ pushq (reg rdi) ++ int_expr lvl e0 ++
    (match op with
     | Nbinop o ->
       (match o with
        | Nadd -> popq rsi ++ addq (reg rsi) (reg rdi)
        | Nsub -> popq rsi ++ subq (reg rsi) (reg rdi)
        | Nmul -> popq rsi ++ imulq (reg rsi) (reg rdi)
        | Ndiv -> movq (reg rdi) (reg rax) ++ cqto ++ popq rdi ++
                  idivq (reg rdi) ++ movq (reg rax) (reg rdi))
     | Ibinop o ->
       (match o with
        | Ipow -> cqto (* TODO remove cqto and implement *))
     | _ -> assert false)

let rec float_expr lvl = function Econst (n, _) -> label ""
(* TODO remove previous code and implement:
   function
   | Fconst n ->
   movq (imm n) (reg rdi)
   | Fvar { level = l; offset = ofs; by_reference = br } ->
   movq (reg rbp) (reg rsi) ++
   iter (lvl - l) (movq (ind ~ofs:16 rsi) (reg rsi)) ++
   movq (ind ~ofs rsi) (reg rdi) ++
   if br then movq (ind rdi) (reg rdi) else nop
   | Faddr { level = l; offset = ofs; by_reference = br } ->
   movq (reg rbp) (reg rdi) ++
   iter (lvl - l) (movq (ind ~ofs:16 rdi) (reg rdi)) ++
   addq (imm ofs) (reg rdi) ++
   if br then movq (ind rdi) (reg rdi) else nop
   | Funop (op, e) ->
   float_expr lvl e (* TODO implement*)
   | Fbinop (op, e0, e1) ->
   float_expr lvl e1 ++ pushq (reg rdi) ++ float_expr lvl e0 ++
   (match op with
   | Fadd -> popq rsi ++ addq (reg rsi) (reg rdi)
   | Fsub -> popq rsi ++ subq (reg rsi) (reg rdi)
   | Fmul -> popq rsi ++ imulq (reg rsi) (reg rdi)
   | Fdiv -> movq (reg rdi) (reg rax) ++ cqto ++ popq rdi ++
             idivq (reg rdi) ++ movq (reg rax) (reg rdi)) *)

let rec char_expr lvl = function Econst (n, _) -> label ""
(* TODO remove previous code and implement *)

let rec string_expr lvl = function Econst (n, _) -> label ""
(* TODO remove previous code and implement *)

let cmp_op = function
  | Beq -> sete | Bneq -> setne
  | Blt -> setl | Ble -> setle | Bgt -> setg | Bge -> setge

(* compile la valeur de l'expression dans %rdi *)
let rec bool_expr lvl = function Econst (n, _) -> label ""
  (* | Econst c -> label ""
  | Evar v -> label ""
  | Eaddr v -> label ""
  | Eunop (op, e) ->
    (match op with
     | Bunop o ->
       (match o with
          Bnot ->
          bool_expr lvl e ++ testq (reg rdi) (reg rdi) ++
                 sete (reg dil) ++ movzbq (reg dil) rdi))
  | Ebinop (op, e0, e1) ->
    (match op with
     | Bbinop o ->
       (match o with
        | Band ->
          let lab = fresh_label () in
          bool_expr lvl e1 ++
          testq (reg rdi) (reg rdi) ++ je lab ++ bool_expr lvl e2 ++ label lab
        | Bor ->
          let lab = fresh_label () in
          bool_expr lvl e1 ++
          testq (reg rdi) (reg rdi) ++ jne lab ++ bool_expr lvl e2 ++ label lab)
     | Cmpbinop o ->
       (match o with
         int_expr lvl e0 ++ pushq (reg rdi) ++
         int_expr lvl e1 ++ popq rsi ++ cmpq (reg rdi) (reg rsi) ++
         (cmp_op op) (reg dil) ++ movzbq (reg dil) rdi
       )) *)

let expression lvl e =
  match e.etype with
  | Standard(Integer) -> int_expr lvl e.eexpr
  | Standard(Real) -> float_expr lvl e.eexpr
  | Standard(Character) -> char_expr lvl e.eexpr
  | Standard(String(_)) -> string_expr lvl e.eexpr
  | Standard(Boolean) -> bool_expr lvl e.eexpr
  | _ -> (* TODO array *) label ""

let popn n = addq (imm (8 * n)) (reg rsp)

let rec stmt lvl = function
  | Swriteln e ->
    let e1 = expression lvl e in
    (match e.etype with
     | Standard(Integer) -> e1 ++ call "print_int"
     | Standard(Real) -> e1 ++ call "print_float"
     | Standard(Character) -> e1 ++ call "print_char"
     | Standard(String(_)) -> e1 ++ call "print_string"
     | Standard(Boolean) -> e1 ++ call "print_bool"
     | _ -> (* TODO array *) label "")
  | Scall ({proc_name = id; proc_level = l}, el) ->
    List.fold_left
      (fun code s -> code ++ expression lvl s ++ pushq (reg rdi)) nop el ++
    movq (reg rbp) (reg rsi) ++
    iter (lvl - l) (movq (ind ~ofs:16 rsi) (reg rsi)) ++ pushq (reg rsi) ++
    call (symb id) ++ popn (1 + List.length el)
  | Sassign ({ level = l; offset = ofs; by_reference = br }, e) ->
    let e1 = expression lvl e in
    (match e.etype with
     | Standard(Integer) ->
       e1 ++ movq (reg rbp) (reg rsi) ++
       iter (lvl - l) (movq (ind ~ofs:16 rsi) (reg rsi)) ++
       (if br then movq (ind ~ofs rsi) (reg rsi)
        else addq (imm ofs) (reg rsi)) ++
       movq (reg rdi) (ind rsi)
     | Standard(Real) ->
       e1 (* TODO implement *)
     | Standard(Character) ->
       e1 (* TODO implement *)
     | Standard(String(_)) ->
       e1 (* TODO implement *)
     | Standard(Boolean) ->
       e1 (* TODO implement *)
     | _ -> (* TODO array *) e1 (* TODO implement *))
  | Sif (e, s1, s2) ->
    let l_else = fresh_label () in
    let l_done = fresh_label () in
    expression lvl e ++
    testq (reg rdi) (reg rdi) ++ jz l_else ++
    stmt lvl s1 ++ jmp l_done ++ label l_else ++ stmt lvl s2 ++ label l_done
  | Swhile (e, s) ->
    let l_test = fresh_label () in
    let l_done = fresh_label () in
    label l_test ++ expression lvl e ++
    testq (reg rdi) (reg rdi) ++ jz l_done ++
    stmt lvl s ++ jmp l_test ++ label l_done
  | Sblock sl ->
    List.fold_left (fun code s -> code ++ stmt lvl s) nop sl

let frame_size dl =
  8 * List.fold_left (fun s d -> match d with
      | Var vl -> s + List.length vl
      | Procedure _ -> s) 0 dl

let procedure p =
  Format.eprintf "procedure %s at level %d@."
    p.pident.proc_name p.pident.proc_level;
  let fs = 8 + frame_size p.locals in
  label (symb p.pident.proc_name) ++
  subq (imm fs) (reg rsp) ++
  movq (reg rbp) (ind ~ofs:(fs - 8) rsp) ++
  leaq (ind ~ofs:(fs - 8) rsp) rbp ++
  stmt (p.pident.proc_level + 1) p.body ++
  movq (reg rbp) (reg rsp) ++ popq rbp ++ ret

let rec decl = function
  | Var _ -> nop
  | Procedure p -> procedure p ++ decls p.locals

and decls dl = List.fold_left (fun code d -> code ++ decl d) nop dl

let prog p =
  let fs = 8 + frame_size p.locals in
  let code_main = stmt 0 p.body in
  let code_procs = decls p.locals in
  { text =
      glabel "main" ++
      subq (imm fs) (reg rsp) ++
      leaq (ind ~ofs:(fs - 8) rsp) rbp ++
      code_main ++
      addq (imm fs) (reg rsp) ++
      movq (imm 0) (reg rax) ++
      ret ++
      label "print_int" ++
      movq (reg rdi) (reg rsi) ++
      movq (ilab ".Sprint_int") (reg rdi) ++
      movq (imm 0) (reg rax) ++
      call "printf" ++
      ret ++
      (* TODO implement other prints *)
      code_procs;
    data =
      label ".Sprint_int" ++ string "%d\n"
  }
