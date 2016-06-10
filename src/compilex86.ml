open X86_64
open Ast

let rec iter n code = if n = 0 then nop else code ++ iter (n - 1) code

let symb x = "S__" ^ x

let fresh_label =
  let r = ref 0 in
  fun () -> incr r; "Label_" ^ string_of_int !r

let rec int_expr lvl = function
  | Iconst n ->
    movq (imm n) (reg rdi)
  | Ivar { level = l; offset = ofs; by_reference = br } ->
    movq (reg rbp) (reg rsi) ++
    iter (lvl - l) (movq (ind ~ofs:16 rsi) (reg rsi)) ++
    movq (ind ~ofs rsi) (reg rdi) ++
    if br then movq (ind rdi) (reg rdi) else nop
  | Iaddr { level = l; offset = ofs; by_reference = br } ->
    movq (reg rbp) (reg rdi) ++
    iter (lvl - l) (movq (ind ~ofs:16 rdi) (reg rdi)) ++
    addq (imm ofs) (reg rdi) ++
    if br then movq (ind rdi) (reg rdi) else nop
  | Iunop (op, e) ->
    int_expr lvl e (* TODO implement*)
  | Ibinop (op, e0, e1) ->
    int_expr lvl e1 ++ pushq (reg rdi) ++ int_expr lvl e0 ++
    (match op with
     | Iadd -> popq rsi ++ addq (reg rsi) (reg rdi)
     | Isub -> popq rsi ++ subq (reg rsi) (reg rdi)
     | Imul -> popq rsi ++ imulq (reg rsi) (reg rdi)
     | Idiv -> movq (reg rdi) (reg rax) ++ cqto ++ popq rdi ++
               idivq (reg rdi) ++ movq (reg rax) (reg rdi)
     | Ipow -> cqto (* TODO remove cqto and implement *))

let rec float_expr lvl = function Fconst n -> label ""
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

let rec char_expr lvl = function Cconst n -> label ""
(* TODO remove previous code and implement *)

let rec string_expr lvl = function Sconst n -> label ""
(* TODO remove previous code and implement *)

let cmp_op = function
  | Beq -> sete | Bneq -> setne
  | Blt -> setl | Ble -> setle | Bgt -> setg | Bge -> setge

(* compile la valeur de l'expression dans %rdi *)
let rec bool_expr lvl = function
  | Bbinop (op, e1, e2) ->
    (match op with
     | Band ->
       let lab = fresh_label () in
       bool_expr lvl e1 ++
       testq (reg rdi) (reg rdi) ++ je lab ++ bool_expr lvl e2 ++ label lab
     | Bor ->
       let lab = fresh_label () in
       bool_expr lvl e1 ++
       testq (reg rdi) (reg rdi) ++ jne lab ++ bool_expr lvl e2 ++ label lab)
  | Bunop (op, e1) ->
    bool_expr lvl e1 ++ testq (reg rdi) (reg rdi) ++
    sete (reg dil) ++ movzbq (reg dil) rdi
  | Bintcmp (op, e0, e1) ->
    int_expr lvl e0 ++ pushq (reg rdi) ++
    int_expr lvl e1 ++ popq rsi ++ cmpq (reg rdi) (reg rsi) ++
    (cmp_op op) (reg dil) ++ movzbq (reg dil) rdi
  | Bfloatcmp (op, e0, e1) ->
    movzbq (reg dil) rdi (* TODO remove this line and implement *)
  | Bcharcmp (op, e0, e1) ->
    movzbq (reg dil) rdi (* TODO remove this line and implement *)

let popn n = addq (imm (8 * n)) (reg rsp)

let rec stmt lvl = function
  | Swriteln e ->
    (match e with
     | Eint e1 -> int_expr lvl e1 ++ call "print_int"
     | Efloat e1 -> float_expr lvl e1 ++ call "print_float"
     | Echar e1 -> char_expr lvl e1 ++ call "print_char"
     | Estring e1 -> string_expr lvl e1 ++ call "print_string")
  | Scall ({proc_name = id; proc_level = l}, el) ->
    List.fold_left
      (fun code s -> code ++ int_expr lvl s ++ pushq (reg rdi)) nop el ++
    movq (reg rbp) (reg rsi) ++
    iter (lvl - l) (movq (ind ~ofs:16 rsi) (reg rsi)) ++ pushq (reg rsi) ++
    call (symb id) ++ popn (1 + List.length el)
  | Sassign ({ level = l; offset = ofs; by_reference = br }, e) ->
    (match e with
     | Eint e1 ->
       int_expr lvl e1 ++
       movq (reg rbp) (reg rsi) ++
       iter (lvl - l) (movq (ind ~ofs:16 rsi) (reg rsi)) ++
       (if br then movq (ind ~ofs rsi) (reg rsi)
        else addq (imm ofs) (reg rsi)) ++
       movq (reg rdi) (ind rsi)
     | Efloat e1 ->
       float_expr lvl e1 (* TODO implement *)
     | Echar e1 ->
       char_expr lvl e1 (* TODO implement *)
     | Estring e1 ->
       string_expr lvl e1 (* TODO implement *))
  | Sif (e, s1, s2) ->
    let l_else = fresh_label () in
    let l_done = fresh_label () in
    bool_expr lvl e ++
    testq (reg rdi) (reg rdi) ++ jz l_else ++
    stmt lvl s1 ++ jmp l_done ++ label l_else ++ stmt lvl s2 ++ label l_done
  | Swhile (e, s) ->
    let l_test = fresh_label () in
    let l_done = fresh_label () in
    label l_test ++ bool_expr lvl e ++
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
      code_procs;
    data =
      label ".Sprint_int" ++ string "%d\n"
  }
