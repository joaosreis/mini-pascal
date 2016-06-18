open X86_64
open Ast

let floats = ref (ins "")
let n_floats = ref 1

let float_to_hexrepr f =
  let double_norm = Int64.shift_left 1L 52 in
  let double_mask = Int64.pred double_norm in
  let i = Int64.bits_of_float f in
  let i = Int64.logand Int64.max_int i in
  let exp = Int64.shift_right_logical i 52 in
  let man = Int64.logand i double_mask in
  let s = Format.sprintf "%03Lx%013Lx" exp man in
  let s1, s2 = "0x" ^ (String.sub s 0 8), "0x" ^ (String.sub s 8 8) in
  int_of_string s1, int_of_string s2

let add_float f =
  let h, l = float_to_hexrepr f in
  floats := !floats ++ label (".LC" ^ string_of_int !n_floats) ++
            long (immi l) ++ long (immi h);
  n_floats := !n_floats + 1;
  !n_floats - 1

let rec iter n code = if n = 0 then nop else code ++ iter (n - 1) code

let symb x = "S__" ^ x

let fresh_label =
  let r = ref 0 in
  fun () -> incr r; "Label_" ^ string_of_int !r

let cmp_op = function
  | Beq -> sete | Bneq -> setne
  | Blt -> setl | Ble -> setle | Bgt -> setg | Bge -> setge

let int_pow =
  label "int_pow" ++
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  movq (imm 1) (reg rax) ++ (* rax -> y = 1 *)
  movq (reg rdi) (reg rbx) ++ (* rbx -> p = x *)
  label "ipow_loop" ++
  movq (imm 1) (reg rcx) ++
  testq (reg rsi) (reg rcx) ++
  jz "j1" ++
  imulq (reg rbx) (reg rax) ++
  label "j1" ++
  shrq (imm 1) (reg rsi) ++
  jnz "j2" ++
  movq (reg rax) (reg rdi) ++
  leave ++ ret ++
  label "j2" ++
  imulq (reg rbx) (reg rbx) ++
  jmp "ipow_loop"

let rec expression lvl e =
  let rec int_expr lvl = function
      Econst (n, _, _) ->
      (match n with
       | Cint x -> movq (imm x) (reg rdi)
       | _ -> assert false)
    | Evar ({ level = l; offset = ofs; by_reference = br }, _, _) ->
      movq (reg rbp) (reg rsi) ++
      iter (lvl - l) (movq (ind ~ofs:16 rsi) (reg rsi)) ++
      movq (ind ~ofs rsi) (reg rdi) ++
      if br then movq (ind rdi) (reg rdi) else nop
    | Eaddr ({ level = l; offset = ofs; by_reference = br }, _, _) ->
      movq (reg rbp) (reg rdi) ++
      iter (lvl - l) (movq (ind ~ofs:16 rdi) (reg rdi)) ++
      addq (imm ofs) (reg rdi) ++
      if br then movq (ind rdi) (reg rdi) else nop
    | Eunop (op, (e, _), _) ->
      expression lvl e ++ negq (reg rdi)
    | Ebinop (op, (e0, _), (e1, _), _) ->
      expression lvl e1 ++ pushq (reg rdi) ++ expression lvl e0 ++
      (match op with
       | Nbinop o ->
         (match o with
          | Nadd -> popq rsi ++ addq (reg rsi) (reg rdi)
          | Nsub -> popq rsi ++ subq (reg rsi) (reg rdi)
          | Nmul -> popq rsi ++ imulq (reg rsi) (reg rdi)
          | Ndiv -> movq (reg rdi) (reg rax) ++ cqto ++ popq rdi ++
                    idivq (reg rdi) ++ movq (reg rax) (reg rdi)
          | Npow -> popq rsi ++ (* rdi -> x; rsi -> n *)
                    call "int_pow")
       | _ -> assert false)

  in let rec float_expr lvl = function
        Econst (c, _, _) ->
        (match c with
           Cfloat x -> let n = add_float x in movsd (inds (".LC" ^ string_of_int n) rip) (reg xmm0)
         | _ -> assert false)
      | Evar ({ level = l; offset = ofs; by_reference = br }, _, _) ->
        movq (reg rbp) (reg rsi) ++
        iter (lvl - l) (movq (ind ~ofs:16 rsi) (reg rsi)) ++
        movq (ind ~ofs rsi) (reg rdi) ++
        if br then
          movq (ind rdi) (reg rdi) ++ movq (reg rdi) (reg xmm0)
        else
          movq (reg rdi) (reg xmm0)
      | Eaddr ({ level = l; offset = ofs; by_reference = br }, _, _) ->
        movq (reg rbp) (reg rdi) ++
        iter (lvl - l) (movq (ind ~ofs:16 rdi) (reg rdi)) ++
        addq (imm ofs) (reg rdi) ++
        if br then
          movq (ind rdi) (reg rdi) ++ movq (reg rdi) (reg xmm0)
        else
          movq (reg rdi) (reg xmm0)
      | Eunop (op, (e, _), _) ->
        expression lvl e ++
        movq (imm 8000000000000000) (reg rsi) ++ cvtsi2sdq (reg rsi) (reg xmm1) ++
        xorpd (reg xmm1) (reg xmm0) (* FIXME not working *)
      | Ebinop (op, (e0, _), (e1, _), _) ->
        expression lvl e1 ++
        movq (reg xmm0) (reg rdi) ++ pushq (reg rdi) ++
        expression lvl e0 ++
        (match op with
         | Nbinop o ->
           (match o with
            | Nadd -> popq rsi ++ movq (reg rsi) (reg xmm1) ++ addsd (reg xmm1) (reg xmm0)
            | Nsub -> popq rsi ++ movq (reg rsi) (reg xmm1) ++ subsd (reg xmm1) (reg xmm0)
            | Nmul -> popq rsi ++ movq (reg rsi) (reg xmm1) ++ mulsd (reg xmm1) (reg xmm0)
            | Ndiv -> popq rsi ++ movq (reg rsi) (reg xmm1) ++ divsd (reg xmm1) (reg xmm0)
            | Npow -> popq rsi ++ movq (reg rsi) (reg xmm1) ++ call "pow")
         | _ -> assert false)

  in let rec char_expr lvl = function
        Econst (n, _, _) ->
        (match n with
         | Cchar x -> movq (imm (Char.code x)) (reg rsi)
         | _ -> assert false)
      | Evar ({ level = l; offset = ofs; by_reference = br }, _, _) ->
        movq (reg rbp) (reg rsi) ++
        iter (lvl - l) (movq (ind ~ofs:16 rsi) (reg rsi)) ++
        movq (ind ~ofs rsi) (reg rdi) ++
        if br then movq (ind rdi) (reg rdi) else nop
      | Eaddr ({ level = l; offset = ofs; by_reference = br }, _, _) ->
        movq (reg rbp) (reg rdi) ++
        iter (lvl - l) (movq (ind ~ofs:16 rdi) (reg rdi)) ++
        addq (imm ofs) (reg rdi) ++
        if br then movq (ind rdi) (reg rdi) else nop
      | Ebinop (op, (e0, _), (e1, _), _) ->
        expression lvl e1 ++ pushq (reg rdi) ++ expression lvl e0 ++
        (match op with
         | Lbinop o ->
           (match o with
            | Lconcat -> (* TODO implement *) label "")
         | _ -> assert false)
      | _ -> assert false

  in let rec string_expr lvl = function Econst (n, _, _) -> label ""

  in match (type_of_expr e) with
  | Standard(Integer) -> int_expr lvl e
  | Standard(Real) -> float_expr lvl e
  | Standard(Character) -> char_expr lvl e
  | Standard(String(_)) -> string_expr lvl e
  | _ -> (* TODO array *) label ""

let rec bool_expr lvl = function
    Bunop (op, e) ->
    (match op with
       Bnot ->
       bool_expr lvl e ++ testq (reg rdi) (reg rdi) ++
       sete (reg dil) ++ movzbq (reg dil) rdi)
  | Bbinop (op, e0, e1) ->
    (match op with
     | Band ->
       let lab = fresh_label () in
       bool_expr lvl e0 ++
       testq (reg rdi) (reg rdi) ++ je lab ++ bool_expr lvl e1 ++ label lab
     | Bor ->
       let lab = fresh_label () in
       bool_expr lvl e0 ++
       testq (reg rdi) (reg rdi) ++ jne lab ++ bool_expr lvl e1 ++ label lab)
  | Bcmp (o, e0, e1) ->
    expression lvl e0 ++ pushq (reg rdi) ++
    expression lvl e1 ++ popq rsi ++ cmpq (reg rdi) (reg rsi) ++
    (cmp_op o) (reg dil) ++
    movzbq (reg dil) rdi

let popn n = addq (imm (8 * n)) (reg rsp)

let rec stmt lvl = function
  | Swriteln e ->
    let e1 = expression lvl e in
    (match (type_of_expr e) with
     | Standard(Integer) -> e1 ++ call "print_int"
     | Standard(Real) -> e1 ++ call "print_float"
     | Standard(Character) -> e1 ++ call "print_char"
     | Standard(String(_)) -> e1 ++ call "print_string"
     | _ -> (* TODO array *) label "")
  | Scall ({proc_name = id; proc_level = l}, el) ->
    List.fold_left
      (fun code s -> code ++ expression lvl s ++ pushq (reg rdi)) nop el ++
    movq (reg rbp) (reg rsi) ++
    iter (lvl - l) (movq (ind ~ofs:16 rsi) (reg rsi)) ++ pushq (reg rsi) ++
    call (symb id) ++ popn (1 + List.length el)
  | Sassign ({ level = l; offset = ofs; by_reference = br }, e) ->
    let e1 = expression lvl e in
    (match (type_of_expr e) with
     | Standard(Integer) ->
       e1 ++ movq (reg rbp) (reg rsi) ++
       iter (lvl - l) (movq (ind ~ofs:16 rsi) (reg rsi)) ++
       (if br then movq (ind ~ofs rsi) (reg rsi)
        else addq (imm ofs) (reg rsi)) ++
       movq (reg rdi) (ind rsi)
     | Standard(Real) ->
       e1 ++ movq (reg rbp) (reg rsi) ++
       iter (lvl - l) (movq (ind ~ofs:16 rsi) (reg rsi)) ++
       (if br then movq (ind ~ofs rsi) (reg rsi)
        else addq (imm ofs) (reg rsi)) ++
       movq (reg xmm0) (ind rsi)
     | Standard(Character) ->
       e1 (* TODO implement *)
     | Standard(String(_)) ->
       e1 (* TODO implement *)
     | _ -> (* TODO array *) e1 (* TODO implement *))
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
  let n = 8 * List.fold_left (fun s d -> match d with
      | Var vl -> s + List.length vl
      | Procedure _ -> s) 0 dl
  in if n = 0 then 16
  else if n mod 16 <> 0 then n + 8
  else n

let procedure p =
  Format.eprintf "procedure %s at level %d@."
    p.pident.proc_name p.pident.proc_level;
  let fs = frame_size p.locals in
  label (symb p.pident.proc_name) ++
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  subq (imm fs) (reg rsp) ++
  stmt (p.pident.proc_level + 1) p.body ++
  leave ++ ret

let rec decl = function
  | Var _ -> nop
  | Procedure p -> procedure p ++ decls p.locals

and decls dl = List.fold_left (fun code d -> code ++ decl d) nop dl

let print_int_asm =
  label "print_int" ++
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  movq (reg rdi) (reg rsi) ++
  movq (ilab ".Sprint_int") (reg rdi) ++
  movq (imm 0) (reg rax) ++
  call "printf" ++
  leave ++
  ret

let print_float_asm =
  label "print_float" ++
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  movq (ilab ".Sprint_float") (reg rdi) ++
  movq (imm 1) (reg rax) ++
  call "printf" ++
  leave ++
  ret

let print_char_asm =
    label "print_char" ++
    pushq (reg rbp) ++
    movq (reg rsp) (reg rbp) ++
    movq (reg rdi) (reg rsi) ++
    movq (ilab ".Sprint_char") (reg rdi) ++
    movq (imm 0) (reg rax) ++
    call "printf" ++
    leave ++
    ret

let prog p =
  let fs = frame_size p.locals in
  let code_main = stmt 0 p.body in
  let code_procs = decls p.locals in
  { text =
      glabel "main" ++
      pushq (reg rbp) ++
      movq (reg rsp) (reg rbp) ++
      subq (imm fs) (reg rsp) ++
      code_main ++
      movq (imm 0) (reg rax) ++
      leave ++
      ret ++
      print_int_asm ++
      print_float_asm ++
      print_char_asm ++
      int_pow ++
      !floats ++
      code_procs;
    data =
      label ".Sprint_int" ++ string "%d\n" ++
      label ".Sprint_float" ++ string "%f\n" ++
      label ".Sprint_char" ++ string "%c\n"
  }
