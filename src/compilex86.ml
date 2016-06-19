open X86_64
open Ast

let constants = ref nop
let n_constants = ref 1

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
  constants := !constants ++ label (".LC" ^ string_of_int !n_constants) ++
               long (immi l) ++ long (immi h);
  n_constants := !n_constants + 1;
  !n_constants - 1

let add_string s =
  constants := !constants ++ label ("message" ^ string_of_int !n_constants) ++
               inline ("\t.string \"" ^ s ^ "\"\n");
  n_constants := !n_constants + 1;
  !n_constants - 1

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
       | Ibinop o ->
         (match o with
          | Imod -> movq (reg rdi) (reg rax) ++ cqto ++ popq rdi ++
                    idivq (reg rdi) ++ movq (reg rdx) (reg rdi))
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
        negsd (reg xmm0) (* FIXME not working *)
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

  in let rec string_expr lvl = function
        Econst (c, _, _) ->
        (match c with
           Cstring x-> let n = add_string x in movq (ilab ("message" ^ string_of_int n)) (reg rdi)
         | _ -> assert false)
      | _ -> assert false

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
  | Swriteln e -> expression lvl e ++
                  (match (type_of_expr e) with
                   | Standard(Integer) -> call "print_int"
                   | Standard(Real) -> call "print_float"
                   | Standard(Character) -> call "print_char"
                   | Standard(String(_)) -> call "print_string"
                   | _ -> (* TODO array *) label "")
  | Sread e ->
    (match e with
      Evar ({ level = l; offset = ofs; by_reference = br }, _, _) ->
        movq (reg rbp) (reg rsi) ++
        iter (lvl - l) (movq (ind ~ofs:16 rsi) (reg rsi)) ++
        leaq (ind ~ofs rsi) rdi
     | _ -> assert false) ++
    (match type_of_expr e with
       Standard(Integer) -> call "read_int"
     | Standard(Real) -> call "read_float"
     | Standard(Character) -> call "read_char"
     | Standard(String(_)) -> call "read_string"
     | _ -> assert false)
  | Scall ({proc_name = id; proc_level = l}, el) ->
    List.fold_left
      (fun code s -> code ++ expression lvl s ++ match type_of_expr s with
         | Standard(Integer) -> pushq (reg rdi)
         | Standard(Real) -> movq (reg xmm0) (reg rdi) ++ pushq (reg rdi)
         | _ -> pushq (reg rdi)) nop el ++
    movq (reg rbp) (reg rsi) ++
    iter (lvl - l) (movq (ind ~ofs:16 rsi) (reg rsi)) ++ pushq (reg rsi) ++
    call (symb id) ++ popn (List.length el - 1)
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

let frame_lenght dl =
  List.fold_left (fun s d -> match d with
      | Var vl -> s + List.length vl
      | Procedure _ -> s) 0 dl

let frame_size dl =
  let n = 8 * frame_lenght dl
  in if n = 0 then 16
  else let r = n mod 16 in
    n + r

let frame_size_p dl fl =
  let n = 8 * (frame_lenght dl + List.length fl)
  in if n = 0 then 16
  else let r = n mod 16 in
    n + r

let procedure p =
  let fs = frame_size p.locals in
  label (symb p.pident.proc_name) ++
  cfi_startproc ++
  pushq (reg rbp) ++
  cfi_def_cfa_offset (immi 16) ++
  cfi_offset (immi 6) (immi (-16)) ++
  movq (reg rsp) (reg rbp) ++
  cfi_def_cfa_register (immi 6) ++
  subq (imm fs) (reg rsp) ++
  stmt (p.pident.proc_level + 1) p.body ++
  leave ++ cfi_def_cfa (immi 7) (immi 8) ++
  ret ++ cfi_endproc
let rec decl = function
  | Var _ -> nop
  | Procedure p -> procedure p ++ decls p.locals

and decls dl = List.fold_left (fun code d -> code ++ decl d) nop dl

let print_int_asm =
  label "print_int" ++
  cfi_startproc ++
  pushq (reg rbp) ++
  cfi_def_cfa_offset (immi 16) ++
  cfi_offset (immi 6) (immi (-16)) ++
  movq (reg rsp) (reg rbp) ++
  cfi_def_cfa_register (immi 6) ++
  movq (reg rdi) (reg rsi) ++
  movq (ilab ".Sprint_int") (reg rdi) ++
  movq (imm 0) (reg rax) ++
  call "printf" ++
  leave ++ cfi_def_cfa (immi 7) (immi 8) ++
  ret ++ cfi_endproc

let print_float_asm =
  label "print_float" ++
  cfi_startproc ++
  pushq (reg rbp) ++
  cfi_def_cfa_offset (immi 16) ++
  cfi_offset (immi 6) (immi (-16)) ++
  movq (reg rsp) (reg rbp) ++
  cfi_def_cfa_register (immi 6) ++
  subq (imm 16) (reg rsp) ++
  movq (ilab ".Sprint_float") (reg rdi) ++
  movq (imm 1) (reg rax) ++
  call "printf" ++
  leave ++ cfi_def_cfa (immi 7) (immi 8) ++
  ret ++ cfi_endproc

let print_char_asm =
  label "print_char" ++
  cfi_startproc ++
  pushq (reg rbp) ++
  cfi_def_cfa_offset (immi 16) ++
  cfi_offset (immi 6) (immi (-16)) ++
  movq (reg rsp) (reg rbp) ++
  cfi_def_cfa_register (immi 6) ++
  movq (reg rdi) (reg rsi) ++
  movq (ilab ".Sprint_char") (reg rdi) ++
  movq (imm 0) (reg rax) ++
  call "printf" ++
  leave ++ cfi_def_cfa (immi 7) (immi 8) ++
  ret ++ cfi_endproc

let print_string_asm =
  label "print_string" ++
  cfi_startproc ++
  pushq (reg rbp) ++
  cfi_def_cfa_offset (immi 16) ++
  cfi_offset (immi 6) (immi (-16)) ++
  movq (reg rsp) (reg rbp) ++
  cfi_def_cfa_register (immi 6) ++
  movq (reg rdi) (reg rsi) ++
  movq (ilab ".Sprint_string") (reg rdi) ++
  movq (imm 0) (reg rax) ++
  call "printf" ++
  leave ++ cfi_def_cfa (immi 7) (immi 8) ++
  ret ++ cfi_endproc

let prints =
  print_int_asm ++
  print_float_asm ++
  print_char_asm ++
  print_string_asm

let read_int_asm =
  label "read_int" ++
  cfi_startproc ++
  pushq (reg rbp) ++
  cfi_def_cfa_offset (immi 16) ++
  cfi_offset (immi 6) (immi (-16)) ++
  movq (reg rsp) (reg rbp) ++
  cfi_def_cfa_register (immi 6) ++
  subq (imm 16) (reg rsp) ++
  movq (reg rdi) (ind ~ofs:(-8) rbp) ++
  movq (ind ~ofs:(-8) rbp) (reg rax) ++
  movq (reg rax) (reg rsi) ++
  movq (ilab ".Sread_int") (reg rdi) ++
  movq (imm 0) (reg rax) ++
  call "scanf" ++
  leave ++ cfi_def_cfa (immi 7) (immi 8) ++
  ret ++ cfi_endproc

let read_float_asm =
  label "read_float" ++
  cfi_startproc ++
  pushq (reg rbp) ++
  cfi_def_cfa_offset (immi 16) ++
  cfi_offset (immi 6) (immi (-16)) ++
  movq (reg rsp) (reg rbp) ++
  cfi_def_cfa_register (immi 6) ++
  subq (imm 16) (reg rsp) ++
  movq (reg rdi) (ind ~ofs:(-8) rbp) ++
  movq (ind ~ofs:(-8) rbp) (reg rax) ++
  movq (reg rax) (reg rsi) ++
  movq (ilab ".Sread_float") (reg rdi) ++
  movq (imm 0) (reg rax) ++
  call "scanf" ++
  leave ++ cfi_def_cfa (immi 7) (immi 8) ++
  ret ++ cfi_endproc

let reads =
  read_int_asm ++
  read_float_asm

let prog p =
  let fs = frame_size p.locals in
  let code_main = stmt 0 p.body in
  let code_procs = decls p.locals in
  { text =
      glabel "main" ++
      cfi_startproc ++
      pushq (reg rbp) ++
      cfi_def_cfa_offset (immi 16) ++
      cfi_offset (immi 6) (immi (-16)) ++
      movq (reg rsp) (reg rbp) ++
      cfi_def_cfa_register (immi 6) ++
      subq (imm fs) (reg rsp) ++
      code_main ++
      movq (imm 0) (reg rax) ++
      leave ++ cfi_def_cfa (immi 7) (immi 8) ++
      ret ++ cfi_endproc ++
      prints ++
      reads ++
      int_pow ++
      !constants ++
      code_procs;
    data =
      label ".Sprint_int" ++ string "%d\n" ++
      label ".Sprint_float" ++ string "%lf\n" ++
      label ".Sprint_char" ++ string "%c\n" ++
      label ".Sprint_string" ++ string "%s\n" ++
      label ".Sread_int" ++ string "%d" ++
      label ".Sread_float" ++ string "%lf" ++
      label ".Sread_char" ++ string "%c" ++
      label ".Sread_string" ++ string "%s"
  }
