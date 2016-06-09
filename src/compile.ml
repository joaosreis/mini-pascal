
open Mips
open Ast

(*
            arg 1
            ...
            arg n
            $fp p�re
   ---------------------------
            $fp appelant
   $fp ---> $ra
            ...
            var locales
            ...
            calculs
   $sp ---> ...
*)

let pushr r = mips [Arith (Sub, SP, SP, Oimm 4); Sw (r, Areg (0, SP))]
let popr r = mips [Lw (r, Areg (0, SP)); Arith (Add, SP, SP, Oimm 4)]
let pop1 = mips [Arith (Add, SP, SP, Oimm 4)]
let popn n = mips [Arith (Add, SP, SP, Oimm (4 * n))]

let rec iter n code = if n = 0 then nop else code ++ iter (n - 1) code

let symb x = "S__" ^ x

let label =
  let r = ref 0 in
  fun () -> incr r; "Label_" ^ string_of_int !r

let rec int_expr lvl = function
  | Econst n ->
      mips [Li (A0, n)]
  | Evar { level = l; offset = ofs; by_reference = br } ->
      assert (l <= lvl);
      mips [Move (T1, FP)] ++
      iter (lvl - l) (mips [Lw (T1, Areg (8, T1))]) ++
      mips [Lw (A0, Areg (ofs, T1))] ++
      if br then mips [Lw (A0, Areg (0, A0))] else nop
  | Eaddr { level = l; offset = ofs; by_reference = br } ->
      assert (l <= lvl);
      mips [Move (T1, FP)] ++
      iter (lvl - l) (mips [Lw (T1, Areg (8, T1))]) ++
      mips [Arith (Add, A0, T1, Oimm ofs)] ++
      if br then mips [Lw (A0, Areg (0, A0))] else nop
  | Ebinop (op, e0, e1) ->
      let a = match op with
	| Badd -> Add | Bsub -> Sub | Bmul -> Mul | Bdiv -> Div
      in
      int_expr lvl e0 ++ pushr A0 ++ int_expr lvl e1 ++ popr T1 ++
      mips [Arith (a, A0, T1, Oreg A0)]

let rec bool_expr lvl = function
  | Bcmp (op, e0, e1) ->
      let s = match op with
	| Beq -> Eq | Bneq -> Ne
	| Blt -> Lt | Ble -> Le | Bgt -> Gt | Bge -> Ge
      in
      int_expr lvl e0 ++ pushr A0 ++ int_expr lvl e1 ++ popr T1 ++
      mips [Set (s, A0, T1, Oreg A0)]
  | Band (e1, e2) ->
      let lab = label () in
      bool_expr lvl e1 ++
      mips [Beqz (A0, lab)] ++
      bool_expr lvl e2 ++
      mips [Label lab]
  | Bor (e1, e2) ->
      let lab = label () in
      bool_expr lvl e1 ++
      mips [Bnez (A0, lab)] ++
      bool_expr lvl e2 ++
      mips [Label lab]
  | Bnot e1 ->
      bool_expr lvl e1 ++
      mips [Set (Eq, A0, A0, Oimm 0)]

let rec stmt lvl = function
  | Swriteln e ->
      int_expr lvl e ++
      mips [Li (V0, 1); Syscall; Li (V0, 4); La (A0, "newline"); Syscall]
  | Scall ({proc_name = id; proc_level = l}, el) ->
      List.fold_left
	(fun code s -> code ++ int_expr lvl s ++ pushr A0) nop el ++
      mips [Move (T1, FP)] ++
      iter (lvl - l) (mips [Lw (T1, Areg (8, T1))]) ++ pushr T1 ++
      mips [Jal (symb id)] ++ popn (1 + List.length el)
  | Sassign ({ level = l; offset = ofs; by_reference = br }, e) ->
      int_expr lvl e ++
      mips [Move (T1, FP)] ++
      iter (lvl - l) (mips [Lw (T1, Areg (8, T1))]) ++
      mips
	[if br then Lw (T1, Areg (ofs, T1)) else Arith (Add, T1, T1, Oimm ofs)]
      ++
      mips [Sw (A0, Areg (0, T1))]
  | Sif (e, s1, s2) ->
      let l_else = label () in
      let l_done = label () in
      bool_expr lvl e ++
      mips [Beqz (A0, l_else)] ++
      stmt lvl s1 ++
      mips [B l_done; Label l_else] ++
      stmt lvl s2 ++
      mips [Label l_done]
  | Swhile (e, s) ->
      let l_test = label () in
      let l_done = label () in
      mips [Label l_test] ++
      bool_expr lvl e ++
      mips [Beqz (A0, l_done)] ++
      stmt lvl s ++
      mips [B l_test; Label l_done]
  | Sblock sl ->
      List.fold_left (fun code s -> code ++ stmt lvl s) nop sl

let frame_size dl =
  4 * List.fold_left (fun s d -> match d with
			| Var vl -> s + List.length vl
			| Procedure _ -> s) 0 dl

let procedure p =
  Format.eprintf "procedure %s at level %d@."
    p.pident.proc_name p.pident.proc_level;
  let fs = 8 + frame_size p.locals in
  mips [Label (symb p.pident.proc_name);
	Arith (Sub, SP, SP, Oimm fs); (* alloue la frame *)
	Sw (FP, Areg (fs - 4, SP));   (* sauve $fp *)
	Sw (RA, Areg (fs - 8, SP));   (* sauve $ra *)
	Arith (Add, FP, SP, Oimm (fs - 8)); (* $fp = ... *)
       ] ++
  stmt (p.pident.proc_level + 1) p.body ++
  mips [Lw (RA, Areg (0, FP));        (* restaure $ra *)
	Lw (FP, Areg (4, FP));        (* restaure $fp *)
	Arith (Add, SP, SP, Oimm fs); (* d�salloue la frame *)
        Jr RA]

let rec decl = function
  | Var _ -> nop
  | Procedure p -> procedure p ++ decls p.locals

and decls dl = List.fold_left (fun code d -> code ++ decl d) nop dl

let prog p =
  let fs = 8 + frame_size p.locals in
  let code_main = stmt 0 p.body in
  let code_procs = decls p.locals in
  { text =
      mips [
	Label "main";
	Arith (Sub, SP, SP, Oimm fs); (* alloue la frame *)
	Arith (Add, FP, SP, Oimm (fs - 8)); (* $fp = ... *)
      ] ++
      code_main ++
      mips [
	Arith (Add, SP, SP, Oimm fs); (* d�salloue la frame *)
	Li (V0, 10); (* exit plut�t que jr $ra *)
        Syscall;
      ] ++
      code_procs;
    data =
      [Asciiz ("newline", "\n")]
  }

