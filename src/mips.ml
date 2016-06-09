
type register =
  | A0 | A1 | V0 | T0 | T1 | S0 | RA | SP | FP

type address =
  | Alab of string
  | Areg of int * register

type operand =
  | Oimm of int
  | Oreg of register

type arith = Add | Sub | Mul | Div

type condition = Eq | Ne | Le | Lt | Ge | Gt

type label = string

type instruction =
  | Move of register * register
  | Li of register * int
  | La of register * label
  | Lw of register * address
  | Sw of register * address
  | Arith of arith * register * register * operand
  | Neg of register * register
  | Set of condition * register * register * operand
  | B of label
  | Beqz of register * label
  | Bnez of register * label
  | Jal of string
  | Jr of register
  | Jalr of register
  | Syscall
  | Label of string

type data =
  | Asciiz of string * string
  | Word of string * int

type code =
  | Clist of instruction list
  | Capp of code * code

let nop = Clist []

let mips l = Clist l

let (++) c1 c2 = Capp (c1, c2)

type program = {
  text : code;
  data : data list;
}

open Format

let print_register fmt = function
  | A0 -> pp_print_string fmt "$a0"
  | A1 -> pp_print_string fmt "$a1"
  | V0 -> pp_print_string fmt "$v0"
  | T0 -> pp_print_string fmt "$t0"
  | T1 -> pp_print_string fmt "$t1"
  | S0 -> pp_print_string fmt "$s0"
  | RA -> pp_print_string fmt "$ra"
  | SP -> pp_print_string fmt "$sp"
  | FP -> pp_print_string fmt "$fp"

let print_arith fmt = function
  | Add -> pp_print_string fmt "add"
  | Sub -> pp_print_string fmt "sub"
  | Mul -> pp_print_string fmt "mul"
  | Div -> pp_print_string fmt "div"

let print_condition fmt = function
  | Eq -> pp_print_string fmt "seq"
  | Ne -> pp_print_string fmt "sne"
  | Lt -> pp_print_string fmt "slt"
  | Le -> pp_print_string fmt "sle"
  | Gt -> pp_print_string fmt "sgt"
  | Ge -> pp_print_string fmt "sge"

let print_address fmt = function
  | Alab s -> pp_print_string fmt s
  | Areg (ofs, r) -> fprintf fmt "%d(%a)" ofs print_register r

let print_operand fmt = function
  | Oimm i -> pp_print_int fmt i
  | Oreg r -> print_register fmt r

let print_instruction fmt = function
  | Move (dst, src) ->
      fprintf fmt "\tmove %a, %a\n" print_register dst print_register src
  | Li (r, i) ->
      fprintf fmt "\tli   %a, %d\n" print_register r i
  | La (r, s) ->
      fprintf fmt "\tla   %a, %s\n" print_register r s
  | Lw (r, a) ->
      fprintf fmt "\tlw   %a, %a\n" print_register r print_address a
  | Sw (r, a) ->
      fprintf fmt "\tsw   %a, %a\n" print_register r print_address a
  | Arith (a, dst, src, op) ->
      fprintf fmt "\t%a  %a, %a, %a\n"
	print_arith a print_register dst print_register src print_operand op
  | Neg (dst, src) ->
      fprintf fmt "\tneg  %a, %a\n" print_register dst print_register src
  | Set (cond, dst, src, op) ->
      fprintf fmt "\t%a  %a, %a, %a\n"
	print_condition cond print_register dst print_register src
	print_operand op
  | B l ->
      fprintf fmt "\tb    %s\n" l
  | Beqz (r, l) ->
      fprintf fmt "\tbeqz %a, %s\n" print_register r l
  | Bnez (r, l) ->
      fprintf fmt "\tbnez %a, %s\n" print_register r l
  | Jal s ->
      fprintf fmt "\tjal  %s\n" s
  | Jalr r ->
      fprintf fmt "\tjalr %a\n" print_register r
  | Jr r ->
      fprintf fmt "\tjr   %a\n" print_register r
  | Syscall ->
      fprintf fmt "\tsyscall\n"
  | Label s ->
      fprintf fmt "%s:\n" s

let rec print_code fmt = function
  | Clist l -> List.iter (print_instruction fmt) l
  | Capp (c1, c2) -> print_code fmt c1; print_code fmt c2

let print_data fmt = function
  | Asciiz (l, s) ->
      fprintf fmt "%s:\n\t.asciiz %S\n" l s
  | Word (l, n) ->
      fprintf fmt "%s:\n\t.word %d\n" l n

let print_program fmt p =
  fprintf fmt "\t.text\n";
  print_code fmt p.text;
  fprintf fmt "\t.data\n";
  List.iter (print_data fmt) p.data;
  fprintf fmt "@."


