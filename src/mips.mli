
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

type code

val nop : code
val mips : instruction list -> code
val (++) : code -> code -> code

type data =
  | Asciiz of string * string
  | Word of string * int

type program = {
  text : code;
  data : data list;
}

val print_program : Format.formatter -> program -> unit

