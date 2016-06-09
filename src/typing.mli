
val debug: bool ref

val prog: Ast.parsed_program -> Ast.typed_program

exception Error of string
