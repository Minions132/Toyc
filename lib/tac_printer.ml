open Tac
open Ast
let operand_to_string (op:operand)= 
match op with
  | Var name -> name
  | Temp id -> Printf.sprintf "t%d" id
  | IntLit n -> string_of_int n

let binop_to_string:Ast.binop -> string = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | And -> "&&"
  | Or -> "||"

let unop_to_string:Ast.unop -> string = function
  | Not -> "!"
  | Minus -> "-"
  | Plus -> "+"

let instr_to_string:Tac.tac_instr->string = function
  | Assign (dst, src) ->
    Printf.sprintf "%s = %s" (operand_to_string dst) (operand_to_string src)
  | BinOp (res, op1, op, op2) ->
    Printf.sprintf "%s = %s %s %s"
      (operand_to_string res)
      (operand_to_string op1)
      (binop_to_string op)
      (operand_to_string op2)
  | UnOp (res, op, op1) ->
    Printf.sprintf "%s = %s%s"
      (operand_to_string res)
      (unop_to_string op)
      (operand_to_string op1)
  | Goto lbl ->
    Printf.sprintf "goto %s" lbl
  | IfGoto (cond, lbl) ->
    Printf.sprintf "if %s goto %s" (operand_to_string cond) lbl
  | Call (ret_opt, fname, args) ->
    let args_str = String.concat ", " (List.map operand_to_string args) in
    (match ret_opt with
     | Some ret -> Printf.sprintf "%s = call %s(%s)" (operand_to_string ret) fname args_str
     | None -> Printf.sprintf "call %s(%s)" fname args_str)
  | Param op ->
    Printf.sprintf "param %s" (operand_to_string op)
  | Return ret_opt ->
    (match ret_opt with
     | Some ret -> Printf.sprintf "return %s" (operand_to_string ret)
     | None -> "return")
  | Label lbl ->
    Printf.sprintf "%s:" lbl

let print_func_tac oc (func_tac:Tac.func_tac) =
  Printf.fprintf oc "Function %s:\n" func_tac.fname;
  List.iter (fun instr ->
    Printf.fprintf oc "  %s\n" (instr_to_string instr)
  ) func_tac.body

let print_program_tac oc prog_tac =
  List.iter (print_func_tac oc) prog_tac