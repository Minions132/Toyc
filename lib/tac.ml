(* tac.ml *)

type operand =
  | Var of string (* 变量名 *)
  | Temp of int (* 临时变量，需要唯一编号 *)
  | IntLit of int
(* 整数常量 *)
(* 更多操作数类型，如：函数地址，标签等 *)

type tac_instr =
  (* 赋值 *)
  | Assign of operand * operand
  (* 算术/逻辑/关系运算 *)
  | BinOp of operand * Ast.binop * operand * operand (* result, op1, operator, op2 *)
  (* 一元运算 *)
  | UnOp of operand * Ast.unop * operand (* result, operator, op1 *)
  (* 跳转 *)
  | Goto of string (* Label *)
  | IfGoto of operand * string (* condition, Label *)
  (* 函数调用 *)
  | Call of operand option * string * operand list (* optional result, func_name, args *)
  | Param of operand (* 函数参数 *)
  | Return of operand option (* optional return value *)
  (* 内存操作（如果ToyC支持指针/数组，但文档中说省去了） *)
  (* | Load of operand * operand (* result = *addr *)
  | Store of operand * operand (* *addr = value *) *)
  (* 标签 *)
  | Label of string

(* 栈帧相关的类型定义 *)
type stack_info = {
  frame_size: int;          (* 栈帧大小 *)
  spill_offset: int;        (* 溢出变量的偏移量 *)
  local_vars: (string * int) list;  (* 局部变量及其栈偏移量 *)
}

(* 寄存器分配相关的类型定义 *)
type reg_info = {
  reg_name: string;         (* 寄存器名称 *)
  is_caller_saved: bool;    (* 是否是调用者保存的寄存器 *)
  var_name: string option;  (* 当前存储的变量名 *)
}

(* 函数定义扩展，添加栈帧信息 *)
type func_tac = {
  fname : string;
  params : Ast.param list;
  body : tac_instr list;
  return_type : Ast.typ;
  stack_info : stack_info;  (* 新增：栈帧信息 *)
}

type program_tac = func_tac list

(* 用于生成唯一临时变量名和标签 *)
let next_temp_id = ref 0

let new_temp () =
  let id = !next_temp_id in
  incr next_temp_id;
  Temp id
;;

let next_label_id = ref 0
let new_label_prefix = "L"

let new_label () =
  let id = !next_label_id in
  incr next_label_id;
  new_label_prefix ^ string_of_int id
;;

(* 辅助函数：计算函数的栈帧大小 *)
let calculate_frame_size (func : func_tac) : int =
  let param_count = List.length func.params in
  let local_var_count = 
    List.fold_left (fun count instr ->
      match instr with
      | Assign (Var _, _) -> count + 1
      | _ -> count
    ) 0 func.body
  in
  (* 对齐到16字节边界 *)
  ((param_count + local_var_count) * 4 + 15) land (lnot 15)

(* 辅助函数：获取变量在栈中的位置 *)
let get_var_stack_position (var_name : string) (stack_info : stack_info) : int option =
  List.find_map (fun (name, offset) -> 
    if name = var_name then Some offset else None
  ) stack_info.local_vars

(* 检查是否需要保存调用者保存的寄存器 *)
let needs_callee_saves (instrs : tac_instr list) : bool =
  List.exists (function
    | Call _ -> true
    | _ -> false
  ) instrs

(* 生成新的临时标签，支持不同用途 *)
let new_label_with_prefix prefix =
  let id = !next_label_id in
  incr next_label_id;
  prefix ^ string_of_int id
;;

let new_loop_label () = new_label_with_prefix "LOOP_"
let new_if_label () = new_label_with_prefix "IF_"
let new_endif_label () = new_label_with_prefix "ENDIF_"
let new_else_label () = new_label_with_prefix "ELSE_"

(* 用于分析基本块的辅助函数 *)
let find_basic_blocks (instrs : tac_instr list) : tac_instr list list =
  let rec split_blocks acc current = function
    | [] -> List.rev (List.rev current :: acc)
    | (Label _ as instr) :: rest when current <> [] ->
        split_blocks (List.rev current :: acc) [instr] rest
    | instr :: rest ->
        split_blocks acc (instr :: current) rest
  in
  split_blocks [] [] instrs
;;
