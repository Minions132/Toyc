(* codegen.ml *)
open Ast
open Symbol
open Tac
open Env

(*调试信息生成函数*) 
let string_of_operand = function
  | Var s -> s
  | Temp i -> "t" ^ string_of_int i
  | IntLit n -> string_of_int n

let string_of_tac_instr = function
  | Assign (d, s) -> Printf.sprintf "  %s := %s" (string_of_operand d) (string_of_operand s)
  | BinOp (r, o, op1, op2) -> Printf.sprintf "  %s := %s %s %s" (string_of_operand r) (string_of_operand op1) (Ast.string_of_binop o) (string_of_operand op2)
  | UnOp (r, o, op1) -> Printf.sprintf "  %s := %s %s" (string_of_operand r) (Ast.string_of_unop o) (string_of_operand op1)
  | Goto l -> "  goto " ^ l
  | IfGoto (c, l) -> Printf.sprintf "  if %s goto %s" (string_of_operand c) l
  | Call (r_opt, f, _) -> 
      let res = match r_opt with Some r -> string_of_operand r ^ " := " | None -> "" in
      Printf.sprintf "  %scall %s" res f
  | Param p -> "  param " ^ string_of_operand p
  | Return r_opt -> 
      let res = match r_opt with Some r -> " " ^ string_of_operand r | None -> "" in
      "  return" ^ res
  | Label l -> l ^ ":"

let print_program_tac (prog_tac: program_tac) =
  List.iter (fun func ->
    Printf.eprintf "TAC for function %s:\n" func.fname;
    if func.body = [] then
      Printf.eprintf "  [!!! EMPTY BODY !!!]\n"
    else
      List.iter (fun instr -> Printf.eprintf "%s\n" (string_of_tac_instr instr)) func.body;
    Printf.eprintf "\n"
  ) prog_tac


(* 用于收集生成的三地址码指令 *)
let current_tac_instrs : tac_instr list ref = ref []

(* 用于生成新的临时变量和标签 *)
let reset_temp_label_counters () =
  next_temp_id := 0;
  next_label_id := 0
;;

(* RISC-V 寄存器分配和汇编生成辅助模块 *)
module RiscVGen = struct
  (* 可用的临时寄存器 *)
  let temp_registers = [ "t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6" ]
  let arg_registers = [ "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7" ]
  let return_register = "a0"
  let zero_register = "zero"
  let stack_pointer = "sp"
  let frame_pointer = "fp"
  let return_address = "ra"

  (* 简单的寄存器分配：为每个操作数分配寄存器或栈位置 *)
  let var_to_reg = ref (Hashtbl.create 100)
  let next_temp_reg = ref 0

  let reset_reg_allocation () =
    Hashtbl.clear !var_to_reg;
    next_temp_reg := 0
  ;;

  let allocate_register () =
    if !next_temp_reg < List.length temp_registers
    then (
      let reg = List.nth temp_registers !next_temp_reg in
      incr next_temp_reg;
      reg)
    else failwith "Ran out of temporary registers"
  ;;

  let get_operand_location (op : operand) : string =
    match op with
    | Var name ->
      (match Hashtbl.find_opt !var_to_reg name with
       | Some reg -> reg
       | None ->
         let reg = allocate_register () in
         Hashtbl.add !var_to_reg name reg;
         reg)
    | Temp id ->
      let reg = allocate_register () in
      Hashtbl.add !var_to_reg ("t" ^ string_of_int id) reg;
      reg
    | IntLit n -> string_of_int n (* 立即数直接使用 *)
  ;;

  (* 汇编代码输出列表 *)
  let asm_output : string list ref = ref []
  let emit_asm (instr : string) = asm_output := !asm_output @ [ instr ]

  (* 将操作数加载到寄存器（如果需要） *)
  let load_operand (op : operand) (dest_reg : string) =
    match op with
    | IntLit n -> emit_asm (Printf.sprintf "  li %s, %d" dest_reg n)
    | Var _ | Temp _ ->
      let src_reg = get_operand_location op in
      if src_reg <> dest_reg then emit_asm (Printf.sprintf "  mv %s, %s" dest_reg src_reg)
  ;;

  (* 生成二元操作的RISC-V汇编 *)
  let gen_binop (result : operand) (op : binop) (op1 : operand) (op2 : operand) =
    let result_reg = get_operand_location result in
    let op1_reg = get_operand_location op1 in
    match op, op2 with
    | Plus, IntLit n when n >= -2048 && n <= 2047 ->
        emit_asm (Printf.sprintf "  addi %s, %s, %d" result_reg op1_reg n)
    | Minus, IntLit n when n >= -2048 && n <= 2047 ->
        emit_asm (Printf.sprintf "  addi %s, %s, %d" result_reg op1_reg (-n))
    | And, IntLit n when n >= -2048 && n <= 2047 ->
        emit_asm (Printf.sprintf "  andi %s, %s, %d" result_reg op1_reg n)
    | Or, IntLit n when n >= -2048 && n <= 2047 ->
        emit_asm (Printf.sprintf "  ori %s, %s, %d" result_reg op1_reg n)
    | _, _ ->
        let op2_reg = allocate_register () in
        load_operand op2 op2_reg;
        (match op with
        | Plus -> emit_asm (Printf.sprintf "  add %s, %s, %s" result_reg op1_reg op2_reg)
        | Minus -> emit_asm (Printf.sprintf "  sub %s, %s, %s" result_reg op1_reg op2_reg)
        | Times -> emit_asm (Printf.sprintf "  mul %s, %s, %s" result_reg op1_reg op2_reg)
        | Div -> emit_asm (Printf.sprintf "  div %s, %s, %s" result_reg op1_reg op2_reg)
        | Mod -> emit_asm (Printf.sprintf "  rem %s, %s, %s" result_reg op1_reg op2_reg)
        | Eq ->
            emit_asm (Printf.sprintf "  xor %s, %s, %s" result_reg op1_reg op2_reg);
            emit_asm (Printf.sprintf "  seqz %s, %s" result_reg result_reg)
        | Neq ->
            emit_asm (Printf.sprintf "  xor %s, %s, %s" result_reg op1_reg op2_reg);
            emit_asm (Printf.sprintf "  snez %s, %s" result_reg result_reg)
        | Lt -> emit_asm (Printf.sprintf "  slt %s, %s, %s" result_reg op1_reg op2_reg)
        | Le ->
            emit_asm (Printf.sprintf "  slt %s, %s, %s" result_reg op2_reg op1_reg);
            emit_asm (Printf.sprintf "  xori %s, %s, 1" result_reg result_reg)
        | Gt -> emit_asm (Printf.sprintf "  slt %s, %s, %s" result_reg op2_reg op1_reg)
        | Ge ->
            emit_asm (Printf.sprintf "  slt %s, %s, %s" result_reg op1_reg op2_reg);
            emit_asm (Printf.sprintf "  xori %s, %s, 1" result_reg result_reg)
        | And -> emit_asm (Printf.sprintf "  and %s, %s, %s" result_reg op1_reg op2_reg)
        | Or -> emit_asm (Printf.sprintf "  or %s, %s, %s" result_reg op1_reg op2_reg))

  (* 生成一元操作的RISC-V汇编 *)
  let gen_unop (result : operand) (op : unop) (op1 : operand) =
    let result_reg = get_operand_location result in
    let op1_reg = get_operand_location op1 in
    let instr =
      match op with
      | Not -> Printf.sprintf "  seqz %s, %s" result_reg op1_reg (* 逻辑非：如果op1为0，结果为1 *)
      | Minus -> Printf.sprintf "  neg %s, %s" result_reg op1_reg
      | Plus -> Printf.sprintf "  mv %s, %s" result_reg op1_reg (* 一元加法：直接复制 *)
    in
    emit_asm instr
  ;;

  (* 寄存器使用追踪 *)
  let used_registers = ref []
  
  (* 保存和恢复寄存器 *)
  let save_registers regs =
    List.iteri (fun i reg ->
      if List.mem reg !used_registers then
        emit_asm (Printf.sprintf "  sw %s, %d(sp)" reg (-(i + 1) * 4))
    ) regs

  let restore_registers regs =
    List.iteri (fun i reg ->
      if List.mem reg !used_registers then
        emit_asm (Printf.sprintf "  lw %s, %d(sp)" reg (-(i + 1) * 4))
    ) regs

  (* 计算需要的栈空间 *)
  let calculate_stack_size func =
    let local_vars = ref 0 in
    let max_args = ref 0 in
    List.iter (fun instr ->
      match instr with
      | Call (_, _, args) -> max_args := max !max_args (List.length args)
      | Assign (Var _, _) -> incr local_vars
      | _ -> ()
    ) func.body;
    16 + (!local_vars * 4) + (!max_args * 4)

  (* 改进的函数序言和尾声生成 *)
  let generate_function_prologue fname stack_size =
    emit_asm (Printf.sprintf "\n%s:" fname);
    emit_asm "  # Function prologue";
    emit_asm (Printf.sprintf "  addi sp, sp, -%d" stack_size);
    emit_asm (Printf.sprintf "  sw ra, %d(sp)" (stack_size - 4));
    emit_asm (Printf.sprintf "  sw fp, %d(sp)" (stack_size - 8));
    emit_asm "  addi fp, sp, 0"

  let generate_function_epilogue stack_size =
    emit_asm "  # Function epilogue";
    emit_asm (Printf.sprintf "  lw ra, %d(sp)" (stack_size - 4));
    emit_asm (Printf.sprintf "  lw fp, %d(sp)" (stack_size - 8));
    emit_asm (Printf.sprintf "  addi sp, sp, %d" stack_size);
    emit_asm "  ret"

  (* 改进的generate_risc_v函数 *)
  let generate_risc_v (prog_tac : program_tac) : string =
    reset_reg_allocation ();
    asm_output := [];
    used_registers := [];
    
    (* 添加汇编指令头 *)
    emit_asm ".section .text";
    emit_asm ".global main";
    
    List.iter (fun func ->
      reset_reg_allocation ();
      let stack_size = calculate_stack_size func in
      
      (* 生成函数序言 *)
      generate_function_prologue func.fname stack_size;
      
      (* 保存被调用者保存的寄存器 *)
      save_registers temp_registers;
      
      (* 处理参数 *)
      List.iteri (fun i (name, _) ->
        if i < List.length arg_registers then
          let arg_reg = List.nth arg_registers i in
          Hashtbl.add !var_to_reg name arg_reg;
          used_registers := arg_reg :: !used_registers
      ) func.params;
      
      (* 生成函数体 *)
      List.iter (fun instr ->
        match instr with
        | Assign (dest, src) ->
            let dest_reg = get_operand_location dest in
            used_registers := dest_reg :: !used_registers;
            load_operand src dest_reg
            
        | BinOp (result, op, op1, op2) ->
            gen_binop result op op1 op2
            
        | UnOp (result, op, op1) ->
            gen_unop result op op1
            
        | Call (result_opt, fname, args) ->
            (* 保存调用者保存的寄存器 *)
            save_registers !used_registers;
            
            (* 传递参数 *)
            List.iteri (fun i arg ->
              if i < List.length arg_registers then
                let arg_reg = List.nth arg_registers i in
                load_operand arg arg_reg
            ) args;
            
            emit_asm (Printf.sprintf "  call %s" fname);
            
            (* 恢复调用者保存的寄存器 *)
            restore_registers !used_registers;
            
            (* 处理返回值 *)
            Option.iter (fun result ->
              let result_reg = get_operand_location result in
              if result_reg <> return_register then
                emit_asm (Printf.sprintf "  mv %s, %s" result_reg return_register)
            ) result_opt
            
        | Goto label ->
            emit_asm (Printf.sprintf "  j %s" label)
            
        | IfGoto (cond, label) ->
            let cond_reg = get_operand_location cond in
            emit_asm (Printf.sprintf "  bnez %s, %s" cond_reg label)
            
        | Return expr_opt ->
            (match expr_opt with
            | Some expr ->
                load_operand expr return_register
            | None -> ());
            generate_function_epilogue stack_size
            
        | Label label ->
            emit_asm (Printf.sprintf "%s:" label)
            
        | Param _ -> ()  (* 参数在Call指令中处理 *)
      ) func.body;
      
      (* 如果函数没有明确的返回语句，添加默认返回 *)
      match List.rev func.body with
      | Return _ :: _ -> ()
      | _ -> generate_function_epilogue stack_size
    ) prog_tac;
    
    String.concat "\n" !asm_output
end

(* 表达式翻译函数：将AST表达式转换为三地址码，并返回表达式结果所在的operand *)
let rec generate_expr_tac (env : Env.t) (expr : expr) : operand =
  match expr.expr_desc with
  | IntLit n -> IntLit n
  | Var name -> Var name
  | BinOp (e1, op, e2) ->
    let op1_tac = generate_expr_tac env e1 in
    let op2_tac = generate_expr_tac env e2 in
    let result_temp = new_temp () in
    current_tac_instrs
    := !current_tac_instrs @ [ BinOp (result_temp, op, op1_tac, op2_tac) ];
    result_temp
  | UnOp (op, e) ->
    let op1_tac = generate_expr_tac env e in
    let result_temp = new_temp () in
    current_tac_instrs := !current_tac_instrs @ [ UnOp (result_temp, op1_tac) ];
    result_temp
  | Call (fname, args) ->
    let arg_tacs = List.map (generate_expr_tac env) args in
    List.iter
      (fun arg_tac -> current_tac_instrs := !current_tac_instrs @ [ Param arg_tac ])
      arg_tacs;
    (match Symbol.find_func env.symtab fname with
     | Some finfo ->
       if finfo.return_type = TVoid
       then (
         current_tac_instrs := !current_tac_instrs @ [ Call (None, fname, []) ];
         Temp (-1))
       else (
         let result_temp = new_temp () in
         current_tac_instrs
         := !current_tac_instrs @ [ Call (Some result_temp, fname, []) ];
         result_temp)
     | None ->
      Env.add_error
       env
       (Printf.sprintf "未定义的函数 '%s'" fname)
       expr.expr_loc.line
       expr.expr_loc.column;
       Temp (-1))
;;

(* 语句翻译函数：将AST语句转换为三地址码序列 *)
let rec generate_stmt_tac (env : Env.t) (stmt : stmt) : unit =
  match stmt.stmt_desc with
  | Block stmts -> List.iter (generate_stmt_tac env) stmts
  | Empty -> ()
  | ExprStmt e -> ignore (generate_expr_tac env e)
  | VarDecl (name, init_expr_opt) ->
    (match init_expr_opt with
     | Some init_expr ->
       let init_tac = generate_expr_tac env init_expr in
       current_tac_instrs := !current_tac_instrs @ [ Assign (Var name, init_tac) ]
     | None -> ())
  | Assign (name, e) ->
    let expr_tac = generate_expr_tac env e in
    current_tac_instrs := !current_tac_instrs @ [ Assign (Var name, expr_tac) ]
  | If (cond, then_stmt, else_stmt_opt) ->
    let cond_tac = generate_expr_tac env cond in
    let label_else = new_label () in
    let label_end = new_label () in
    current_tac_instrs := !current_tac_instrs @ [ IfGoto (cond_tac, label_end) ];
    current_tac_instrs := !current_tac_instrs @ [ Goto label_else ];
    generate_stmt_tac env then_stmt;
    current_tac_instrs := !current_tac_instrs @ [ Goto label_end ];
    current_tac_instrs := !current_tac_instrs @ [ Label label_else ];
    Option.iter (generate_stmt_tac env) else_stmt_opt;
    current_tac_instrs := !current_tac_instrs @ [ Label label_end ]
  | While (cond, body) ->
    let label_loop_start = new_label () in
    let label_loop_end = new_label () in
    current_tac_instrs := !current_tac_instrs @ [ Label label_loop_start ];
    let cond_tac = generate_expr_tac env cond in
    current_tac_instrs := !current_tac_instrs @ [ IfGoto (cond_tac, label_loop_end) ];
    generate_stmt_tac env body;
    current_tac_instrs := !current_tac_instrs @ [ Goto label_loop_start ];
    current_tac_instrs := !current_tac_instrs @ [ Label label_loop_end ]
  | Break ->
      Env.add_error
      env
      "Break/Continue in codegen not fully implemented for nested loops"
      stmt.stmt_loc.line
      stmt.stmt_loc.column;
    current_tac_instrs := !current_tac_instrs @ [ Goto (new_label ()) ]
  | Continue ->
    Env.add_error
      env
      "Break/Continue in codegen not fully implemented for nested loops"
      stmt.stmt_loc.line
      stmt.stmt_loc.column;
    current_tac_instrs := !current_tac_instrs @ [ Goto (new_label ()) ]
  | Return expr_opt ->
    (match expr_opt with
     | Some e ->
       let ret_tac = generate_expr_tac env e in
       current_tac_instrs := !current_tac_instrs @ [ Return (Some ret_tac) ]
     | None -> current_tac_instrs := !current_tac_instrs @ [ Return None ])
;;

(* 函数翻译函数 *)
let generate_func_tac (env : Env.t) (func_def : func_def) : func_tac =
  reset_temp_label_counters ();
  current_tac_instrs := [];
  List.iter (generate_stmt_tac env) func_def.body;
  { fname = func_def.fname
  ; params = func_def.params
  ; body = !current_tac_instrs
  ; return_type = func_def.rtyp
  }
;;

(* 整个程序翻译函数 *)
let generate_program_tac (prog : program) : program_tac =
  let initial_env = Env.create () in
  List.map (generate_func_tac initial_env) prog
;;

(* 生成RISC-V汇编的主入口 *)
let generate_risc_v_assembly (prog : program) : string =
  let prog_tac = generate_program_tac prog in
  RiscVGen.generate_risc_v prog_tac
;;
