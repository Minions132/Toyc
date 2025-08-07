(* open Toyc
open Ast
let rec print_expr = function
  | IntLit n -> Printf.sprintf "Number(%d)" n
  | Var v -> Printf.sprintf "ID(%s)" v
  | BinOp (e1, op, e2) ->
      let op_str = match op with
        | Plus -> "+" | Minus -> "-" | Times -> "*" | Div -> "/" | Mod -> "%"
        | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Le -> "<=" | Gt -> ">" | Ge -> ">="
        | And -> "&&" | Or -> "||"
      in
      Printf.sprintf "Binop(%s, %s, %s)" op_str (print_expr e1.expr_desc) (print_expr e2.expr_desc)
  | UnOp (Minus, e) -> Printf.sprintf "Unop(-, %s)" (print_expr e.expr_desc)
  | UnOp (Not, e) -> Printf.sprintf "Unop(!, %s)" (print_expr e.expr_desc)
  | Call (fname, args) ->
      Printf.sprintf "Call(%s, [%s])" fname 
        (String.concat ", " (List.map (fun e -> print_expr e.expr_desc) args))

let rec print_stmt indent = function

  | Return None -> indent ^ "Return"
  | Return (Some e) -> indent ^ Printf.sprintf "Return(%s)" (print_expr e.expr_desc)
  | VarDecl (v, None) -> indent ^ Printf.sprintf "Dec1(%s, None)" v
  | VarDecl (v, Some e) -> indent ^ Printf.sprintf "Dec1(%s, %s)" v (print_expr e.expr_desc)
  | Assign (v, e) -> indent ^ Printf.sprintf "Assign(%s, %s)" v (print_expr e.expr_desc)
  | ExprStmt e -> indent ^ print_expr e.expr_desc
  | Break -> indent ^ "Break"
  | Continue -> indent ^ "Continue"
  | Block stmts ->
      let new_indent = indent ^ "\n" in
      let stmt_str =
        if stmts = [] then ""
        else "\n" ^ (String.concat ",\n" (List.map (print_stmt new_indent) (List.map (fun s -> s.stmt_desc) stmts))) ^ "\n" ^ indent
      in
      "Block([" ^ stmt_str ^ "])"
  | If (cond, then_stmt, else_opt) ->
      let new_indent = indent ^ "    " in
      let then_str = print_stmt new_indent then_stmt.stmt_desc in
      let else_str = match else_opt with
        | None -> ""
        | Some stmt -> ",\n" ^ print_stmt new_indent stmt.stmt_desc
      in
      indent ^ Printf.sprintf "If(%s, %s%s)" 
        (print_expr cond.expr_desc) then_str else_str
  | While (cond, stmt) ->
      let new_indent = indent ^ "    " in
      let stmt_str = print_stmt new_indent stmt.stmt_desc in
      indent ^ Printf.sprintf "While(%s, %s)" 
        (print_expr cond.expr_desc) stmt_str

let print_func func =
  let typ_str = match func.rtyp with
    | TInt -> "int"
    | TVoid -> "void"
  in
  let params_str = String.concat ", " 
    (List.map (fun (n, _) -> n) func.params) in
  
  let body_str = String.concat ",\n" 
    (List.map (print_stmt "    ") (List.map (fun s -> s.stmt_desc) func.body)) in
  
  Printf.sprintf "Function %s(%s) : %s {\n%s\n}" 
    func.fname params_str typ_str body_str


 let () =  
    if Array.length Sys.argv < 2 then (  
        Printf.eprintf "用法: %s <file.tc>\n" Sys.argv.(0);  
        exit 1  
    );

    let filename = Sys.argv.(1) in  
    let ic = open_in filename in  
    let lexbuf = Lexing.from_channel ic in  
    (* 设置文件名到lexbuf的位置信息中 *)
    lexbuf.Lexing.lex_curr_p <- { 
        lexbuf.Lexing.lex_curr_p with 
        Lexing.pos_fname = filename 
    };

    try  
        let ast = Parser.program Lexer.token lexbuf in  
        let errors = Semantic.check_program ast in
        if errors <> [] then (
          (* 打印所有语义错误 *)
          List.iter (fun err ->
              Printf.eprintf "语义错误: %s\n" err.Error.msg
          ) errors;
          close_in ic;
          exit 1
        );
        List.iter (fun f -> print_endline (print_func f)) ast;  
        close_in ic  
    with  
    | Parsing.Parse_error ->  (* 正确的异常名称 *)
        let start_pos = Lexing.lexeme_start_p lexbuf in  (* 获取token起始位置 *)
        let end_pos = Lexing.lexeme_end_p lexbuf in      (* 获取token结束位置 *)
        let tok = Lexing.lexeme lexbuf in
        
        (* 计算起始位置的行列号 *)
        let start_line = start_pos.Lexing.pos_lnum in
        let start_col = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol + 1 in
        
        (* 计算结束位置的行列号 *)
        let end_line = end_pos.Lexing.pos_lnum in
        let end_col = end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol + 1 in
        
        (* 更精确的错误信息 *)
        if start_line = end_line then
            Printf.eprintf "语法错误: %s, 行 %d, 列 %d-%d\n在 token '%s' 处\n"  
                filename start_line start_col end_col tok
        else
            Printf.eprintf "语法错误: %s, 行 %d 列 %d 到 行 %d 列 %d\n在 token '%s' 处\n"  
                filename start_line start_col end_line end_col tok;
        
        close_in ic;  
        exit 1
*)

(* main.ml - 解析.tc文件并输出AST *)

(* 导入所需模块 *)
(* open Toyc
open Ast

(* 辅助函数：将类型转换为字符串 *)
let string_of_typ = function
  | TInt -> "int"
  | TVoid -> "void"
;;

(* 辅助函数：将二元操作符转换为字符串 *)
let string_of_binop = function
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
  | Plus -> "+"
  | Minus -> "-"
;;

(* 辅助函数：将一元操作符转换为字符串 *)
let string_of_unop = function
  | Not -> "!"
  | Minus -> "-"
  | Plus -> "+"
;;

(* 递归打印表达式（不包含位置信息） *)
let rec print_expr e =
  match e.expr_desc with
  | IntLit i -> string_of_int i
  | Var s -> s
  | BinOp (e1, op, e2) ->
    Printf.sprintf "BinOp(%s %s %s)" (print_expr e1) (string_of_binop op) (print_expr e2)
  | UnOp (op, e) -> Printf.sprintf "UnOp(%s%s)" (string_of_unop op) (print_expr e)
  | Call (fname, args) ->
    Printf.sprintf "Call %s(%s)" fname (String.concat ", " (List.map print_expr args))
;;

(* 递归打印语句（不包含位置信息） *)
let rec print_stmt s =
  match s.stmt_desc with
  | Block stmts ->
    "{\n" ^ String.concat "" (List.map (fun st -> print_stmt st ^ "\n") stmts) ^ "}"
  | Empty -> ";"
  | ExprStmt e -> print_expr e ^ ";"
  | VarDecl (v, None) -> "VarDecl(" ^ v ^ ", None);"
  | VarDecl (v, Some e) -> "VarDecl(" ^ v ^ ", Num(" ^ print_expr e ^ "));"
  | Assign (v, e) -> "Assign(" ^ v ^ ", ID(" ^ print_expr e ^ "));"
  | If (cond, then_branch, else_branch) ->
    let else_str =
      match else_branch with
      | None -> ""
      | Some st -> " else " ^ print_stmt st
    in
    Printf.sprintf "if (%s) \n%s%s" (print_expr cond) (print_stmt then_branch) else_str
  | While (cond, body) ->
    Printf.sprintf "while (%s) \n%s" (print_expr cond) (print_stmt body)
  | Break -> "break;"
  | Continue -> "continue;"
  | Return None -> "return;"
  | Return (Some e) -> "return " ^ print_expr e ^ ";"
;;

(* 打印函数参数 *)
let print_param (name, typ) = Printf.sprintf "%s: %s" name (string_of_typ typ)

(* 打印函数定义（不包含位置信息） *)
let print_func_def func =
  Printf.sprintf
    "Function: %s %s(%s)\n%s"
    (string_of_typ func.rtyp)
    func.fname
    (String.concat ", " (List.map print_param func.params))
    (print_stmt { stmt_desc = Block func.body; stmt_loc = { line = 0; column = 0 } })
;;

(* 主程序逻辑 *)
let () =
  if Array.length Sys.argv < 2
  then (
    Printf.eprintf "用法: %s <file.tc>\n" Sys.argv.(0);
    exit 1);
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  (* 设置文件名到lexbuf的位置信息中 *)
  lexbuf.Lexing.lex_curr_p
  <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  try
    let ast = Parser.program Lexer.token lexbuf in
    let errors = Semantic.check_program ast in
    if errors <> []
    then (
      (* 打印所有语义错误 *)
      List.iter (fun err -> Printf.eprintf "语义错误: %s\n" (Error.to_string err)) errors;
      close_in ic;
      exit 1);
    List.iter (fun f -> print_endline (print_func_def f)) ast;
    close_in ic
  with
  | Parsing.Parse_error ->
    (* 正确的异常名称 *)
    let start_pos = Lexing.lexeme_start_p lexbuf in
    (* 获取token起始位置 *)
    let end_pos = Lexing.lexeme_end_p lexbuf in
    (* 获取token结束位置 *)
    let tok = Lexing.lexeme lexbuf in
    (* 计算起始位置的行列号 *)
    let start_line = start_pos.Lexing.pos_lnum in
    let start_col = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol + 1 in
    (* 计算结束位置的行列号 *)
    let end_line = end_pos.Lexing.pos_lnum in
    let end_col = end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol + 1 in
    (* 更精确的错误信息 *)
    if start_line = end_line
    then
      Printf.eprintf
        "语法错误: %s, 行 %d, 列 %d-%d\n在 token '%s' 处\n"
        filename
        start_line
        start_col
        end_col
        tok
    else
      Printf.eprintf
        "语法错误: %s, 行 %d 列 %d 到 行 %d 列 %d\n在 token '%s' 处\n"
        filename
        start_line
        start_col
        end_line
        end_col
        tok;
    close_in ic;
    exit 1
;; *)

(*let () =
  if Array.length Sys.argv < 2
  then (
    Printf.eprintf "Usage: %s <filename.tc>\n" Sys.argv.(0);
    exit 1);
  let filename = Sys.argv.(1) in
  let in_channel = open_in filename in
  let lexbuf = Lexing.from_channel in_channel in
  lexbuf.Lexing.lex_curr_p
  <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  try
    let program = Parser.program Lexer.token lexbuf in
    close_in in_channel;
    (* 打印AST（不包含位置信息） *)
    List.iter
      (fun func ->
         print_endline (print_func_def func);
         print_endline "")
      program
  with
  | Lexer.Error (msg, line, col) ->
    close_in in_channel;
    Printf.eprintf "Lexical error at line %d, column %d: %s\n" line col msg;
    exit 1
  | Parsing.Parse_error ->
    close_in in_channel;
    let pos = lexbuf.Lexing.lex_curr_p in
    Printf.eprintf
      "Syntax error at line %d, column %d\n"
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1);
    exit 1
;;*)

(* main.ml - 编译器主程序 *)

(* 1. 导入所有需要的模块 *)
open Toyc
(* open Ast
open Symbol
open Env
open Typecheck
open Tac
open Codegen *)

(* 2. 定义主函数逻辑 *)
let main () =
  (* 从标准输入(stdin)创建词法缓冲区 *)
  let lexbuf = Lexing.from_channel stdin in
  
  try
    (* 3. 语法分析：调用Parser生成AST *)
    (* Parser.program 是由 ocamlyacc 生成的解析器入口点 *)
    (* Lexer.token 是由 ocamllex 生成的词法分析器函数 *)
    let ast = Parser.program Lexer.token lexbuf in
    
    (* 4. 语义分析：检查AST的类型和语义规则 *)
    let errors = Semantic.check_program ast in
    
    (* 检查是否有语义错误 *)
    if errors <> [] then (
      (* 如果有错误，则在标准错误(stderr)上打印它们并退出 *)
      List.iter (fun err ->
          Printf.eprintf "语义错误: %s\n" (Error.to_string err)
      ) errors;
      exit 1
    );
    
    (* 5. 代码生成：将AST转换为RISC-V汇编代码 *)
    let riscv_assembly = Codegen.generate_risc_v_assembly ast in
    
    (* 6. 输出结果：将生成的汇编代码打印到标准输出(stdout) *)
    print_endline riscv_assembly

  with
  | Parsing.Parse_error ->  (* 捕获语法分析阶段的错误 *)
      (* 获取错误发生的位置信息 *)
      let pos = lexbuf.Lexing.lex_curr_p in
      let line = pos.Lexing.pos_lnum in
      let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
      let tok = Lexing.lexeme lexbuf in
      (* 在标准错误(stderr)上打印语法错误信息并退出 *)
      Printf.eprintf "语法错误: 在文件 stdin 的第 %d 行, 第 %d 列\n在 token '%s' 处\n" line col tok;
      exit 1
  | Failure msg -> (* 捕获其他可能的运行时错误，如寄存器分配用尽等 *)
      Printf.eprintf "运行时错误: %s\n" msg;
      exit 1
;;

(* 7. 执行主函数 *)
let () = main ()