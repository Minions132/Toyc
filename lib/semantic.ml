(* semantic.ml *)
open Ast
open Env
open Typecheck

let check_var_decl env name typ init_expr_opt =
  (* 获取位置信息 - 优先使用初始化表达式的位置 *)
  let loc =
    match init_expr_opt with
    | Some e -> e.expr_loc
    | None -> { line = 0; column = 0 }
    (* 如果没有初始化表达式，使用默认位置 *)
  in
  (* 1. 尝试添加变量到符号表 *)
  if not (Symbol.add_var env.symtab name typ loc)
  then Env.add_error env (Printf.sprintf "变量 '%s' 重复定义" name) loc.line loc.column;
  match init_expr_opt with
  | None -> () (* 无初始化表达式，直接返回 *)
  | Some e ->
    let t = check_expr env e in
    (* 检查表达式类型 *)
    check_type env typ t e.expr_loc;
    (* 验证类型是否匹配 *)
    ignore (Symbol.update_var env.symtab name)
;;

(* 更新符号表 *)

let check_assign env name expr =
  match Symbol.find_var env.symtab name with
  | Some info ->
    let t = check_expr env expr in
    check_type env info.vtype t expr.expr_loc
  | None ->
    Env.add_error
      env
      (Printf.sprintf "未定义的变量 '%s'" name)
      expr.expr_loc.line
      expr.expr_loc.column
;;

let rec check_stmt env stmt =
  match stmt.stmt_desc with
  | Block stmts ->
    let env = Env.enter_scope env in
    List.iter (fun s -> check_stmt env s) stmts;
    Env.leave_scope env |> ignore
  | Empty -> ()
  | ExprStmt e -> ignore (check_expr env e)
  | VarDecl (name, init) -> check_var_decl env name TInt init
  | Assign (name, e) -> check_assign env name e
  | If (cond, then_stmt, else_stmt) ->
    let t = check_expr env cond in
    check_type env TInt t cond.expr_loc;
    check_stmt env then_stmt;
    Option.iter (check_stmt env) else_stmt
  | While (cond, body) ->
    let t = check_expr env cond in
    check_type env TInt t cond.expr_loc;
    let env = Env.enter_loop env in
    check_stmt env body;
    Env.leave_loop env |> ignore
  | Break | Continue ->
    if not (Env.in_loop env)
    then Env.add_error env "break/continue 必须在循环内" stmt.stmt_loc.line stmt.stmt_loc.column
  | Return None ->
    (match !(env.current_return) with
     | Some TVoid -> ()
     | Some _ -> Env.add_error env "非void函数必须返回值" stmt.stmt_loc.line stmt.stmt_loc.column
     | None -> Env.add_error env "return 语句不在函数内" stmt.stmt_loc.line stmt.stmt_loc.column)
  | Return (Some e) ->
    (match !(env.current_return) with
     | Some rt ->
       let t = check_expr env e in
       check_type env rt t e.expr_loc
     | None -> Env.add_error env "return 语句不在函数内" stmt.stmt_loc.line stmt.stmt_loc.column)
;;

let check_func env func =
  if
    not
      (Symbol.add_func
         env.symtab
         func.fname
         { return_type = func.rtyp; params = func.params })
  then
    Env.add_error
      env
      (Printf.sprintf "重复定义函数 '%s'" func.fname)
      func.func_loc.line
      func.func_loc.column;
  let env = { env with current_return = ref (Some func.rtyp) } in
  let env = Env.enter_scope env in
  List.iter
    (fun (name, t) -> ignore (Symbol.add_var env.symtab name t func.func_loc))
    func.params;
  List.iter (check_stmt env) func.body;
  Env.leave_scope env |> ignore;
  env.current_return := None
;;

let check_program prog =
  let env = Env.create () in
  List.iter (check_func env) prog;
  Env.get_errors env
;;
