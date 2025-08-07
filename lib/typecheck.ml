(* typecheck.ml *)
open Ast
open Env

module List = struct
  include List

  let take lst n =
    let rec aux acc n = function
      | [] -> List.rev acc
      | h :: t -> if n <= 0 then List.rev acc else aux (h :: acc) (n - 1) t
    in
    aux [] n lst
  ;;
end

let type_to_string = function
  | TInt -> "int"
  | TVoid -> "void"
;;

let check_type env expected actual loc =
  if expected <> actual
  then
    Env.add_error
      env
      (Printf.sprintf
         "类型不匹配: 期望 %s 但得到 %s"
         (type_to_string expected)
         (type_to_string actual))
      loc.line
      loc.column
;;

let rec check_expr env expr =
  match expr.expr_desc with
  | IntLit _ -> TInt
  | Var name ->
    (match Symbol.find_var env.symtab name with
     | Some info -> info.vtype
     | None ->
       Env.add_error
         env
         (Printf.sprintf "未定义的变量 '%s'" name)
         expr.expr_loc.line
         expr.expr_loc.column;
       TInt)
  | BinOp (e1, op, e2) ->
    let t1 = check_expr env e1 in
    let t2 = check_expr env e2 in
    (match op with
     | Plus | Minus | Times | Div | Mod ->
       check_type env TInt t1 e1.expr_loc;
       check_type env TInt t2 e2.expr_loc;
       TInt
     | Eq | Neq | Lt | Le | Gt | Ge | And | Or ->
       check_type env TInt t1 e1.expr_loc;
       check_type env TInt t2 e2.expr_loc;
       TInt)
  | UnOp (op, e) ->
    let t = check_expr env e in
    (match op with
     | Not | Minus | Plus ->
       check_type env TInt t e.expr_loc;
       TInt)
  (*| Call (fname, args) ->
    (match Symbol.find_func env.symtab fname with
     | Some finfo ->
       if List.length finfo.params <> List.length args
       then
         Env.add_error
           env
           (Printf.sprintf
              "参数数量不匹配: 期望 %d 但得到 %d"
              (List.length finfo.params)
              (List.length args))
           expr.expr_loc.line
           expr.expr_loc.column;
       List.iter2
         (fun (_, pt) arg ->
            let at = check_expr env arg in
            check_type env pt at arg.expr_loc)
         finfo.params
         args;
       finfo.return_type
     | None ->
       Env.add_error
         env
         (Printf.sprintf "未定义的函数 '%s'" fname)
         expr.expr_loc.line
         expr.expr_loc.column;
       TVoid)
  *)
  | Call (fname, args) ->
    (match Symbol.find_func env.symtab fname with
     | Some finfo ->
       let param_len = List.length finfo.params in
       let arg_len = List.length args in
       (* 1. 检查参数数量 *)
       if param_len <> arg_len
       then (
         Env.add_error
           env
           (Printf.sprintf "函数 '%s' 需要 %d 个参数但得到 %d" fname param_len arg_len)
           expr.expr_loc.line
           expr.expr_loc.column;
         (* 2. 检查尽可能多的参数 *)
         let min_len = min param_len arg_len in
         let params_trunc = List.take finfo.params min_len in
         let args_trunc = List.take args min_len in
         List.iter2
           (fun (_, pt) arg ->
              let at = check_expr env arg in
              check_type env pt at arg.expr_loc)
           params_trunc
           args_trunc;
         finfo.return_type)
       else (
         (* 3. 正常情况 *)
         List.iter2
           (fun (_, pt) arg ->
              let at = check_expr env arg in
              check_type env pt at arg.expr_loc)
           finfo.params
           args;
         finfo.return_type)
     | None ->
       Env.add_error
         env
         (Printf.sprintf "未定义的函数: %s" fname)
         expr.expr_loc.line
         expr.expr_loc.column;
       TVoid)
;;
