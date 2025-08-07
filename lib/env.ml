(* env.ml *)
open Ast

(* 环境类型 *)
type t =
  { symtab : Symbol.t
  ; current_return : typ option ref
  ; errors : Error.t list ref (* 修改为 Error.t 列表 *)
  }

(* 创建新环境 *)
let create () = { symtab = Symbol.create (); current_return = ref None; errors = ref [] }

(* 添加错误 *)
let add_error env msg line column =
  env.errors := Error.create msg line column :: !(env.errors)
;;

(* 进入新作用域 *)
let enter_scope env = { env with symtab = Symbol.enter_scope env.symtab }

(* 离开作用域 *)
let leave_scope env = { env with symtab = Symbol.leave_scope env.symtab }

(* 进入循环 *)
let enter_loop env =
  Symbol.enter_loop env.symtab;
  env
;;

(* 离开循环 *)
let leave_loop env =
  Symbol.leave_loop env.symtab;
  env
;;

(* 检查是否在循环中 *)
let in_loop env = Symbol.in_loop env.symtab

(* 设置返回类型 *)
let set_return_type env typ = env.current_return := Some typ

(* 清除返回类型 *)
let clear_return_type env = env.current_return := None

(* 获取错误列表 *)
let get_errors env = List.rev !(env.errors)
