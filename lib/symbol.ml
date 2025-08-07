(* symbol.ml *)
open Ast

(* 变量信息 *)
type var_info =
  { vtype : typ
  ; mutable initialized : bool
  ; defined_loc : loc (* 新增：记录定义位置 *)
  }

(* 函数信息 *)
type func_info =
  { return_type : typ
  ; params : param list
  }

(* 单个作用域 *)
type scope =
  { vars : (string, var_info) Hashtbl.t
  ; funcs : (string, func_info) Hashtbl.t
  }

(* 符号表结构 *)
type t =
  { mutable current : scope
  ; parent : t option
  ; mutable loop_depth : int
  }

(* 创建新作用域 *)
let create_scope () = { vars = Hashtbl.create 16; funcs = Hashtbl.create 8 }

(* 创建新符号表 *)
let create ?parent () = { current = create_scope (); parent; loop_depth = 0 }

(* 进入新作用域 *)
let enter_scope symtab =
  let new_symtab =
    { current = create_scope (); parent = Some symtab; loop_depth = symtab.loop_depth }
  in
  new_symtab
;;

(* 离开作用域 *)
let leave_scope symtab =
  match symtab.parent with
  | Some parent -> parent
  | None -> failwith "Cannot leave global scope"
;;

(* 变量操作 *)
let add_var symtab name vtype loc =
  if Hashtbl.mem symtab.current.vars name
  then false
  else (
    Hashtbl.add symtab.current.vars name { vtype; initialized = false; defined_loc = loc };
    true)
;;

let update_var symtab name =
  try
    let info = Hashtbl.find symtab.current.vars name in
    info.initialized <- true;
    true
  with
  | Not_found -> false
;;

let rec find_var symtab name =
  try Some (Hashtbl.find symtab.current.vars name) with
  | Not_found ->
    (match symtab.parent with
     | Some parent -> find_var parent name
     | None -> None)
;;

(* 函数操作 *)
let add_func symtab name info =
  if Hashtbl.mem symtab.current.funcs name
  then false
  else (
    Hashtbl.add symtab.current.funcs name info;
    true)
;;

let rec find_func symtab name =
  try Some (Hashtbl.find symtab.current.funcs name) with
  | Not_found ->
    (match symtab.parent with
     | Some parent -> find_func parent name
     | None -> None)
;;

(* 循环深度管理 *)
let enter_loop symtab = symtab.loop_depth <- symtab.loop_depth + 1
let leave_loop symtab = symtab.loop_depth <- symtab.loop_depth - 1
let in_loop symtab = symtab.loop_depth > 0
