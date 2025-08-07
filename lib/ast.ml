(* (* {
  params: param list;
  body: stmt;
} *)
(* (需要添加具体位置信息) *)
type typ =
  | TInt
  | TVoid

type binop =
  | Plus
  | Minus
  | Times
  | Div
  | Mod
  | Eq
  | Neq
  | Lt (*小于*)
  | Le (*小于等于*)
  | Gt (*大于*)
  | Ge (*大于等于*)
  | And
  | Or

type unop =
  | Not (*取反*)(*逻辑非*)
  | Minus (*数值取负*)
  | Plus (*一元加法*) (*一元加法通常不需要，但可以用于表达式的统一处理*)
  (* 注意：unop中的Minus和Plus是数值取负和一元加法，和二元运算符中的Minus和Plus不同 *)
  (*plus也可以当作取正符号*)

type expr =
  | IntLit of int (*整数*)
  | Var of string (*标识符*)
  | BinOp of expr * binop * expr (*二元表达式*)
  | UnOp of unop * expr (*一元表达式*)
  | Call of string * expr list (*函数调用*)

type stmt =
  | Block of stmt list (*块语句：{ ... }*)
  | Empty (*空语句：;*)
  | ExprStmt of expr (*表达式语句*)
  | VarDecl of string * expr option(* 变量声明 int x = ... *)(* 声明时没有初始化 *)
  | Assign of string * expr (* x = ... *) (*赋值*)
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | Break
  | Continue
  | Return of expr option (*return (带返回值选项)*)

  (*函数参数*)
type param = string * typ (* 参数名和类型 *)

(*函数*)
type func_def =
  { rtyp : typ (*返回类型*); 
  fname : string (*函数名*); 
  params : param list (*参数列表*); 
  body : stmt list(*函数体*)
  }

type program = func_def list (* 程序是函数定义的列表 *)

type lexer_error = 
{ 
  message: string; 
  line:int;(* 错误发生的位置的行 *)
  column: int;(* 错误发生的位置的列 *)
  char: char option; 
}
*)

(* {
  params: param list;
  body: stmt;
} *)

(*位置类型信息*)
type loc =
  { line : int
  ; column : int
  }

type typ =
  | TInt
  | TVoid

type binop =
  | Plus
  | Minus
  | Times
  | Div
  | Mod
  | Eq
  | Neq
  | Lt (*小于*)
  | Le (*小于等于*)
  | Gt (*大于*)
  | Ge (*大于等于*)
  | And
  | Or

type unop =
  | Not
  (*取反*)
  (*逻辑非*)
  | Minus (*数值取负*)
  | Plus
(*一元加法*)
(*一元加法通常不需要，但可以用于表达式的统一处理*)

(* 注意：unop中的Minus和Plus是数值取负和一元加法，和二元运算符中的Minus和Plus不同 *)
(*plus也可以当作取正符号*)

type expr =
  { expr_desc : expr_desc
  ; expr_loc : loc
  }

and expr_desc =
  | IntLit of int (*整数*)
  | Var of string (*标识符*)
  | BinOp of expr * binop * expr (*二元表达式*)
  | UnOp of unop * expr (*一元表达式*)
  | Call of string * expr list (*函数调用*)

type stmt =
  { stmt_desc : stmt_desc
  ; stmt_loc : loc
  }

and stmt_desc =
  | Block of stmt list (*块语句：{ ... }*)
  | Empty (*空语句：;*)
  | ExprStmt of expr (*表达式语句*)
  | VarDecl of string * expr option
  (* 变量声明 int x = ... *)
  (* 声明时没有初始化 *)
  | Assign of string * expr
  (* x = ... *)
  (*赋值*)
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | Break
  | Continue
  | Return of expr option (*return (带返回值选项)*)

(*函数参数*)
type param = string * typ (* 参数名和类型 *)

(*函数*)
type func_def =
  { rtyp : typ (*返回类型*)
  ; fname : string (*函数名*)
  ; params : param list (*参数列表*)
  ; body : stmt list (*函数体*)
  ; func_loc : loc
  }

type program = func_def list (* 程序是函数定义的列表 *)
(* ast.ml *)

(* ... 你现有的 type 定义 ... *)

(* --- 在这里添加以下内容 --- *)

(* 辅助函数：将二元操作符转换为字符串 *)
(* 辅助函数：将一元操作符转换为字符串 *)
let string_of_unop = function
  | Not -> "!"
  | Minus -> "-"
  | Plus -> "+"
;;
let string_of_binop(op:binop) = 
match op with 
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
;;

