(* error.ml *)
type t =
  { msg : string
  ; line : int
  ; column : int
  }

let create msg line column = { msg; line; column }
let to_string err = Printf.sprintf "[行%d,列%d] %s" err.line err.column err.msg
let print_error err = Printf.eprintf "错误: %s\n" (to_string err)
