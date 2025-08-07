(* {
  open Parser (* 引入 parser.mly 定义的 token 类型 *)
  exception Error of string * int * int
  (*let errors : lexer_error list ref = ref []*) (* 错误列表 *)
}

rule token = parse
  | [' ' '\t' '\n' '\r']     { token lexbuf }  (* 跳过空白 *)
  | "//" [^ '\n']*      { token lexbuf }  (* 单行注释 *)
  | "/*" ( _ | '\n')*? "*/" { token lexbuf } (* 多行注释 *)
  
  (* 关键字 *)
  | "int"    { INT }
  | "void"   { VOID }
  | "if"     { IF }
  | "else"   { ELSE }
  | "while"  { WHILE }
  | "break"  { BREAK }
  | "continue" { CONTINUE }
  | "return" { RETURN }
  
  (* 标识符 *)
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { ID(id) }
  
  (* 整数 *)
  | '-'? ['0'-'9']+ as num { NUM(int_of_string num) }
  
  (* 运算符 *)
  | '='    { ASSIGN }
  | "=="   { EQ }
  | "!="   { NEQ }
  | '<'    { LT }
  | "<="   { LE }
  | '>'    { GT }
  | ">="   { GE }
  | "&&"   { AND }
  | "||"   { OR }
  | '+'    { PLUS }
  | '-'    { MINUS }
  | '*'    { TIMES }
  | '/'    { DIV }
  | '%'    { MOD }
  | '!'    { NOT }
  
  (* 分隔符 *)
  | '('    { LPAREN }
  | ')'    { RPAREN }
  | '{'    { LBRACE }
  | '}'    { RBRACE }
  | ';'    { SEMI }
  | ','    { COMMA }
  
  | eof    { EOF }
  | _ as c {
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.Lexing.pos_lnum in
      let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      raise (Error (Printf.sprintf "非法字符 '%c'" c, line, column))
  }
  (* 处理未匹配的字符，抛出异常 *)
  (*待处理*)
 (*  | _ as c {
    let pos = lexbuf.lex_curr_p in
    let line = pos.pos_lnum in
    let column = pos.pos_cnum - pos.pos_bol + 1 in
    let error = { 
      message = Printf.sprintf "Unexpected character '%c'" c;
      line;
      column;
      char = Some c
    } in
    errors := error :: !errors; (* 将错误添加到错误列表 *)

    token lexbuf (* 继续解析下一个 token *)
  } *)
 *)
 {
  open Parser (* 引入 parser.mly 定义的 token 类型 *)
  exception Error of string * int * int
  (*let errors : lexer_error list ref = ref []*) (* 错误列表 *)
}

rule token = parse
(*   | [' ' '\t' '\n' '\r']     { token lexbuf }  (* 跳过空白 *)
  | "//" [^ '\n']*      { token lexbuf }  (* 单行注释 *)
  | "/*" ( _ | '\n')*? "*/" { token lexbuf } (* 多行注释 *) *)
  | [' ' '\t']     { token lexbuf }  (* 跳过空白 *)
  | '\n'           { Lexing.new_line lexbuf; token lexbuf }  (* 处理换行符 *)
  | '\r'           { token lexbuf }  (* 单独处理回车符 *)
  | "//" [^ '\n']* { token lexbuf }  (* 单行注释 *)
  | "/*"           { comment lexbuf } (* 多行注释 *)


  (* 关键字 *)
  | "int"    { INT }
  | "void"   { VOID }
  | "if"     { IF }
  | "else"   { ELSE }
  | "while"  { WHILE }
  | "break"  { BREAK }
  | "continue" { CONTINUE }
  | "return" { RETURN }
  
  (* 标识符 *)
  | ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id { ID(id) }
  
  (* 整数 *)
  | '-'? ['0'-'9']+ as num { NUM(int_of_string num) }
  
  (* 运算符 *)
  | '='    { ASSIGN }
  | "=="   { EQ }
  | "!="   { NEQ }
  | '<'    { LT }
  | "<="   { LE }
  | '>'    { GT }
  | ">="   { GE }
  | "&&"   { AND }
  | "||"   { OR }
  | '+'    { PLUS }
  | '-'    { MINUS }
  | '*'    { TIMES }
  | '/'    { DIV }
  | '%'    { MOD }
  | '!'    { NOT }
  
  (* 分隔符 *)
  | '('    { LPAREN }
  | ')'    { RPAREN }
  | '{'    { LBRACE }
  | '}'    { RBRACE }
  | ';'    { SEMI }
  | ','    { COMMA }
  
  | eof    { EOF }
  | _ as c {
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.Lexing.pos_lnum in
      let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
      raise (Error (Printf.sprintf "非法字符 '%c'" c, line, column))
  }
  (* 处理未匹配的字符，抛出异常 *)
  (*待处理*)
 (*  | _ as c {
    let pos = lexbuf.lex_curr_p in
    let line = pos.pos_lnum in
    let column = pos.pos_cnum - pos.pos_bol + 1 in
    let error = { 
      message = Printf.sprintf "Unexpected character '%c'" c;
      line;
      column;
      char = Some c
    } in
    errors := error :: !errors; (* 将错误添加到错误列表 *)

    token lexbuf (* 继续解析下一个 token *)
  } *)
and comment = parse
  | "*/" { token lexbuf }  (* 结束注释 *)
  | '\n' { Lexing.new_line lexbuf; comment lexbuf }  (* 注释中的换行 *)
  | _    { comment lexbuf }  (* 其他注释内容 *)
  | eof  { 
      let pos = Lexing.lexeme_start_p lexbuf in
      let line = pos.Lexing.pos_lnum in
      let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
      raise (Error ("未终止的注释", line, column)) 
  }