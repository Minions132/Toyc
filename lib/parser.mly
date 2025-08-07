(*%{
  open Ast (* 引入自定义AST类型 *)
  
%}

/* Token 声明 (与 lexer.mll 对应) */
%token INT VOID IF ELSE WHILE BREAK CONTINUE RETURN
%token <string> ID 
%token <int>NUM
%token ASSIGN EQ NEQ LT LE GT GE AND OR PLUS MINUS TIMES DIV MOD NOT
%token LPAREN RPAREN LBRACE RBRACE SEMI COMMA
%token EOF

/* 优先级和结合性 */

%nonassoc IFNOELSE  (* IF 的优先级 *)
%nonassoc ELSE
%left OR
%left AND
%nonassoc EQ NEQ
%nonassoc LT LE GT GE
%left PLUS MINUS
%left TIMES DIV MOD
%right NOT R_PLUS R_MINUS(*右结合优先级更高*)

/* 起始符号 */
%start program
%type <Ast.program> program

%%

program:
  | func_defs EOF { $1 }

func_defs:
  | func_defs func_def { $1 @ [$2] }
  | func_def          { [$1] }

func_def:
  | typ ID LPAREN params RPAREN block
    { { rtyp = $1; fname = $2; params = $4; body = $6; } }

typ:
  | INT   { TInt }
  | VOID  { TVoid }

params:
  | param_list   { $1 }
  | /* empty */  { [] }

param_list:
  | param               { [$1] }
  | param_list COMMA param { $1 @ [$3] }

param:
  | INT ID { ($2, TInt) }

block:
  | LBRACE stmt_list RBRACE { $2 }

stmt_list:
  | stmt_list stmt    { $1 @ [$2] }
  | stmt             { [$1] }
  | /* empty */      { [] }

/* 语句规则 */
stmt:
  | block                        { Block $1 }
  | SEMI                         { Empty } (* 空语句 *)
  | expr SEMI                    { ExprStmt($1) }
  | ID ASSIGN expr SEMI          { Assign($1, $3) }
  | INT ID SEMI                  { VarDecl($2, None) }(*未初始化*)
  | INT ID ASSIGN expr SEMI      { VarDecl($2, Some $4) }(*可能没有初始化*)
  | IF LPAREN expr RPAREN stmt ELSE stmt { If($3, $5, Some $7) }
  | IF LPAREN expr RPAREN stmt %prec IFNOELSE { If($3, $5, None) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | BREAK SEMI                   { Break }
  | CONTINUE SEMI                { Continue }
  | RETURN SEMI                  { Return None}
  | RETURN expr SEMI             { Return (Some $2) }

/* 表达式规则 */
expr:
  | ID LPAREN args RPAREN   { Call($1, $3) }
  | ID                      { Var($1) }
  | NUM                     { IntLit($1) }
  | LPAREN expr RPAREN      { $2 }
  | expr binop expr         { BinOp($1, $2, $3) }
  | NOT expr            { UnOp(Not, $2) } 
  | MINUS expr          { UnOp(Minus, $2) } %prec R_MINUS(* 数值取负 *)
  | PLUS expr           { UnOp(Plus, $2) } %prec R_PLUS(* 一元加法，通常不需要，但可以用于表达式的统一处理 *)
           
binop:
  | OR     { Or }
  | AND    { And }
  | EQ     { Eq }
  | NEQ    { Neq }
  | LT     { Lt }
  | LE     { Le }
  | GT     { Gt }
  | GE     { Ge }
  | PLUS   { Plus }
  | MINUS  { Minus }
  | TIMES  { Times }
  | DIV    { Div }
  | MOD    { Mod }
   
args:
  | expr_list   { $1 }
  | /* empty */ { [] }

expr_list:
  | expr                { [$1] }
  | expr_list COMMA expr { $1 @ [$3] }*)


%{
  open Ast (* 引入自定义AST类型 *)
  let create_loc start_pos = {
    line = start_pos.Lexing.pos_lnum;
    column = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol + 1;
  }
%}

/* Token 声明 (与 lexer.mll 对应) */
%token INT VOID IF ELSE WHILE BREAK CONTINUE RETURN
%token <string> ID 
%token <int>NUM
%token ASSIGN EQ NEQ LT LE GT GE AND OR PLUS MINUS TIMES DIV MOD NOT
%token LPAREN RPAREN LBRACE RBRACE SEMI COMMA
%token EOF

/* 优先级和结合性 */

%nonassoc IFNOELSE  (* IF 的优先级 *)
%nonassoc ELSE
%left OR
%left AND
%nonassoc EQ NEQ
%nonassoc LT LE GT GE
%left PLUS MINUS
%left TIMES DIV MOD
%right NOT R_PLUS R_MINUS(*右结合优先级更高*)

/* 起始符号 */
%start program
%type <Ast.program> program

%%

program:
  | func_defs EOF { $1 }

func_defs:
  | func_defs func_def { $1 @ [$2] }
  | func_def          { [$1] }

func_def:
  | typ ID LPAREN params RPAREN block
    { 
      let loc = create_loc $startpos in
      { rtyp = $1; 
        fname = $2;
        params = $4; 
        body = $6; 
        func_loc = loc
      } 
    }

typ:
  | INT   { TInt }
  | VOID  { TVoid }

params:
  | param_list   { $1 }
  | /* empty */  { [] }

param_list:
  | param               { [$1] }
  | param_list COMMA param { $1 @ [$3] }

param:
  | INT ID { ($2, TInt) }

block:
  | LBRACE stmt_list RBRACE { $2 }

stmt_list:
  | stmt_list stmt    { $1 @ [$2] }
  | stmt             { [$1] }
  | /* empty */      { [] }

/* 语句规则 */
stmt:
  | block                        
  {
    let loc = create_loc $startpos in
    { 
      stmt_desc = Block $1;
      stmt_loc = loc
    }
  }
  | SEMI (* 空语句 *)
  {
    let loc = create_loc $startpos in
    { 
      stmt_desc = Empty;
      stmt_loc = loc
    }
  }
  | expr SEMI 
  {
    let loc = create_loc $startpos in
    { 
      stmt_desc = ExprStmt($1);
      stmt_loc = loc
    }
  }
  | ID ASSIGN expr SEMI
  {
    let loc = create_loc $startpos in
    { 
      stmt_desc = Assign($1, $3);
      stmt_loc = loc
    }
  }
  | INT ID SEMI (*未初始化*)
  {
    let loc = create_loc $startpos in
    { 
      stmt_desc = VarDecl($2, None);
      stmt_loc = loc
    }
  }
  | INT ID ASSIGN expr SEMI (*可能没有初始化*)
  {
    let loc = create_loc $startpos in
    { 
      stmt_desc =VarDecl($2, Some $4);
      stmt_loc = loc
    }
  }
  | IF LPAREN expr RPAREN stmt ELSE stmt
  {
    let loc = create_loc $startpos in
    { 
      stmt_desc = If($3, $5, Some $7);
      stmt_loc = loc
    }
  }
  | IF LPAREN expr RPAREN stmt %prec IFNOELSE
  {
    let loc = create_loc $startpos in
    { 
      stmt_desc = If($3, $5, None);
      stmt_loc = loc
    }
  }
  | WHILE LPAREN expr RPAREN stmt
  {
    let loc = create_loc $startpos in
    { 
      stmt_desc = While($3, $5);
      stmt_loc = loc
    }
  }
  | BREAK SEMI                  
  {
    let loc = create_loc $startpos in
    { 
      stmt_desc = Break;
      stmt_loc = loc
    }
  }
  | CONTINUE SEMI                
  {
    let loc = create_loc $startpos in
    { 
      stmt_desc = Continue;
      stmt_loc = loc
    }
  }
  | RETURN SEMI               
  {
    let loc = create_loc $startpos in
    { 
      stmt_desc = Return None;
      stmt_loc = loc
    }
  }
  | RETURN expr SEMI          
  {
    let loc = create_loc $startpos in
    { 
      stmt_desc = Return (Some $2);
      stmt_loc = loc
    }
  }

/* 表达式规则 */
expr:
  | ID LPAREN args RPAREN   
    {
      let loc = create_loc $startpos in 
      { 
        expr_desc = Call($1, $3);
        expr_loc = loc 
      }
    }
  | ID                      
    {
      let loc = create_loc $startpos in
      { 
        expr_desc = Var($1);
        expr_loc = loc
      }
    }
  | NUM                  
    {
      let loc = create_loc $startpos in
      { 
        expr_desc = IntLit($1);
        expr_loc = loc
      }
    }
  | LPAREN expr RPAREN     
    {
      let loc = create_loc $startpos in
      { 
        expr_desc = $2.expr_desc;
        expr_loc = loc
      }
    }
  | expr binop expr        
    {
      let loc = create_loc $startpos in
      { 
        expr_desc = BinOp($1, $2, $3);
        expr_loc = loc
      }
    }
  | NOT expr        
    {
      let loc = create_loc $startpos in
      { 
        expr_desc = UnOp(Not, $2);
        expr_loc = loc
      }
    } 
  | MINUS expr %prec R_MINUS        
  { 
    let loc = create_loc $startpos in
    {
      expr_desc = UnOp(Minus, $2);
      expr_loc = loc
    }(* 数值取负 *)
  }
  | PLUS expr %prec R_PLUS(* 一元加法，通常不需要，但可以用于表达式的统一处理 *)
  { 
    let loc = create_loc $startpos in
    {
      expr_desc = UnOp(Plus, $2);
      expr_loc = loc
    } (* 数值取负 *)
  }
       
binop:
  | OR     { Or }
  | AND    { And }
  | EQ     { Eq }
  | NEQ    { Neq }
  | LT     { Lt }
  | LE     { Le }
  | GT     { Gt }
  | GE     { Ge }
  | PLUS   { Plus }
  | MINUS  { Minus }
  | TIMES  { Times }
  | DIV    { Div }
  | MOD    { Mod }
   
args:
  | expr_list   { $1 }
  | /* empty */ { [] }

expr_list:
  | expr                { [$1] }
  | expr_list COMMA expr { $1 @ [$3] }
