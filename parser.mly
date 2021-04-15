%{ open Ast %}

%token LBRACE RBRACE 
%token LPAREN RPAREN 
%token LBRACKET RBRACKET
%token NOT NEGATE 
%token DOT
%token TIMES DIVIDE MOD
%token PLUS MINUS
%token GEQ GT LEQ LT 
%token EQ NEQ 
%token AND OR
%token IF ELSE
%token FOR WHILE RETURN PRINT

%token ASSIGN 
%token SEMI
%token COMMA
%token MAIN
%token TRUE
%token FALSE
%token NULL
%token INT BOOL STRING FLOAT VOID
%token INTARRAY FLOATARRAY BOOLARRAY STRINGARRAY
%token <int> INT_L
%token <float> FLOAT_L
%token <bool> BOOL_L
%token <string> ID STRING_L
%token RETURN
%token EOF

%left SEMI
%right ASSIGN 
%left AND OR
%left EQ NEQ
%left GEQ GT LEQ LT
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT NEGATE
%left LBRACKET RBRACKET
%left LPAREN RPAREN

%start program
%type <Ast.program> program

%%

program:
  { ([]) } 
| program func_decl { $2 :: $1 } 

func_decl:
  dtype ID LPAREN arg_list RPAREN LBRACE stmt_list RBRACE 
  { { typ = $1;
    fname = $2;
    fargs = $4;
    fstmts = $7 } }

arg_list:
  { [] }
| arg_decl arg_list { $1 :: $2 }

arg_decl:
  dtype ID { VarDecl($1, $2, Noexpr) }
| dtype ID COMMA { VarDecl($1, $2, Noexpr) }

/* var_list:
  { [] }
| var_decl var_list { $1 :: $2 } 

var_decl:
  dtype ID ASSIGN expr SEMI { VarDecl($1, $2, $4) } */

stmt_list:
  { [] }
| stmt stmt_list {$1 :: $2}

stmt:
  expr SEMI  {Expr $1}
| dtype ID ASSIGN expr SEMI { VarDecl($1, $2, $4) } 
| dtype ID SEMI { VarDecl($1, $2, Noexpr) }
| IF LPAREN expr RPAREN LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE  { If($3, $6, $10) }
| FOR LPAREN expr SEMI expr SEMI expr RPAREN LBRACE stmt_list RBRACE { For($3, $5, $7, $10) }
| WHILE LPAREN expr RPAREN LBRACE stmt_list RBRACE { While($3, $6) }
| RETURN expr SEMI { Return($2) }


expr:
  expr PLUS  expr { Binop($1, Add, $3) }
| expr MINUS  expr { Binop($1, Sub, $3) }
| expr TIMES  expr { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| expr MOD expr { Binop($1, Mod, $3) }
| expr EQ  expr { Binop($1, Eq, $3) }  
| expr NEQ  expr { Binop($1, Neq, $3) }
| expr GEQ  expr { Binop($1, Geq, $3) }
| expr GT  expr { Binop($1, Gt, $3) }
| expr LEQ  expr { Binop($1, Leq, $3) }
| expr LT  expr { Binop($1, Lt, $3) }
| expr AND  expr { Binop($1, And, $3) }
| expr OR  expr { Binop($1, Or, $3) }
| STRING_L           { StringLit($1) }
| FLOAT_L           { FloatLit($1) }
| INT_L             { IntLit($1)  }
| BOOL_L            { BoolLit($1)  }
| ID               { Id($1) }
| ID LPAREN expr_list RPAREN { Call($1, $3)  }

expr_list:
  expr { [$1] }
| expr COMMA expr_list { $1 :: $3 }

dtype:
    | INT { Int }
    | FLOAT { Float }
    | BOOL { Bool }
    | STRING { String }
    | VOID { Void }