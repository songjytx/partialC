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
%token ARRAY
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
%right NOT
%left LBRACKET RBRACKET
%left LPAREN RPAREN

%start program
%type <Ast.program> program

%%

program:
  { ([]) } 
| program func_decl { $2 :: $1 } 

func_decl:
  dtype ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE 
  { { typ = $1;
    fname = $2;
    formals = $4;
    fstmts = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    dtype ID                   { [($1,$2)]     }
  | formal_list COMMA dtype ID { ($3,$4) :: $1 }

vname: 
    ID {Id($1)}

/*array:
  ID LBRACKET INT_L RBRACKET {ArrayIndex(Id($1), IntLit($3))}*/

stmt_list:
    { [] }
  | stmt_list stmt {$2 :: $1}

stmt:
  expr SEMI  {Expr $1}
| dtype ID ASSIGN expr SEMI { VarDecl($1, $2, $4) }
| dtype ID SEMI { VarDecl($1, $2, Noexpr) }
| IF LPAREN expr RPAREN stmt ELSE stmt  { If($3, $5, $7) }
| FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt { For($3, $5, $7, $9) }
| WHILE LPAREN expr RPAREN stmt { While($3, $5) }
| RETURN expr SEMI { Return($2) }
| LBRACE stmt_list RBRACE { Block(List.rev $2)}
| dtype ID LBRACKET INT_L RBRACKET SEMI{ ArrayDecl($1, $2, $4, Noexpr) }


expr:
    LPAREN expr RPAREN { $2 } 
  | expr PLUS  expr { Binop($1, Add, $3) }
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
  | NOT expr           { Not($2) }
  | STRING_L           { StringLit($1) }
  | FLOAT_L           { FloatLit($1) }
  | INT_L             { IntLit($1)  }
  | BOOL_L            { BoolLit($1)  }
  | ID               { Id($1) }
  | LBRACKET array_opt RBRACKET          { ArrayLit(List.rev $2) } 
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | vname ASSIGN expr {AssignOp($1, $3)}
  | ID LBRACKET INT_L RBRACKET ASSIGN expr {ArrayAssignOp(Id($1), IntLit($3), $6)}
  | ID LBRACKET INT_L RBRACKET {ArrayIndex(Id($1), IntLit($3))}

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }

array_opt:
    { [] } 
  | expr { [$1] }
  | array_opt COMMA expr { $3 :: $1 }

dtype:
  | INT { Int }
  | FLOAT { Float }
  | BOOL { Bool }
  | STRING { String }
  | VOID { Void }
  | ARRAY dtype { Array($2) }