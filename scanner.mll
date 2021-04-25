{ open Parser }
let digit = ['0'-'9']

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| "//" 		{ comment lexbuf }
| '.'	    { DOT }
| '+'  		{ PLUS }
| '-'  		{ MINUS }
| '*'  		{ TIMES }
| '/'  		{ DIVIDE }
| '%'  		{ MOD }
| '>'  		{ GT }
| ">=" 		{ GEQ }
| '<'  		{ LT }
| "<="		{ LEQ }
| "==" 		{ EQ }
| "!=" 		{ NEQ }
| "&&" 		{ AND }
| "||" 		{ OR }
| '!'  		{ NOT }
| '='  		{ ASSIGN }
| ';'  		{ SEMI }
| ','  		{ COMMA }
| '{'  		{ LBRACE }
| '}'  		{ RBRACE }
| '('  		{ LPAREN }
| ')'  		{ RPAREN }
| '['  		{ LBRACKET }
| ']'  		{ RBRACKET }
| "int"		{ INT } 
| "float"  	{ FLOAT }
| "bool"  	{ BOOL }
| "string" 	{ STRING }
| "void"   	{ VOID }
| "arr"     { ARRAY}
| "true"   	{ BOOL_L(true) }
| "false"   { BOOL_L(false) }
| "null"   	{ NULL }
| "struct"  { STRUCT }
| "if"   	{ IF }
| "else"  	{ ELSE }
| "for"   	{ FOR }
| "while" 	{ WHILE }
| "return" 	{ RETURN }
| ['-']?digit+ as lxm { INT_L(int_of_string lxm) }
| ['-']?digit+['.']digit+ as lxm {FLOAT_L(float_of_string lxm)}
| "\'" [^''']+ "\'" as lxm { STRING_L(lxm) }
| ['a'-'z' '_']+ as lxm { ID(lxm) }
| ['A'-'Z']['a'-'z' 'A'-'Z']* as structLit { STRUCT_ID(structLit) }
| eof { EOF }
| _ as ch { raise (Failure("illegal character detected " ^ Char.escaped ch)) }

and comment = parse
  '\n' { tokenize lexbuf }
| _    { comment lexbuf }