{ open Parser }

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| "//" 		{ comment lexbuf }
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
| "print"   { PRINT }
| "int[]"   { INTARRAY }
| "float[]" { FLOATARRAY }
| "bool[]"  { BOOLARRAY }
| "string[]"{ STRINGARRAY }
| "int"		{ INT } 
| "float"  	{ FLOAT }
| "bool"  	{ BOOL }
| "string" 	{ STRING }
| "void"   	{ VOID }
| "true"   	{ TRUE }
| "false"   { FALSE }
| "null"   	{ NULL }
| "if"   	{ IF }
| "else"  	{ ELSE }
| "for"   	{ FOR }
| "while" 	{ WHILE }
| "return" 	{ RETURN }
| ['-']?['0'-'9']+ as lit { INT_L(int_of_string lit) }
| ['-']?['0'-'9']+['.']['0' - '9']+ as lit {FLOAT_L(float_of_string lit)}
| "\'" [^''']+ "\'" as lit { STRING_L(lit) }
| ['a'-'z']+ as lit { ID(lit) }
| eof { EOF }

and comment = parse
  '\n' { tokenize lexbuf }
| _    { comment lexbuf }