type typ = Int | Float | Bool | Void | String
type operator = Add | Sub | Mul | Div | Mod | Sep | Eq | Neq | Lt | Leq | Gt | Geq | And | Or
type assignment = Assign

type expr =
    Binop of expr * operator * expr
  | AssignOp of expr * expr
  | Lit of int
  | Var of string
  | Noexpr
  | StringLit of string
  | FloatLit of float
  | IntLit of int
  | BoolLit of bool
  | Id of string

type args = VarDecl of typ * string * expr

type stmt = 
	  Block of stmt list
	(* | VarDecl of typ * string * expr *)
	| If of expr * stmt list * stmt list
	| For of expr * expr * expr * stmt list
	| While of expr * stmt list
	| Print of expr
	| Return of expr

type func_decl = {
    typ : typ;
    fname : string;
    fargs : args list;
    fstmts : stmt list;
  }

type program = func_decl list

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"
  | String -> "string"

let rec string_of_expr = function
	Noexpr -> ""
	| Lit(i) -> string_of_int i
	| StringLit(s) -> s

let string_of_vdecl = function
	VarDecl(t, id, Noexpr) -> string_of_typ t ^ " " ^ id
  | VarDecl(t, id, e) -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr e

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  |	If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      String.concat ";\n" (List.map string_of_stmt s1)  ^ "else\n" ^ String.concat ";\n" (List.map string_of_stmt s2)
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ String.concat ";\n" (List.map string_of_stmt s)
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ String.concat ";\n" (List.map string_of_stmt s)
  | Print(e) -> "print (" ^ string_of_expr e ^ ")" ^ ";\n"
  | Return(e) -> "return " ^ string_of_expr e


let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_vdecl fdecl.fargs) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.fstmts) ^
  "}\n"

(* = function
 	FuncDecl(t, fname, argslist, stmtlist) -> string_of_typ t ^ " " ^ fname ^ "(" ^ String.concat ", " (List.map snd argslist) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt stmtlist) ^
  "}\n" *)

let string_of_program (funcs) =
  (* String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^ *)
  String.concat "\n" (List.map string_of_fdecl funcs)