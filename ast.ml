type typ = Int | Float | Bool | Void | String | Array of typ |  Struct of string 
type operator = Add | Sub | Mul | Div | Mod | Sep | Eq | Neq | Lt | Leq | Gt | Geq | And | Or
type assignment = Assign

type expr =

    Binop of expr * operator * expr
  | Not of expr
  | AssignOp of expr * expr
  | ArrayAssignOp of expr * expr * expr
  | Var of string
  | StringLit of string
  | FloatLit of float
  | IntLit of int
  | BoolLit of bool
  | Id of string
  | Call of string * expr list
  | ArrayLit of expr list
  | ArrayIndex of expr * expr
  | Noexpr of typ

type bind = typ * string

type stmt = 
	  Block of stmt list
	| VarDecl of typ * string * expr
  | ArrayDecl of typ * string * expr * expr
	| If of expr * stmt * stmt
	| For of stmt * expr * expr * stmt
	| While of expr * stmt
	| Print of expr
	| Return of expr
  | Expr of expr

type struct_decl = {
    sname: string;
    members: stmt list;
  }

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    fstmts : stmt list;
  }

type program = struct_decl list * func_decl list

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"
  | String -> "string"
  | Array(t) -> string_of_typ(t) ^ " array"
  | Struct(t) -> t
  

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let rec string_of_expr = function
	Noexpr(t) -> ""
	| IntLit(i) -> string_of_int i
	| StringLit(s) -> s
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | FloatLit(f) -> string_of_float f
	| BoolLit(true) -> "true"
	| BoolLit(false) -> "false"
	| Id(s) -> s
  | AssignOp(v, e) -> string_of_expr v ^ " = " ^ string_of_expr e
  | ArrayAssignOp(v, i, e) -> string_of_expr v ^  "[" ^ string_of_expr i ^ "]"^" = " ^ string_of_expr e
  | ArrayLit(l) -> "[" ^ (String.concat ", " (List.map string_of_expr l)) ^ "]"
  | ArrayIndex(v, i) -> string_of_expr v ^ "[" ^ string_of_expr i ^ "]"
  | _ -> "no expression matched*******"

let string_of_vdecl = function
	  VarDecl(t, id, Noexpr(ty)) -> string_of_typ t ^ " " ^ id
  | VarDecl(t, id, e) -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr e

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | VarDecl(t, s1, Noexpr(ty)) ->  string_of_typ t ^" " ^s1 ^ ";\n" 
  | VarDecl(t, s1, e1) -> string_of_typ t ^" " ^s1 ^ " = " ^ string_of_expr e1 ^ ";\n"
  | ArrayDecl(t, v, e, Noexpr(ty)) -> string_of_typ t ^ " " ^ v ^  "[" ^ string_of_expr e ^ "];\n"
  |	If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s1  ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_stmt e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^ string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Return(e) -> "return " ^ string_of_expr e
  | _ -> "Statement Not Matched??"


let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.fstmts) ^
  "}\n"

let string_of_structs sdecl = 
  "struct " ^ sdecl.sname ^ "{\n" ^
  String.concat "" (List.map string_of_stmt sdecl.members) ^
  "};\n"

let string_of_program (structs, funcs) =
  String.concat "" (List.map string_of_structs structs) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)