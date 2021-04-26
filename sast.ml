open Ast

type sexpr = typ * sx 
and sx = 
    SBinop of sexpr * operator * sexpr
  | SNot of sexpr
  | SAssignOp of sexpr * sexpr
  | SArrayAssignOp of sexpr * sexpr * sexpr
  | SStructAssignOp of sexpr * expr * sexpr
  | SVar of string
  | SStringLit of string
  | SFloatLit of float
  | SIntLit of int
  | SBoolLit of bool
  | SArrayLit of sexpr list
  | SArrayIndex of sexpr * sexpr
  | SStructAccess of sexpr * expr
  | SId of string
  | SCall of string * sexpr list
  | SNoexpr of typ

type sstmt = 
    SBlock of sstmt list
  | SExpr of sexpr
  | SVarDecl of typ * string * sexpr
  | SArrayDecl of typ * string * sexpr * sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sstmt * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SPrint of sexpr
  | SReturn of sexpr


type sstruct_decl = {
    ssname: string;
    smembers: bind list;
  }

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    sfstmts : sstmt list;
  }

type sprogram = sstruct_decl list * sfunc_decl list

(* pretty printing function*)

let rec string_of_sexpr (sex:sexpr) = match snd sex with 
    SNoexpr(t) -> ""
  | SIntLit(i) -> string_of_int i
  | SStringLit(s) -> s
  | SArrayLit(l) -> "[" ^ (String.concat ", " (List.map string_of_sexpr l)) ^ "]"
  | SArrayIndex(v, i) -> string_of_sexpr v ^ "[" ^ string_of_sexpr i ^ "]"
  | SCall(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SId(s) -> s
  | SAssignOp(v, e) -> string_of_sexpr v ^ " = " ^ string_of_sexpr e
  | SArrayAssignOp(v, i, e) -> string_of_sexpr v ^  "[" ^ string_of_sexpr i ^ "]"^" = " ^ string_of_sexpr e
  | _ -> "NOT FOUND"

let string_of_svdecl = function
    VarDecl(t, id, Noexpr(ty)) -> string_of_typ t ^ " " ^ id
  | VarDecl(t, id, e) -> string_of_typ t ^ " " ^ id ^ " = "


let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  (* | SVarDecl(t, s1, SNoexpr) -> string_of_typ t ^" " ^s1 ^ ";\n" *)
  | SVarDecl(t, s1, e1) -> string_of_typ t ^" " ^s1 ^ " = " ^ string_of_sexpr e1 ^ ";\n"
  | SArrayDecl(t, v, e1, e) -> string_of_typ t ^ " " ^ v ^  "[" ^ string_of_sexpr e1 ^ "];\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sstmt e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^ string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SReturn(e) -> "return " ^ string_of_sexpr e

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sfstmts) ^
  "}\n"

let string_of_sstructs sdecl = 
  "struct " ^ sdecl.ssname ^ "{\n" ^
  String.concat "\n" (List.map snd sdecl.smembers) ^
  "\n};\n"

let string_of_sprogram (structs, funcs) =
  String.concat "" (List.map string_of_sstructs structs) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)