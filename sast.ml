open Ast

type sexpr = typ* sx 
and sx = 
    SBinop of sexpr * operator * sexpr
  | SAssignOp of string * sexpr
  | SLit of int
  | SVar of string
  | SNoexpr
  | SStringLit of string
  | SFloatLit of float
  | SIntLit of int
  | SBoolLit of bool
  | SId of string
  | SCall of string * sexpr list

type sstmt = 
    SBlock of sstmt list
  | SExpr of sexpr
  | SVarDecl of typ * string * sx
  | SIf of sexpr * sstmt list * sstmt list
  | SFor of sexpr * sexpr * sexpr * sstmt list
  | SWhile of sexpr * sstmt list
  | SPrint of sexpr
  | SReturn of sexpr


(* do not create SVarDecl*)

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    sfstmts : sstmt list;
  }

type sprogram = sfunc_decl list

(* pretty printing function*)

let rec string_of_sexpr (sex:sexpr) = match snd sex with 
  SNoexpr -> ""
  | SLit(i) -> string_of_int i
  | SStringLit(s) -> s
  | SCall(f, el) ->
   f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"


let string_of_svdecl = function
  VarDecl(t, id, Noexpr) -> string_of_typ t ^ " " ^ id
  | VarDecl(t, id, e) -> string_of_typ t ^ " " ^ id ^ " = "
(*Mingjie, please fix bug here*)

let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      String.concat ";\n" (List.map string_of_sstmt s1)  ^ "else\n" ^ String.concat ";\n" (List.map string_of_sstmt s2)
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ String.concat ";\n" (List.map string_of_sstmt s)
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ String.concat ";\n" (List.map string_of_sstmt s)
  | SReturn(e) -> "return " ^ string_of_sexpr e


let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sfstmts) ^
  "}\n"

(* = function
  FuncDecl(t, fname, argslist, stmtlist) -> string_of_typ t ^ " " ^ fname ^ "(" ^ String.concat ", " (List.map snd argslist) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt stmtlist) ^
  "}\n" *)

let string_of_sprogram (funcs) =
  (* String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^ *)
  String.concat "\n" (List.map string_of_sfdecl funcs)