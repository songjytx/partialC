open Ast

type sexpr = typ* sx 
and sx = 
    SLiteral of int
  | SFliteral of float
  | SBoolLit of bool
  | SStringLit of string
  | SId of string
  | SBinop of sexpr * operator * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SNoexpr

type stmt = 
    SBlock of stmt list
  | SVarDecl of typ * string * sexpr
  | SIf of sexpr * stmt list * stmt list
  | SFor of sexpr * sexpr * sexpr * stmt list
  | SWhile of sexpr * stmt list
  | SPrint of sexpr
  | SReturn of sexpr

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sfargs : args list;
    sfstmts : stmt list;
  }

type sprogram = sfunc_decl list