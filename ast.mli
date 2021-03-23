type operator = Add | Sub | Mul | Div | Sep
type assignment = Assign

type expr =
    Binop of expr * operator * expr
  | AssignOp of expr * expr
  | Lit of int
  | Var of string
