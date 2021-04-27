(* Semantic checking for the PartialC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

let check (structs, functions) =
  let add_struct map sd = 
    let dup_err = "struct dup error" 
    and make_err er = raise (Failure er)
    and n = sd.sname
    in match sd with
      _ when StringMap.mem sd.sname map -> make_err dup_err
    | _ -> StringMap.add n sd map
  in
  let check_struct struc =
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name (ty, name, 0) m) StringMap.empty struc.members
    in
      {
        ssname = struc.sname;
        smembers = struc.members;
    }
  in
  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ "may not be redefined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname 
    in match fd with 
         _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in
  (*build in fucntions*)
  let built_in_funcs = List.fold_left add_func StringMap.empty [
      {typ = Void; fname = "prints"; formals = [(String, "args")];  fstmts = [] };
      {typ = Void; fname = "printi"; formals = [(Int, "args")];  fstmts = [] };
      {typ = Void; fname = "printf"; formals = [(Float, "args")];  fstmts = [] };
      {typ = Int;  fname = "sizeof"; formals = [(Array(Int), "args")]; fstmts = [] }
      ] 
  in
  (* Collect all other function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_funcs functions
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in
  let _ = find_func "main" in (* Ensure "main" is defined *)
  let check_function func =
    let add_var map (tp, name, len) = 
        let dup_err = "Variable with name " ^ name ^" is a duplicate." in
        match (tp, name) with
          _ when StringMap.mem name map -> raise (Failure dup_err)
        | _ -> StringMap.add name (tp, name, len) map
    in
    
    let find_var map name =
        try StringMap.find name map
        with Not_found -> raise( Failure("Undeclared variable: " ^ name))
    in
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in 
    let type_of_identifier s symbols =  
      let (ty, _, _) = try StringMap.find s symbols with Not_found -> raise( Failure("ID not found: " ^ s)) 
    in ty in
    let rec check_expr map e = match e with
        IntLit  l -> (Int, SIntLit l, map)
      | FloatLit l -> (Float, SFloatLit l, map)
      | BoolLit l  -> (Bool, SBoolLit l, map)
      | StringLit l -> (String, SStringLit l, map)
      | ArrayLit(l) ->
        let array_body = List.map (check_expr map) l in 
        let array_type, _, _ = List.nth array_body 0 in
            (Array array_type, SArrayLit(List.map (fun (t, sx, _) -> (t,sx)) array_body), map)
      | ArrayIndex(name, idx) ->
        let stringName = match name with
            Id i -> i
          | _ -> raise(Failure("Invalid identifier for array: " ^ string_of_expr name)) in
        let (typ, sid, map1) = check_expr map name
        in
        let (idx_type, sindex, map2) = check_expr map1 idx in
        let _ = match sindex with 
          SIntLit l ->
            let (_, _, size) = StringMap.find stringName map in 
            if l >= size && size != 0 then raise(Failure("Array Index out ouf bound: " ^ string_of_int l)) 
            else l
          | _ -> 0
        in 
        let element_type = match typ with
            Array(t) -> t
          | _ -> raise(Failure("Type is not expected: " ^ string_of_typ typ))
        in
        (element_type, SArrayIndex((typ, sid), (idx_type, sindex)), map2)

      | StructAccess(v, m) -> 
        let stringName = match v with
            Id i -> i
          | _ -> raise(Failure("Invalid identifier for struct: " ^ string_of_expr v)) in  
        let lt, vname, map1 = find_name v map "assignment error" in
      (Int, SStructAccess((lt, vname), m), map1)
      | Noexpr(ty) -> (ty, SNoexpr(ty), map)
      | Id s       -> (type_of_identifier s map, SId s, map)
      | AssignOp(v, e)-> 
        let lt, vname, map1 = find_name v map "assignment error" in
        let rt, ex, map2 = check_expr map1 e in
        (check_assign lt rt "type miss match", SAssignOp((lt, vname), (rt, ex)), map2)

      | ArrayAssignOp(v, i, e)-> 
        let stringName = match v with
            Id i -> i
          | _ -> raise(Failure("Invalid identifier for array: " ^ string_of_expr v)) in
        let lt, vname, map1 = find_name v map "assignment error" in
        let rt, ex, map2 = check_expr map1 e in
        let it, ix, map3 = check_expr map2 i in
        let (idx_type, sindex, _) = check_expr map i in
          let _ = match sindex with 
            SIntLit l -> 
              let (_, _, size) = StringMap.find stringName map in 
              if l >= size && size != 0 then raise(Failure("Array Index out ouf bound: " ^ string_of_int l)) 
              else l
            | _ -> 0
          in 
        let element_type = (match lt with
            Array(t) -> t
            | _ -> raise (Failure ("got " ^ string_of_typ lt))
            )
        in
        (check_assign element_type rt "array type miss match", SArrayAssignOp((lt, vname), (it, ix),(rt, ex)), map3)

      | StructAssignOp(v, m, e)->
        let stringName = match v with
            Id i -> i
          | _ -> raise(Failure("Invalid identifier for array: " ^ string_of_expr v)) in  
        let lt, vname, map1 = find_name v map "assignment error" in
        let rt, ex, map2 = check_expr map1 e in
      (check_assign Ast.Int Ast.Int "array type miss match", SStructAssignOp((lt, vname), m, (rt, ex)), map2)

      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e', map') = check_expr map e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft ft err, e')
            (* Hack for struct *)
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'), map)

      | Not(e) as notEx-> let (t, e', map') = check_expr map e in
        if t != Bool then 
          raise (Failure ("expecting bool expression in " ^ string_of_expr notEx))
        else (Bool, SNot((t, e')), map')
      | Binop(e1, op, e2) as ex ->
        let (t1, e1', map') = check_expr map e1  
        in let (t2, e2', map'') = check_expr map' e2 
        in
        let same = t1 = t2 in
        let ty = 
        match t1 with
        (* | ArrayList inner -> (match op with
                  Add -> t1
                  | _ -> make_err ("Illegal binary operation, cannot perform "^string_of_expr ex^" on lists.")) *)
        _ -> match op with
                Add | Sub | Mul | Div | Mod    when same && t1 = Int   -> Int
              | Add | Sub | Mul | Div    when same && t1 = Float -> Float
              | Add                      when same && t1 = String -> String
              | Eq | Neq                 when same               -> Bool
              | Lt | Leq | Gt | Geq      when same && (t1 = Int || t1 = Float) -> Bool
              | And | Or                 when same && t1 = Bool -> Bool
              | _ -> raise (Failure ("Illegal binary operator " ^ string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^ string_of_typ t2 ^ " in " ^ string_of_expr ex))
        in (ty, SBinop((t1, e1'), op, (t2, e2')), map'')

    and find_name (name) map err = match name with
        Id _ -> check_expr map name
        | _ -> raise (Failure ("find name error"))
    in
    let check_bool_expr map e = 
      let (t', e', map') = check_expr map e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if Bool != Bool then raise (Failure err) else (t', e') 
      (* Hack for struct *)
    in
    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt map st = match st with
        Expr e -> let (ty, sexpr, new_map) = check_expr map e in (SExpr (ty, sexpr), new_map)
      | VarDecl(tp, id, e) ->
        let (right_ty, sexpr, map') = check_expr map e  in
        let err = "illegal argument found." in
        let len = match e with
            Ast.ArrayLit t ->  List.length t 
          | _ -> 0 in
        let new_map = add_var map' (tp, id, len) in
        let right = (right_ty, sexpr) in
        (SVarDecl(tp, id, right), new_map)
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
      | ArrayDecl(t, id, e1, e) ->
        let (ty', e1', _) = check_expr map e1 in
          if ty' != Ast.Int then raise ( Failure ("Integer is expected instead of " ^ string_of_typ t))
        else 
          let len = match e1 with
            Ast.IntLit t -> t
          in
          let new_map = add_var map (t, id, len) in
          let (t2, sx2, map') = check_expr map e in
          let r2 = (t2, sx2) in
          (SArrayDecl(t, id, (ty', e1'), r2), new_map)
      | Return e -> let (t, e', map') = check_expr map e in
        if t = func.typ then (SReturn (t, e'), map' )
        else raise ( Failure ("return gives " ^ string_of_typ t ^ " expected " ^
       string_of_typ func.typ ^ " in " ^ string_of_expr e))

      | Block sl -> 
        let rec check_stmt_list map sl = match sl with
            [Return _ as s] -> ([fst (check_stmt map s)], map)
          | Return _ :: _   -> raise (Failure "nothing may follow a return")
          | Block sl :: ss  -> check_stmt_list map (sl @ ss) (* Flatten blocks *)
          | s :: ss         -> let cs, m' = check_stmt map s in 
                              let csl, m'' = check_stmt_list m' ss in 
                              (cs::csl, m'')
          | []              -> ([], map)
        in (SBlock(fst (check_stmt_list map sl)), map)
      | While(cond, stmtList) -> SWhile(check_bool_expr map cond, fst (check_stmt map stmtList)), map
      | For(e1, e2, e3, stmtList) -> let (st1, m') = check_stmt map e1 in 
                                     let (ty3, sx3, m'') = check_expr m' e3 in
                                     SFor(st1, check_bool_expr m'' e2, (ty3, sx3), fst (check_stmt m'' stmtList)), m''
      | If(cond, s1, s2) -> 
        let sthen, _ = check_stmt map s1 in
        let selse, _ = check_stmt map s2 in
        (SIf(check_bool_expr map cond, sthen, selse), map)
      | _ -> raise (Failure "Match failure")

    in (* body of check_function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name (ty, name, 0) m) StringMap.empty func.formals
    in
      {
        styp = func.typ;
        sfname = func.fname;
        sformals = func.formals;
        sfstmts = match fst (check_stmt symbols (Block(func.fstmts))) with
          SBlock(sl) -> sl
          | _ -> let err = "internal error: block didn't become a block?"
          in raise (Failure err)
    }
      
  in 
  let sfuncc = List.map check_function functions in 
  let sstructs = List.map check_struct structs in
  (sstructs, sfuncc)

