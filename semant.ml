(* Semantic checking for the PartialC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

let check (functions) =
  (**** Checking Functions ****)

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " is a built-in function and may not be redefined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n map -> make_err dup_err  
       | _ ->  StringMap.add n fd map 
  in
  (*build in fucntions*)
  let built_in_funcs = List.fold_left add_func StringMap.empty [
      {typ = Void; fname = "prints"; formals = [(String, "args")];  fstmts = [] };
      {typ = Void; fname = "printi"; formals = [(Int, "args")];  fstmts = [] };
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
    let add_var map ventry = 
        let name = snd ventry in
        let dup_err = "Variable with name " ^ name ^" is a duplicate." in
        match ventry with
        _ when StringMap.mem name map -> raise (Failure dup_err)
        | _ -> StringMap.add name ventry map
    in
    
    let find_var map name =
        try StringMap.find name map
        with Not_found -> raise( Failure("Undeclared variable: " ^ name))
    in
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in 
    let type_of_identifier s symbols =  fst(try StringMap.find s symbols with Not_found -> raise( Failure("ID not found: " ^ s))) 
    (* let _ = print_string "check identifier map \n" in let _ = print_string ( snd (StringMap.find s symbols)^"\n") in *)
    in

    let rec check_expr map e = match e with
        IntLit  l -> (Int, SLit l, map)
      | FloatLit l -> (Float, SFloatLit l, map)
      | BoolLit l  -> (Bool, SBoolLit l, map)
      | StringLit l -> (String, SStringLit l, map)
      | Noexpr     -> (Void, SNoexpr, map)
      | Id s       -> (type_of_identifier s map, SId s, map)
      | AssignOp(v, e)-> 
        let lt, vname, map1 = find_name v map "assignment error" in
        let rt, ex, map2 = check_expr map1 e in
        (check_assign lt rt "type miss match", SAssignOp((lt, vname), (rt, ex)), map2)
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
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'), map)
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
              Add | Sub | Mul | Div      when same && t1 = Int   -> Int
              | Add | Sub | Mul | Div    when same && t1 = Float -> Float
              | Add                      when same && t1 = String -> String
              | Eq | Neq                 when same               -> Bool
              | Lt | Leq | Gt | Geq      when same && (t1 = Int || t1 = Float) -> Bool
              | And | Or                 when same && t1 = Bool -> Bool
              | _ -> raise (Failure ("Illegal binary operator " ^ string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^ string_of_typ t2 ^ " in " ^ string_of_expr ex))
        in (ty, SBinop((t1, e1'), op, (t2, e2')), map'')

    and find_name (name : expr) map err : (Ast.typ * Sast.sx * (Ast.typ * StringMap.key) StringMap.t) = match name with
        Id _ -> check_expr map name
        | _ -> raise (Failure ("find name error"))
    in
    let check_bool_expr map e = 
      let (t', e', map') = check_expr map e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    in
    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt map st = match st with
        Expr e -> let (ty, sx, map') = check_expr map e in (SExpr (ty, sx), map')
      | Return e -> let (t, e', map') = check_expr map e in
        if t = func.typ then (SReturn (t, e'), map' )
        else raise ( Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.typ ^ " in " ^ string_of_expr e))
      | VarDecl(t, id, e) ->
        let (right_t, sx, map') = check_expr map e  in

        let err = "illegal argument found." in
        (* let ty = check_type_equal t right_t err in *)
        let new_map = add_var map' (t, id) in
        let right = (right_t, sx) in
        (SVarDecl(t, id, right), new_map)
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
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


    in (* body of check_function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name (ty, name) m) StringMap.empty func.formals
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
      
  in (List.map check_function functions)