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
         (* _ when StringMap.mem name map -> make_err dup_err*)
        _ -> StringMap.add name ventry map
    in
    
    let find_var map name =
        try StringMap.find name map
        with Not_found -> raise( Failure("Undeclared variable: " ^ name))
    in
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   
    let type_of_identifier s symbols = fst(try StringMap.find s symbols with Not_found -> raise( Failure("ID not found: " ^ s))) 
    in

    let rec expr map e = match e with
        IntLit  l -> (Int, SLit l, map)
      | FloatLit l -> (Float, SFloatLit l, map)
      | BoolLit l  -> (Bool, SBoolLit l, map)
      | StringLit l -> (String, SStringLit l, map)
      | Noexpr     -> (Void, SNoexpr, map)
      | Id s       -> (type_of_identifier s map, SId s, map)
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e', map') = expr map e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'), map)
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt map st = match st with
        Expr e -> let _ = print_string "Check 3\n" in 
            let (ty, sx, map') = expr map e in (SExpr (ty, sx), map')
      | Return e -> let (t, e', map') = expr map e in
        if t = func.typ then (SReturn (t, e'), map' )
        else raise ( Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.typ ^ " in " ^ string_of_expr e))
      | VarDecl(t, id, e) ->
        let _ = print_string "Check1\n" in
        let (right_t, sx, map') = expr map e  in

        let err = "illegal argument found." in
        (* let ty = check_type_equal t right_t err in *)
        let new_map = add_var map' (t, id) in
        let right = (right_t, sx) in
        let _ = print_string "Check2: " in
        let _ = print_string ((snd (StringMap.find id new_map))^"\n") in
        (SVarDecl(t, id, right), new_map)
        
      (* A block is correct if each statement is correct and nothing
         follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          let rec check_stmt_list map sl = match sl with
              [Return _ as s] -> ([fst (check_stmt map s)], map)
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list map (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> let _ = print_string "check ss\n" in ((fst (check_stmt map s)) :: (fst (check_stmt_list map ss)), map)
            | []              -> ([], map)
          in (SBlock(fst (check_stmt_list map sl)), map)

    in (* body of check_function *)
    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name (ty, name) m) StringMap.empty func.formals
    in
      {
        styp = func.typ;
        sfname = func.fname;
        sformals = func.formals;
        sfstmts = match fst (check_stmt symbols (Block func.fstmts)) with
          SBlock(sl) -> sl
          | _ -> let err = "internal error: block didn't become a block?"
          in raise (Failure err)
    }
      
  in (List.map check_function functions)