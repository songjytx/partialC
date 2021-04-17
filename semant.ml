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
      {typ = Void; fname = "print"; formals = [(String, "arg")];  fstmts = [] };
      {typ = Void; fname = "printf"; formals = [(String, "arg")];  fstmts = [] };
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
    (*let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                  StringMap.empty ( func.locals )
    in*)

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
    let symbols = StringMap.add "a" (Int, "a") StringMap.empty in
    let type_of_identifier s symbols = fst(StringMap.find s symbols) 
    in

    let rec expr e = match e with
        IntLit  l -> (Int, SLit l)
      | FloatLit l -> (Float, SFloatLit l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | StringLit l -> (String, SStringLit l)
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s symbols, SId s)
      | Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | Return e -> let (t, e') = expr e in
        if t = func.typ then SReturn (t, e') 
        else raise ( Failure ("return gives " ^ string_of_typ t ^ " expected " ^
		   string_of_typ func.typ ^ " in " ^ string_of_expr e))
	    
	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
          let rec check_stmt_list = function
              [Return _ as s] -> [check_stmt s]
            | Return _ :: _   -> raise (Failure "nothing may follow a return")
            | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
            | s :: ss         -> check_stmt s :: check_stmt_list ss
            | []              -> []
          in SBlock(check_stmt_list sl)
      (* | Declare(t, id) ->
        let new_map = add_var map (t, id) in
        (SDeclare(t, id), new_map) *)

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      sfstmts = match check_stmt (Block func.fstmts) with
	SBlock(sl) -> sl
      | _ -> let err = "internal error: block didn't become a block?"
      in raise (Failure err)
    }
  in (List.map check_function functions)