(* Semantic checking for the PartialC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

let check (functions) =
  (**** Checking Functions ****)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void;
      fname = name; 
      fargs = [];
      fstmts = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", Int);
                               ("printb", Bool);
                               ("printf", Float);
                               ("printbig", Int) ]
  in

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

  (* Collect all other function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    
    let rec expr e = match e with
        IntLit  l -> (Int, SLiteral l)
      | FloatLit l -> (Float, SFliteral l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | StringLit l -> (String, SStringLit l)
      | Noexpr     -> (Void, SNoexpr)

    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
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
      | Print e -> SPrint(expr e)

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sfargs = func.fargs;
      sfstmts = match check_stmt (Block func.fstmts) with
	SBlock(sl) -> sl
      | _ -> let err = "internal error: block didn't become a block?"
      in raise (Failure err)
    }
  in (List.map check_function functions)