(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (functions) =
  let report_error e = raise (Failure e) in 
  let context    = L.global_context () in
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "PartialC" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and char_t     = L.i8_type     context
  and void_t     = L.void_type   context in

(*   let string_t   = L.pointer_type i8_t in *)
  let string_t = L.struct_type context [| L.pointer_type char_t; i32_t (*length*); |] in 
  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Void  -> void_t
    | A.String -> string_t
  in

  let printf_t = L.var_arg_function_type i32_t [| (L.pointer_type char_t)|] in
    let printf_func = L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl map fdecl =
      let name = fdecl.sfname
      and formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t ) fdecl.sformals) in 
      let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) map in

    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    let char_format_str = L.build_global_stringptr "%s" "" builder
    and int_format_str = L.build_global_stringptr "%d" "" builder 
    and float_format_str = L.build_global_stringptr "%f" "" builder in

    let lookup map n : L.llvalue = match StringMap.find_opt n map with
        Some v -> v 
      | None -> report_error( "Couldn't find " ^ n)
    in

    (* Construct code for an expression; return its value *)
    let rec expr map builder ((_, e) : sexpr) = match e with

        SLit i  -> L.const_int i32_t i, map, builder  
      | SFloatLit f -> L.const_float float_t f, map, builder
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0), map, builder
      | SStringLit s -> L.build_global_stringptr s "str" builder, map, builder
      | SNoexpr     -> L.const_int i32_t 0, map, builder
      | SId s       -> L.build_load (lookup map s) s builder, map, builder
      | SAssignOp (v, e) -> let (e', map1, builder) = expr map builder e in match (snd v) with
                            SId s -> 
                            ignore(L.build_store e' (lookup map s) builder); e', map1, builder
      | SBinop ((A.Float,_ ) as e1, op, e2) ->
	  let (e1', _, _) = expr map builder e1
	  and (e2', _, _) = expr map builder e2 in
	  (match op with 
	    A.Add     -> L.build_fadd
	  | A.Sub     -> L.build_fsub
	  | A.Mul     -> L.build_fmul
	  | A.Div     -> L.build_fdiv 
	  | A.Eq   -> L.build_fcmp L.Fcmp.Oeq
	  | A.Neq     -> L.build_fcmp L.Fcmp.One
	  | A.Lt    -> L.build_fcmp L.Fcmp.Olt
	  | A.Leq     -> L.build_fcmp L.Fcmp.Ole
	  | A.Gt -> L.build_fcmp L.Fcmp.Ogt
	  | A.Geq     -> L.build_fcmp L.Fcmp.Oge
	  | A.And | A.Or ->
	      raise (Failure "internal error: semant should have rejected and/or on float")
	  ) e1' e2' "tmp" builder, map, builder
      | SBinop (e1, op, e2) ->
	  let (e1', _, _) = expr map builder e1
	  and (e2', _, _) = expr map builder e2 in
	  (match op with
	    A.Add     -> L.build_add
	  | A.Sub     -> L.build_sub
	  | A.Mul     -> L.build_mul
    | A.Div     -> L.build_sdiv
	  | A.And     -> L.build_and
	  | A.Or      -> L.build_or
	  | A.Eq   -> L.build_icmp L.Icmp.Eq
	  | A.Neq     -> L.build_icmp L.Icmp.Ne
	  | A.Lt    -> L.build_icmp L.Icmp.Slt
	  | A.Leq     -> L.build_icmp L.Icmp.Sle
	  | A.Gt -> L.build_icmp L.Icmp.Sgt
	  | A.Geq     -> L.build_icmp L.Icmp.Sge
	  ) e1' e2' "tmp" builder, map, builder

    | SCall ("prints", [e]) -> let e', _, builder = expr map builder e in L.build_call printf_func [| char_format_str ; e' |] "printf" builder, map, builder
    
    | SCall ("printi", [e]) -> let e', _, builder = expr map builder e in L.build_call printf_func [| int_format_str ; e' |] "printf" builder, map, builder
    
    | _ -> let _ = print_string "gg" in (L.const_int i32_t 0), map, builder

    | SCall (f, args) ->

    let (fdef, fdecl) = StringMap.find f function_decls in
	  let llargs = List.map (fun(a,b,c) -> a) (List.rev (List.map (expr map builder) (List.rev args))) in
	  let result = (match fdecl.styp with 
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list llargs) result builder, map, builder

    in
    
    (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr = match L.block_terminator (L.insertion_block builder) with
	      Some _ -> ()
      | None -> ignore (instr builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)

    let rec stmt map builder s = match s with
        SBlock sl -> 
               let b, _ = List.fold_left (fun (b, m) s -> stmt m b s) (builder, map) sl in (b, map)
      | SReturn e -> ignore(match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> let e',_,_ = (expr map builder e) in L.build_ret e' builder ); builder, map

      | SExpr e -> ignore(expr map builder e); builder, map
      | SVarDecl(ty, st, rex) -> 
            let l_type = ltype_of_typ ty in
            let addr = L.build_alloca l_type st builder in
            let rval, m', builder = expr map builder rex in
            let m'' = StringMap.add st addr m' in
            let _ = L.build_store rval addr builder in 
            (builder, m'')
      | _ -> report_error "No implementation"
    in

    (* Build the code for each statement in the function *)
    let builder,_= stmt StringMap.empty builder (SBlock fdecl.sfstmts) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
