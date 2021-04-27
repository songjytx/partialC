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
let translate (structs, functions) =
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
  and void_t     = L.void_type   context 
  and struct_t n  = L.named_struct_type context n in
 (* Some compond type *)
  let array_t = fun (llvm_type) -> L.struct_type context [| L.pointer_type llvm_type; i32_t; i32_t|] in
  let string_t = L.struct_type context [| L.pointer_type char_t|] in 
  let i32OF = L.const_int (L.i32_type context) in
  (* llvm type of some strcut members *)
  let rec ltype_of_struct_members = function
      A.Struct n -> struct_t n 
    | A.Int -> i32_t
    | A.Float -> float_t
    | A.String -> string_t
    | A.Bool -> i1_t
  in
  (* Structs declaration*)
  let structs_decls = 
    let struct_decl map sdecl =
      let name = sdecl.ssname
      and member_types = Array.of_list (List.map (fun (t,_) -> ltype_of_struct_members t) sdecl.smembers) in 
      let stype = L.struct_type context member_types in
      StringMap.add name (stype, sdecl.smembers) map in
    List.fold_left struct_decl StringMap.empty structs
  in
  let lookup_struc s = 
  try StringMap.find s structs_decls
    with Not_found -> raise (Failure ("struct not found")) in
  (* llvm types for primitive types*)
  let rec ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Void  -> void_t
    | A.String -> string_t
    | A.Array a -> array_t (ltype_of_typ a)
    | A.Struct n -> fst (lookup_struc n)
  in
  (* llvm type of array element *)
  let rec ltype_of_array_element = function
  A.Array a -> ltype_of_typ a
  in
  (* print fucntion *)
  let printf_t = L.var_arg_function_type i32_t [| (L.pointer_type char_t)|] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls =
    let function_decl map fdecl =
      let name = fdecl.sfname
      and formal_types = Array.of_list (List.map (fun (t,_ ) -> ltype_of_typ t ) fdecl.sformals) in 
      let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) map in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    let char_format_str = L.build_global_stringptr "%s\n" "" builder
    and int_format_str = L.build_global_stringptr "%d\n" "" builder 
    and float_format_str = L.build_global_stringptr "%f\n" "" builder in
  (* loca, variables *)
    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
      let local = L.build_alloca (ltype_of_typ t) n builder in
          ignore (L.build_store p local builder);
          StringMap.add n (local, A.Void) m 
      in
      List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
   (* loop up for variables *)
    let lookup map n = match StringMap.find_opt n map with
        Some (v, _) -> v 
      | None -> try fst (StringMap.find n local_vars)
                with Not_found -> report_error("Could not find " ^ n)
    in
    (* Construct code for an expression; return its value *)
    let rec expr map builder ((_, expression) : sexpr) = match expression with
        SIntLit i  -> L.const_int i32_t i, map, builder  
      | SFloatLit f -> L.const_float float_t f, map, builder
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0), map, builder
      | SStringLit s -> 
        let alloc = L.build_alloca string_t "alloc" builder in 
        let strGlobal = L.build_global_string s "strGlobal" builder in
        let str = L.build_bitcast strGlobal (L.pointer_type i8_t) "str_cast" builder in 
        let str_loc = L.build_struct_gep alloc 0 "str_cast_loc" builder in
        let _ = L.build_store str str_loc builder in
        let value = L.build_load alloc "" builder
        in (value, map, builder)

      | SArrayLit a -> 
         let llvm_ty = ltype_of_typ (fst (List.hd a))in
         let ty = array_t llvm_ty in 
         let alloc = L.build_alloca ty "alloc" builder in
         let data_location = L.build_struct_gep alloc 0 "data_location" builder in
         let len_loc = L.build_struct_gep alloc 1 "" builder in
         let len = List.length a in
         let cap = len * 2 in 
         let data_loc = L.build_array_alloca llvm_ty (i32OF cap) "data_loc" builder in
         let array_iter (acc, builder) ex = 
           let value, m', builder = expr map builder ex in
           let item_loc = L.build_gep data_loc [|i32OF acc |] "item_loc" builder in
           let _ = L.build_store value item_loc builder in
           (acc+1, builder)
         in
         let _, builder = List.fold_left array_iter (0, builder) a in
         let _ = L.build_store data_loc data_location builder in
         let _ = L.build_store (i32OF len) len_loc builder in
         let value = L.build_load alloc "value" builder 
       in (value, map, builder)

      | SArrayIndex(id, idx) -> 
        let name = match snd id with 
            SId s -> s
          | _ -> "err:cannot index non-id"
        in
        let a_addr = lookup map name in
        let data_location = L.build_struct_gep a_addr 0 "" builder in
        let data_loc = L.build_load data_location "" builder in
        let ival, _, builder = expr map builder idx in
        let i_addr = L.build_gep data_loc [| ival |] "" builder in 
        let value = L.build_load i_addr "" builder in
        (value, map, builder)

      | SNoexpr(t)     -> (match t with
          A.Int -> L.const_int i32_t 0
        | A.Float -> L.const_float float_t 0.0
        | A.Bool -> L.const_int i1_t 1
        | A.String -> 
          let alloc = L.build_alloca string_t "alloc" builder in 
          let strGlobal = L.build_global_string "" "strGlobal" builder in
          let str = L.build_bitcast strGlobal (L.pointer_type i8_t) "str_cast" builder in (* Mingjie: this is crucial*)
          let str_loc = L.build_struct_gep alloc 0 "str_cast_loc" builder in
          let _ = L.build_store str str_loc builder in
          L.build_load alloc "" builder

        | A.Array _ -> 
          let llvm_ty = ltype_of_typ (fst (List.hd []))in
          let ty = array_t llvm_ty in 
          let alloc = L.build_alloca ty "alloc" builder in
          let data_location = L.build_struct_gep alloc 0 "data_location" builder in
          let len_loc = L.build_struct_gep alloc 1 "" builder in
          let len = 0 in
          let cap = len * 2 in 
          let data_loc = L.build_array_alloca llvm_ty (i32OF cap) "data_loc" builder
          in
          let _ = L.build_store data_loc data_location builder in
          let _ = L.build_store (i32OF len) len_loc builder in
          L.build_load alloc "value" builder) , map, builder

      | SId s       -> L.build_load (lookup map s) s builder, map, builder
      | SAssignOp (v, e) -> 
        let (e1, map1, builder) = expr map builder e in 
          (match (snd v) with
            SId s -> 
            ignore(L.build_store e1 (lookup map s) builder); e1, map1, builder)

      | SArrayAssignOp (v, i, e) -> 
        let rval, m', builder = expr map builder e in
        let name = match snd v with
            SId s -> s
        in
        let a_addr = lookup map name in
        let data_location = L.build_struct_gep a_addr 0 "" builder in
        let data_loc = L.build_load data_location "" builder in
        let ival, _, builder = expr map builder i in
        let addr = L.build_gep data_loc [| ival |] "" builder  in
        let _ = L.build_store rval addr builder in 
        (rval, m', builder)

      | SStructAssignOp (v, m, e) ->
        let rval, map1, builder = expr map builder e in
        let name = match snd v with
            SId s -> s
        in
        let a_addr = lookup map1 name in
        let strcut_name = (match snd (StringMap.find name map1) with A.Struct i -> i) in
        (* let _ = print_string strcut_name in *)

        let mname = (match m with A.Id i -> i) in

        let members = snd (lookup_struc strcut_name) in  
          let rec get_idx n lst i = match lst with
            | [] -> raise (Failure( "Struct member Error"))
            | hd::tl -> if (hd=n) then i else get_idx n tl (i+1)   
          in let idx = (get_idx mname (List.map (fun (_,nm) -> nm) members) 0) in            
        let ptr = L.build_struct_gep a_addr idx ("struct_p") builder in
        let _ = L.build_store rval ptr builder in
        (rval, map1, builder)

      | SStructAccess (v, m) -> 
        let name = match snd v with
            SId s -> s
        in
        let a_addr = lookup map name in
        let strcut_name = (match snd (StringMap.find name map) with A.Struct i -> i) in

        let mname = (match m with A.Id i -> i) in

        let members = snd (lookup_struc strcut_name) in  
          let rec get_idx n lst i = match lst with
            | [] -> raise (Failure( "Struct member Error"))
            | hd::tl -> if (hd=n) then i else get_idx n tl (i+1)   
          in let idx = (get_idx mname (List.map (fun (_,nn) -> nn) members) 0) in 
        let ptr = L.build_struct_gep a_addr idx ("struct_p") builder in
        let value = L.build_load ptr "member_v" builder in
        (value, map, builder)

      | SNot (e) -> 
        let (e', _, _) = expr map builder e in
        L.build_not e' "not operation" builder, map, builder

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
    	  ) e1' e2' "float op" builder, map, builder

      | SBinop (e1, A.Mod, e2) -> 
        let (e1', _, _) = expr map builder e1
        and (e2', _, _) = expr map builder e2 in
        L.const_srem e1' e2', map, builder

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
    	  ) e1' e2' "general op" builder, map, builder

      | SCall ("prints", [e]) -> 
        let e', _, builder = expr map builder e in L.build_call printf_func [| char_format_str ; e' |] "printf" builder, map, builder

      | SCall ("printi", [e]) -> 
        let e', _, builder = expr map builder e in L.build_call printf_func [| int_format_str ; e' |] "printf" builder, map, builder

      | SCall ("printf", [e]) -> 
        let e', _, builder = expr map builder e in L.build_call printf_func [| float_format_str ; e' |] "printf" builder, map, builder
      
      | SCall ("sizeof", [e]) -> let a_addr = (match e with _, SId s -> lookup map s) in 
        let len_field_loc = L.build_struct_gep a_addr 1 "" builder in
        let value = L.build_load len_field_loc "" builder in
        value, map, builder

      | SCall ("sizeof", [e]) -> 
        let (e', _, builder) = expr map builder e  in
        let length = L.array_length (L.type_of e') in
        L.const_int i32_t length, map, builder

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
          (match ty with
          A.Struct s-> 
              let l_type = ltype_of_typ ty in
              let addr = L.build_alloca l_type st builder in         
              let m' = StringMap.add st (addr, ty) map in
              (builder, m')
          | _ ->
              let l_type = ltype_of_typ ty in
              let addr = L.build_alloca l_type st builder in
              let rval, m', builder = expr map builder rex in
              let m'' = StringMap.add st (addr, A.Void) m' in
              let _ = L.build_store rval addr builder in 
          (builder, m''))

      | SArrayDecl(t, v, e1, e) -> 

          let llvm_ty = ltype_of_typ t in
          let addr = L.build_alloca llvm_ty v builder in          
          let alloc = L.build_alloca llvm_ty "alloc" builder in
          let data_location = L.build_struct_gep alloc 0 "data_location" builder in
          let len = (match e1 with _, SIntLit i -> i) in 
          let len_loc = L.build_struct_gep alloc 1 "" builder in
          let cap = len * 2 in 
          let data_loc = L.build_array_alloca (ltype_of_array_element t) (i32OF cap) "data_loc" builder in
          let noexpr_value = (match t with 
              Array Ast.Int -> L.const_int i32_t 0
            | Array Ast.Float -> L.const_float float_t 0.0
            | Array Ast.Bool -> L.const_int i1_t 1
            | Array Ast.String -> let alloc = L.build_alloca string_t "alloc" builder in 
                        let strGlobal = L.build_global_string "" "strGlobal" builder in
                        let str = L.build_bitcast strGlobal (L.pointer_type i8_t) "str_cast" builder in (* Mingjie: this is crucial*)
                        let str_loc = L.build_struct_gep alloc 0 "str_cast_loc" builder in
                        let _ = L.build_store str str_loc builder in
                        L.build_load alloc "" builder)
          in 
          let rec sto (acc, builder) = 
            let item_loc = L.build_gep data_loc [|i32OF acc |] "item_loc" builder in
            let _ = L.build_store noexpr_value item_loc builder in
            if acc < len then sto (acc + 1, builder) else acc, builder
          in
          let _, builder = sto (0, builder) in
          let m' = StringMap.add v (addr, A.Void) map in
          let _ = L.build_store data_loc data_location builder in
          let _ = L.build_store (i32OF len) len_loc builder in
          let value = L.build_load alloc "value" builder in 
          let dl = lookup m' v in
          let _ = L.build_store value dl builder in 
          (* ignore(L.build_store value (lookup m' v) builder);  *)
          (builder, m')

      | SWhile(condition, stmtList) ->
          let pred_bb = L.append_block context "while" the_function in
          ignore(L.build_br pred_bb builder);
          let body_bb = L.append_block context "while_body" the_function in
          let body_bldr, m' = stmt map (L.builder_at_end context body_bb) stmtList in
          add_terminal body_bldr (L.build_br pred_bb);
          let pred_builder = L.builder_at_end context pred_bb in
          let bool_val, _, _ = expr m' pred_builder condition in
          let merge_bb = L.append_block context "merge" the_function in
          ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
          L.builder_at_end context merge_bb, m'

      | SFor(e1, e2, e3, stmtList) -> stmt map builder ( SBlock [ e1 ; SWhile (e2, SBlock [stmtList ; SExpr e3]) ] )
      | SIf(e, s1, s2) -> 
        let bool_val, m', builder = expr map builder e in
        let merge_bb = L.append_block context "merge" the_function in
        let build_br_merge = L.build_br merge_bb in (* partial function *)
        let then_bb = L.append_block context "then" the_function in
        let then_builder, m'' = stmt m' (L.builder_at_end context then_bb) s1 in
          add_terminal then_builder build_br_merge;
        let else_bb = L.append_block context "else" the_function in
        let else_builder, m'' = stmt m' (L.builder_at_end context else_bb) s2 in
          add_terminal else_builder build_br_merge;
        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context merge_bb, m'
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
