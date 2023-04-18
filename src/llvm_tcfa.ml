open List 
open Tast
open Tcfa
open Llvm
open Llvm.ValueKind
open Llvm.TypeKind
open Llvm.Opcode
open Printf
open Lldebug
open Llou
open Btcfa
open Str
(*open Batteries  *)
open Cpa_utils
open Thrift_server
(*
  By Cyrus@AVTA  
  Abstraction over lifting bianry code, llvm IR bridge.
*)

let create_example () =
  let fun_hash = Hashtbl.create 6 in
  let node_hash = Hashtbl.create 6 in
  let edge_hash = Hashtbl.create 6 in
  let temp1_hash = Hashtbl.create 6 in
  let temp2_hash = Hashtbl.create 6 in
  
  let file_loc = {
      file_name = "simple_example";
      nice_file_name = "simple_example";
      offset = 0;
      length = 1;
      starting_line = 0;
      ending_line = 0;
      starting_line_in_origin = 0;
      ending_line_in_origin = 0;
    } in

  let fun_dec_main = {
      func_location = file_loc;
      func_type = FunctionType {
                      return_type = VoidType;
                      parameters = [];
                      takes_var_args = false;
                    };
      func_name = "main";
      func_orig_name = "main";
      func_is_global = true;
      func_parameters = [];
    } in

  let fun_dec_error = {
      func_location = file_loc;
      func_type = FunctionType {
                      return_type = VoidType;
                      parameters = [];
                      takes_var_args = false;
                    };
      func_name = "__VERIFIER_error";
      func_orig_name = "__VERIFIER_error";
      func_is_global = true;
      func_parameters = [];
    } in
  
  let id_node_en = incr_node () in
  let main_en =  {
      fentry_node_number = id_node_en;
      fentry_leaving_edges = [];
      fentry_entering_edges = [];
      fentry_is_loop_start = false;
      fentry_function_name = "main";
      fentry_leaving_summary_edge = None;
      fentry_entering_summary_edge = None;
      fentry_location = file_loc;
      fentry_function_definition = fun_dec_main;
      fentry_return_variable = None;
      fentry_exit_node = None;
    } in

  let id_node_exit = incr_node () in
  let main_exit = {
          fexit_node_number = id_node_exit;
          fexit_leaving_edges = [];
          fexit_entering_edges = [];
          fexit_is_loop_start = false;
          fexit_function_name = "main";
          fexit_leaving_summary_edge = None;
          fexit_entering_summary_edge = None;
          fexit_function_entry_node = None;
    } in

  main_en.fentry_exit_node <- (Some main_exit);
  main_exit.fexit_function_entry_node <- (Some main_en);
  Hashtbl.add temp1_hash id_node_en (FunctionEntryNode main_en);
  Hashtbl.add fun_hash id_node_en main_en;

  Hashtbl.add temp1_hash id_node_exit (FunctionExitNode main_exit);


  let id_node_en = incr_node () in
  let error_en =  {
      fentry_node_number = id_node_en;
      fentry_leaving_edges = [];
      fentry_entering_edges = [];
      fentry_is_loop_start = false;
      fentry_function_name = "__VERIFIER_error";
      fentry_leaving_summary_edge = None;
      fentry_entering_summary_edge = None;
      fentry_location = file_loc;
      fentry_function_definition = fun_dec_error;
      fentry_return_variable = None;
      fentry_exit_node = None;
    } in

  let id_node_exit = incr_node () in
  let error_exit = {
          fexit_node_number = id_node_exit;
          fexit_leaving_edges = [];
          fexit_entering_edges = [];
          fexit_is_loop_start = false;
          fexit_function_name = "__VERIFIER_error";
          fexit_leaving_summary_edge = None;
          fexit_entering_summary_edge = None;
          fexit_function_entry_node = None;
    } in

  error_en.fentry_exit_node <- (Some error_exit);
  error_exit.fexit_function_entry_node <- (Some error_en);
  
  Hashtbl.add temp2_hash id_node_en (FunctionEntryNode error_en);

  Hashtbl.add fun_hash id_node_en error_en;
  Hashtbl.add temp2_hash id_node_exit (FunctionExitNode error_exit);

  let id_edge = incr_edge () in
  let b_edge = BlankEdge {
                   eblank_edge_number = id_edge;
                   eblank_predecessor = (FunctionEntryNode error_en);
                   eblank_successor = (FunctionExitNode error_exit);
                   eblank_raw_statement = "dummy error balnk edge;";
                   eblank_file_location = file_loc;
                   eblank_description = "dummy error blank edge;";
                 } in
  Hashtbl.add edge_hash id_edge b_edge;
  error_en.fentry_leaving_edges <- (b_edge :: error_en.fentry_leaving_edges);
  error_exit.fexit_entering_edges <-(b_edge :: error_exit.fexit_entering_edges);



  let id_node_gen = incr_node () in
  let gen1 = {
      ngen_node_number = id_node_gen;
      ngen_leaving_edges = [];
      ngen_entering_edges = [];
      ngen_is_loop_start = false;
      ngen_function_name = "main";
      ngen_leaving_summary_edge = None;
      ngen_entering_summary_edge = None;
    } in
  Hashtbl.add temp1_hash id_node_gen (GeneralNode gen1);

  let a_decl = VariableDeclaration {
                            var_location = file_loc;
                            var_type = PointerType {
                                           pointer_is_const = false;
                                           pointer_is_volatile = false;
                                           pointer_data_type = NumericType INT;
                                         };
                            var_name = "a";
                            var_orig_name = "a";
                            var_is_global = false;
                            var_qualified_name = ("main::a");
                            var_initializer = None;
                          } in

  let id_edge1 = incr_edge () in
  let decl_edge = DeclarationEdge {
                      edecl_edge_number = id_edge1;
                      edecl_predecessor = FunctionEntryNode main_en;
                      edecl_successor = GeneralNode gen1;
                      edecl_raw_statement = "variable a;";
                      edecl_file_location = file_loc;
                      edecl_declaration = a_decl;
                    } in

  Hashtbl.add edge_hash id_edge1 decl_edge;
  main_en.fentry_leaving_edges <- (decl_edge :: main_en.fentry_leaving_edges);
  gen1.ngen_entering_edges <-(decl_edge :: gen1.ngen_entering_edges);
 
  let id_node_gen = incr_node () in
  let gen2 = {
      ngen_node_number = id_node_gen;
      ngen_leaving_edges = [];
      ngen_entering_edges = [];
      ngen_is_loop_start = false;
      ngen_function_name = "main";
      ngen_leaving_summary_edge = None;
      ngen_entering_summary_edge = None;
    } in
  Hashtbl.add temp1_hash id_node_gen (GeneralNode gen2);

  let rhs_assign = LiteralExpr (IntLitExpr {
                                    int_location = file_loc;
                                    int_type = NumericType INT;
                                    int_value = -1;
                     }) in
  let a_expr = LHSExpr (IdExpr {
                            id_location = file_loc;
                            id_type = PointerType {
                                           pointer_is_const = false;
                                           pointer_is_volatile = false;
                                           pointer_data_type = NumericType INT;
                                         };
                            id_name = "a";
                            id_declaration = a_decl;
                 }) in

  let assignee_stmt = {
      eassign_location = file_loc;
      eassign_lhs = PointerExpr {
                        pointer_location = file_loc;
                        pointer_type = PointerType {
                                           pointer_is_const = false;
                                           pointer_is_volatile = false;
                                           pointer_data_type = NumericType INT;
                                         };
                        pointer_operand = a_expr;
                      };
      eassign_rhs = Expr rhs_assign;
    } in
      
  let id_edge = incr_edge () in
  let stmt_edge = StatementEdge {
                      estmt_edge_number = id_edge;
                      estmt_predecessor = GeneralNode gen1;
                      estmt_successor = GeneralNode gen2;
                      estmt_raw_statement = "assignment for a;";
                      estmt_file_location = file_loc;
                      estmt_statement = ExprAssignmentStatement assignee_stmt;
                    } in  
  Hashtbl.add edge_hash id_edge stmt_edge;
  gen1.ngen_leaving_edges <- (stmt_edge :: gen1.ngen_leaving_edges);
  gen2.ngen_entering_edges <-(stmt_edge :: gen2.ngen_entering_edges);
  

  let id_node_gen = incr_node () in
  let gen3 = {
      ngen_node_number = id_node_gen;
      ngen_leaving_edges = [];
      ngen_entering_edges = [];
      ngen_is_loop_start = false;
      ngen_function_name = "main";
      ngen_leaving_summary_edge = None;
      ngen_entering_summary_edge = None;
    } in
  Hashtbl.add temp1_hash id_node_gen (GeneralNode gen3);
 

  let ptr_expr = LHSExpr (PointerExpr {
                              pointer_location = file_loc;
                              pointer_type = NumericType INT;
                              pointer_operand = a_expr;
                   }) in
 
  let b_init = InitExpr {
                     init_expr_location = file_loc;
                     init_expression = ptr_expr;
    } in
  let b_decl = VariableDeclaration {
                            var_location = file_loc;
                            var_type = NumericType INT;
                            var_name = "b";
                            var_orig_name = "b";
                            var_is_global = false;
                            var_qualified_name = ("main::b");
                            var_initializer = Some b_init;
                          } in

  let id_edge = incr_edge () in
  let decl_edge = DeclarationEdge {
                      edecl_edge_number = id_edge;
                      edecl_predecessor = GeneralNode gen2;
                      edecl_successor = GeneralNode gen3;
                      edecl_raw_statement = "declare b with intial;";
                      edecl_file_location = file_loc;
                      edecl_declaration = b_decl;
                    } in

  Hashtbl.add edge_hash id_edge decl_edge;
  gen2.ngen_leaving_edges <- (decl_edge :: gen2.ngen_leaving_edges);
  gen3.ngen_entering_edges <-(decl_edge :: gen3.ngen_entering_edges);

  let id_node_gen = incr_node () in
  let gen4 = {
      ngen_node_number = id_node_gen;
      ngen_leaving_edges = [];
      ngen_entering_edges = [];
      ngen_is_loop_start = false;
      ngen_function_name = "main";
      ngen_leaving_summary_edge = None;
      ngen_entering_summary_edge = None;
    } in
  Hashtbl.add temp1_hash id_node_gen (GeneralNode gen4);
 

  let id_node_gen = incr_node () in
  let gen5 = {
      ngen_node_number = id_node_gen;
      ngen_leaving_edges = [];
      ngen_entering_edges = [];
      ngen_is_loop_start = false;
      ngen_function_name = "main";
      ngen_leaving_summary_edge = None;
      ngen_entering_summary_edge = None;
    } in
  Hashtbl.add temp1_hash id_node_gen (GeneralNode gen5);

  let b_expr = LHSExpr (IdExpr {
                            id_location = file_loc;
                            id_type = NumericType INT;
                            id_name = "b";
                            id_declaration = b_decl;
                 }) in
   
  let expr = BinExpr {
                 bin_location = file_loc;
                 bin_type = NumericType BOOL;
                 bin_operand_1 = b_expr;
                 bin_operand_2 = LiteralExpr (IntLitExpr {
                                                  int_location = file_loc;
                                                  int_type = NumericType INT;
                                                  int_value = -1;
                                   });
                 bin_operator = EQ;
               } in

  let id_edge = incr_edge () in
  let assume_true = AssumeEdge {
                        eassume_edge_number = id_edge;
                        eassume_predecessor = GeneralNode gen3;
                        eassume_successor = GeneralNode gen4;
                        eassume_raw_statement = "truth path;";
                        eassume_file_location = file_loc;
                        eassume_truth_assumption = true;
                        eassume_swapped = false;
                        eassume_artificial_intermediate = false;
                        eassume_expression = expr;
                    } in
  Hashtbl.add edge_hash id_edge assume_true;
  gen3.ngen_leaving_edges <- (assume_true :: gen3.ngen_leaving_edges);
  gen4.ngen_entering_edges <-(assume_true :: gen4.ngen_entering_edges);
   
  let id_edge = incr_edge () in
  let assume_false = AssumeEdge {
                        eassume_edge_number = id_edge;
                        eassume_predecessor = GeneralNode gen3;
                        eassume_successor = GeneralNode gen5;
                        eassume_raw_statement = "false path;";
                        eassume_file_location = file_loc;
                        eassume_truth_assumption = false;
                        eassume_swapped = true;
                        eassume_artificial_intermediate = false;
                        eassume_expression = expr;
                    } in
  Hashtbl.add edge_hash id_edge assume_false;
  gen3.ngen_leaving_edges <- (assume_false :: gen3.ngen_leaving_edges);
  gen5.ngen_entering_edges <-(assume_false :: gen5.ngen_entering_edges);



  let id_edge = incr_edge () in
  let b_edge1 = BlankEdge {
                   eblank_edge_number = id_edge;
                   eblank_predecessor = (GeneralNode gen5);
                   eblank_successor = (FunctionExitNode main_exit);
                   eblank_raw_statement = "dummy balnk edge;";
                   eblank_file_location = file_loc;
                   eblank_description = "dummy blank edge;";
                 } in
  
  Hashtbl.add edge_hash id_edge b_edge;
  gen5.ngen_leaving_edges <- (b_edge1 :: gen5.ngen_leaving_edges);
  main_exit.fexit_entering_edges <-(b_edge1 :: main_exit.fexit_entering_edges);

  let id_node_gen = incr_node () in
  let gen6 = {
      ngen_node_number = id_node_gen;
      ngen_leaving_edges = [];
      ngen_entering_edges = [];
      ngen_is_loop_start = false;
      ngen_function_name = "main";
      ngen_leaving_summary_edge = None;
      ngen_entering_summary_edge = None;
    } in
  Hashtbl.add temp1_hash id_node_gen (GeneralNode gen6);

  let id_edge = incr_edge () in
  let b_edge2 = BlankEdge {
                   eblank_edge_number = id_edge;
                   eblank_predecessor = (GeneralNode gen6);
                   eblank_successor = (FunctionExitNode main_exit);
                   eblank_raw_statement = "dummy balnk edge;";
                   eblank_file_location = file_loc;
                   eblank_description = "dummy blank edge;";
                 } in
  Hashtbl.add edge_hash id_edge b_edge2;
  gen6.ngen_leaving_edges <- (b_edge2 :: gen6.ngen_leaving_edges);
  main_exit.fexit_entering_edges <-(b_edge2 :: main_exit.fexit_entering_edges);

  (match b_edge2 with
   | BlankEdge blank_e -> printf "printing out the second blank edge in main: \n %s\n" (pr_blank_edge blank_e)
   | _ -> failwith "expecting blank_edge!");

  printf "general node gen6: \n %s\n" (pr_tcfa_node (GeneralNode gen6));


  

  let callee_id_expr = IdExpr {
                           id_location = file_loc;
                           id_type = FunctionType {
                                         return_type = VoidType;
                                         parameters = [];
                                         takes_var_args = false;
                                       };
                           id_name = "__VERIFIER_error";
                           id_declaration = FunctionDeclaration fun_dec_error;
                         } in 
  let call_expr = {
      call_location = file_loc;
      call_type = VoidType;
      call_function_name = LHSExpr callee_id_expr;
      call_parameters = [];
      call_declaration = fun_dec_error;
    } in

  let fcs = {
      cstmt_location = file_loc;
      cstmt_expression = call_expr;
    } in 
  let fc = (FunctionCallStatement fcs) in
  
  let id_edge = incr_edge () in
  let fs_edge = {
      esummary_edge_number = id_edge;
      esummary_predecessor = GeneralNode gen4;
      esummary_successor = GeneralNode gen6;
      esummary_raw_statement = "function call summary;";
      esummary_file_location = file_loc;
      esummary_expression = fc;
      esummary_function_entry = error_en;
    } in
  Hashtbl.add edge_hash (id_edge) (FunctionSummaryEdge fs_edge);  
  gen4.ngen_leaving_summary_edge <- Some fs_edge;
  gen6.ngen_entering_summary_edge <- Some fs_edge;
  
  let id_edge = incr_edge () in
  let fc_edge = FunctionCallEdge {
                    ecall_edge_number = id_edge;
                    ecall_predecessor = (GeneralNode gen4);
                    ecall_successor = FunctionEntryNode error_en;
                    ecall_raw_statement = "funcion call edge;";
                    ecall_file_location = file_loc;
                    ecall_function_call = fc;
                    ecall_summary_edge = fs_edge;
                  } in
  Hashtbl.add edge_hash id_edge fc_edge;
  gen4.ngen_leaving_edges <- (fc_edge :: gen4.ngen_leaving_edges);
  error_en.fentry_entering_edges <- (fc_edge :: error_en.fentry_entering_edges);

  let id_edge = incr_edge () in
  let fr_edge = FunctionReturnEdge {
                    efret_edge_number = id_edge;
                    efret_predecessor = FunctionExitNode error_exit;
                    efret_successor = GeneralNode gen6;
                    efret_raw_statement = "return to caller: main ;";
                    efret_file_location = file_loc;
                    efret_summary_edge = fs_edge;
                  } in
  Hashtbl.add edge_hash id_edge fr_edge;
  error_exit.fexit_leaving_edges <- (fr_edge :: error_exit.fexit_leaving_edges);
  gen6.ngen_entering_edges <- (fr_edge :: gen6.ngen_entering_edges);
 
  let gen6_enters = gen6.ngen_entering_edges in
  let gen6_leaves = gen6.ngen_leaving_edges in

  List.iter (fun e -> printf "entering edge of gen6: \n %s \n" (pr_tcfa_edge e)) gen6_enters;
  List.iter (fun e -> printf "leaving edge of gen6: \n %s \n" (pr_tcfa_edge e)) gen6_leaves;
  
  
  Hashtbl.add node_hash "main" temp1_hash;
  Hashtbl.add node_hash "__VERIFIER_error" temp2_hash;

   
  let example_bin = {
      tcfa_functions = fun_hash;
      tcfa_all_nodes = node_hash;
      tcfa_all_edges = edge_hash;

      tcfa_main_function = main_en;
      tcfa_file_names = [];
      tcfa_language = C;
    } in

(*------------------------Export and convert tcfa_bin------------------------------------------------*)

  printf "\n\n------------------Printing out tcfa from main entry function------------------------------------\n\n";
  
  print_string (pr_tcfa example_bin);
  
(*-----------------Connecting to cpa thrift server---------------------------------------------*)
  print_dot example_bin;
  printf "----tcfa_dot generated!-----\n";

  let reach_result = check_reachability example_bin in
  printf "Reachability result: %s \n" (pr_cpa_result reach_result)

















                    
let main () = 
  printf "-------start to iterate globals:-------\n";
  let file_name = Sys.argv.(1) in
  let llctx = Llvm.global_context () in
  let llmem = Llvm.MemoryBuffer.of_file file_name in
  let llm = Llvm_bitreader.parse_bitcode llctx llmem in

  
  (*----------------------- iterate over globals (create globals)---------------------------- *)
  
  let ins2id_hash = fold_left_globals global_var (Hashtbl.create 6) llm in
  let id2dec_hash = Hashtbl.create 6 in
  iter_globals (fun lli ->
      let instr = string_of_llvalue lli in
      printf "instruction: \n %s \n" instr;
      
      printf "-------instruction value kind (iterating globals)-------\n";
      print_value_kind lli;
      printf "-------instruction type kind (itereating globals)-------\n";
      let illty = type_of lli in
      let itty = llty_to_tty illty in
      print_type illty;
      (*     print_val lli;  *)

 (*     
      let global_init = global_initializer lli in
      let global_str = string_of_llvalue global_init in
      printf "global initializer: \n %s \n" global_str;
      printf "-------initializer value kind-------\n";
      print_value_kind global_init;
      printf "-------initializer type kind-------\n";
  
      let gllty = type_of global_init in
      let gtty = llty_to_tty gllty in
      print_type gllty;
      let var_init = None in
  *)  

     (*------------------get global initializer--------------------------------*)

      let var_init =
        (if (not (is_externally_initialized lli)) then
           (

             let global_init = global_initializer lli in
 
             let global_str = string_of_llvalue global_init in 
             printf "-----global initializer:--------\n%s\n" global_str;
             printf "-------initializer value kind-------\n";

             print_value_kind global_init;
(*
             printf "-------initializer type kind-------\n";
             
             let gllty = type_of global_init in
             let gtty = llty_to_tty gllty in
             print_type gllty;
  
          
             None
  *)           
           
             let file_loc = getFileLocation file_name global_init in
             let initial_kind = classify_value global_init in
             (match initial_kind with
              | ConstantArray | ConstantDataArray | ConstantVector | ConstantStruct ->

                 let g_init = getConstantAggregateInitializer global_init file_name ins2id_hash id2dec_hash in
                 Some g_init
                 (*
                 None*)
              | ConstantAggregateZero ->
                 let gllty = type_of global_init in
                 let gtty = llty_to_tty gllty in
                 let g_init = getZeroInitializer global_init gtty file_name in
                 Some g_init
               (*
                 None*)
              (*              | UndefValue -> failwith "undefined value, a type-less constant in global initializer."  *)
              | NullValue | Function->
                 (*--- can't get type kind for NullValue, core dump in ocaml
                 let gllty = type_of global_init in *)
                 None
              | _ ->                           
                 let gllty = type_of global_init in
                 let gtty = llty_to_tty gllty in
                 let g_expr = getConstant global_init gtty file_name ins2id_hash id2dec_hash in
                 let g_int = InitExpr {
                                 init_expr_location = file_loc;
                                 init_expression = g_expr;} in
                 Some g_int
                 (*
                 None*)
             )                                    
           )
         else
           None
        ) in
  
      let var_id = Hashtbl.find ins2id_hash lli in 
      let var_decl = {
          var_location = getFileLocation file_name lli;
          var_type = itty;
          var_name = "global_"^(string_of_int var_id);
          var_orig_name = "global_"^(string_of_int var_id);
          var_is_global = true;
          var_qualified_name = "global_"^(string_of_int var_id);
          var_initializer = var_init;
        } in
      Hashtbl.add id2dec_hash var_id (VariableDeclaration var_decl)
    ) llm;
 
(*---------------iterate over functions--------------------------------------*)
  (*find "main" entry function in llvm IR*)
  let fun_main = ref "" in
  let fun_error = ref "" in
  iter_functions
    (fun fv ->
      let fun_name = value_name fv in
      let n = String.length fun_name in
      let reg_sub = regexp_string "sub" in
      let reg_main = regexp_string "main" in
      (*     let reg_error = regexp_string "__VERIFIER_error" in *)
      printf "function name : %s \n" fun_name ;
      if ((string_match reg_sub fun_name 0) && (string_match reg_main fun_name (n-4)))
      then
        fun_main := fun_name
      else
        () 
    ) llm;

(*-------------------Extract call graph from main entry functions------------ *)
  let cg_hash = Hashtbl.create 6 in
 
  (*  fun_main := "main";*)
  let main_callee = fun_callee llm (!fun_main) [] in
  llcg_main cg_hash llm (!fun_main) main_callee;
  let fun_num = Hashtbl.length cg_hash in
  Printf.printf "Functions reachable  count: %d\n" fun_num;

               
(*create a tcfa for cg*)
  let fun_id_hash = Hashtbl.create fun_num in
  let fun_hash = Hashtbl.create fun_num in
  let node_hash = Hashtbl.create fun_num in
  let edge_hash = Hashtbl.create fun_num in

 (* Hashtbl.iter (fun key value -> printf "fun_id__hash keys: {%s} \n" key) fun_id_hash; *)
  
  print_endline ("function name mapping size: "^ (string_of_int fun_num)); 
  (* 
  Hashtbl.iter (fun x y -> Printf.printf "%s -> %s\n"  x (string_of_int y)) fun_id_hash; 
  Hashtbl.iter (fun x y -> Printf.printf "%s -> %s\n" (string_of_int x) (pr_tcfa_node y)) node_hash;
  Hashtbl.iter (fun x y -> Printf.printf "%s -> %s\n" (string_of_int x) (pr_tcfa_edge y)) edge_hash 
  *)
  
  (*------ Initialize tcfa for call graph ----------*)
  let callers = list_dup (Hashtbl.fold (fun key bind caller-> caller @ [key]) cg_hash []) in
  printf "------all calllers in the main call graph:-----------\n";
  List.iter (fun caller -> printf "caller_name: %s\n" caller) callers;
  
  fun_init file_name callers llm cg_hash fun_id_hash fun_hash node_hash edge_hash ins2id_hash id2dec_hash;
  
  tcfa_gen file_name callers llm fun_id_hash fun_hash node_hash edge_hash ins2id_hash id2dec_hash;
     
 (*Printing out all the nodes and edges in tcfa*)
  Hashtbl.iter (fun x y -> Printf.printf "%s -> %s\n" (string_of_int x) (pr_tcfa_edge y)) edge_hash;
   
   
  let tcfa_bin = {
      tcfa_functions = fun_hash;
      tcfa_all_nodes = node_hash;
      tcfa_all_edges = edge_hash;

      tcfa_main_function = Hashtbl.find fun_hash (Hashtbl.find fun_id_hash (!fun_main));
      (*
      tcfa_main_function = Hashtbl.find fun_hash (Hashtbl.find fun_id_hash "sub_4003b0_main");
       *)
      tcfa_file_names = [];
      tcfa_language = C;
    } in

(*------------------------Export and convert tcfa_bin------------------------------------------------*)


  let oc = open_out "bin.tcfa" in
  fprintf oc "%s\n" (pr_tcfa tcfa_bin);
  close_out oc;
 

  printf "\n\n------------------Printing out tcfa from main entry function------------------------------------\n\n";
  
  print_string (pr_tcfa tcfa_bin);
  
(*-----------------Connecting to cpa thrift server---------------------------------------------*)
  print_dot tcfa_bin;
  printf "----tcfa_dot generated!-----\n";

  let reach_result = check_reachability tcfa_bin in
  printf "Reachability result: %s \n" (pr_cpa_result reach_result)
    
    
let () = Printexc.record_backtrace true
let () = main ()
             
  
   
