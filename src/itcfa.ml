open List 
open Tast
open Tcfa
open Llou
open Lldebug
open Llvm
open Printf

(*
  By Cyrus@AVTA  
  This module is buliding up tcfa nodes and edges for individual llvm IR insturctions.
*)
 
(*---------------------------------------------LLVM IR alloca instruction translation------------------------------------------*)
let ins2tcfa_alloca file_name fun_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash node_hash edge_hash =
  let instr = string_of_llvalue lli in
  printf "instruction: \n %s \n" instr;
  printf "-------instruction value kind and type kind-------\n";
  let illty = type_of lli in
  let itty = llty_to_tty illty in
  print_value_kind lli;
  print_type illty;
  let ins_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash lli) in
  let opr = operand lli 0 in
  let llty = type_of opr in
  printf "-------operand value kind and type kind------- \n";
  let tty = llty_to_tty llty in
  print_value_kind opr;
  print_type llty;

  let ins_succ =
    (match (instr_succ lli) with
     | Before ins_s -> ins_s
     | _ -> failwith "Condn't find next instuction!"
    ) in
  let succ_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash ins_succ) in

  let id_edge = incr_edge () in  
  let file_loc = getFileLocation file_name lli in
  (*Initializer?----*)
  let var_decl = getAssignedTcfaDeclaration lli file_name fun_name None ins2id_hash id2dec_hash in 
  let decl_edge = DeclarationEdge {
                      edecl_edge_number = id_edge;
                      edecl_predecessor = ins_gen;
                      edecl_successor = succ_gen;
                      edecl_raw_statement = instr^";";
                      edecl_file_location = file_loc;
                      edecl_declaration = var_decl;
                    } in

  Hashtbl.add edge_hash id_edge decl_edge;
(*
  printf "tcfa: %s\n" (pr_tcfa_edge decl_edge);
 *)
  (*       Hashtbl.add var_hash lli var; *)
  let ins_gen' = tnode_to_general ins_gen in
  let succ_gen' = tnode_to_general succ_gen in
  ins_gen'.ngen_leaving_edges <- (decl_edge :: ins_gen'.ngen_leaving_edges);
  succ_gen'.ngen_entering_edges <-(decl_edge :: succ_gen'.ngen_entering_edges)


(*-----------------------------------------LLVM IR store instruction translation----------------------------------------*)
let ins2tcfa_store file_name fun_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash node_hash edge_hash =
  let instr = string_of_llvalue lli in
  Printf.printf "instr: %s\n" (value_name lli);
  Printf.printf "Store instruction: %s\n" instr;
  let n = num_operands lli in
  printf "number of operands: %s \n" (string_of_int n);
  let opr1 = operand lli 0 in
  let opr2 = operand lli 1 in
  printf "-----operand1: --------\n" ;
  let llty1 = type_of opr1 in
  let tty1 = llty_to_tty llty1 in
  print_value_kind opr1;
  print_type llty1;
  printf "-----operand2: --------\n" ;
  let llty2 = type_of opr2 in
  let tty2 = llty_to_tty llty2 in
  print_value_kind opr2;
  print_type llty2;

  let ins_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash lli) in
  let ins_succ =
    (match (instr_succ lli) with
     | Before ins_s -> ins_s
     | _ -> failwith "Condn't find next instuction!"
    ) in
  let succ_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash ins_succ) in

  let file_loc = getFileLocation file_name lli in  
  let rhs_assign = getExpression opr1 tty1 file_name ins2id_hash id2dec_hash in
  let assignee_stmt = get_assign_stmt opr2 rhs_assign file_name fun_name ins2id_hash id2dec_hash in
  
  let id_edge = incr_edge () in
  let stmt_edge = StatementEdge {
                      estmt_edge_number = id_edge;
                      estmt_predecessor = ins_gen;
                      estmt_successor = succ_gen;
                      estmt_raw_statement = instr^";";
                      estmt_file_location = file_loc;
                      estmt_statement = ExprAssignmentStatement assignee_stmt;
                    } in  
  Hashtbl.add edge_hash id_edge stmt_edge;
  let ins_gen' = tnode_to_general ins_gen in
  let succ_gen' = tnode_to_general succ_gen in
  ins_gen'.ngen_leaving_edges <- (stmt_edge :: ins_gen'.ngen_leaving_edges);
  succ_gen'.ngen_entering_edges <-(stmt_edge :: succ_gen'.ngen_entering_edges)

 (*-----------------------------------------LLVM IR load instruction translation-----------------------------------------*)
let ins2tcfa_load file_name fun_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash node_hash edge_hash =
  let instr = string_of_llvalue lli in
  let n =num_operands lli in

  Printf.printf "instr: %s\n" (value_name lli);
  Printf.printf "Load instruction: %s\n" instr;
  printf "-------instruction value kind and type kind-------\n";
  let illty = type_of lli in
  let itty = llty_to_tty illty in
  print_value_kind lli;
  print_type illty;


  printf "number of operands: %s \n" (string_of_int n);
  printf "-------operand value kind and type kind-------\n";
  let opr = operand lli 0 in
  let llty = type_of opr in
  let tty = llty_to_tty llty in
  print_value_kind opr;
  print_type llty;
  
  let ins_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash lli) in
  let ins_succ =
    (match (instr_succ lli) with
     | Before ins_s -> ins_s
     | _ -> failwith "Condn't find next instuction!"
    ) in
  let succ_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash ins_succ) in

  let file_loc = getFileLocation file_name lli in


  
  let opr_expr = getExpression opr tty file_name ins2id_hash id2dec_hash in
  let ptr_expr = LHSExpr (PointerExpr {
                              pointer_location = file_loc;
                              pointer_type = itty;
                              pointer_operand = opr_expr;
                   }) in
  (*Initializer?----*)
  let var_init = InitExpr {
                     init_expr_location = file_loc;
                     init_expression = ptr_expr;
    } in
  let var_decl = getAssignedTcfaDeclaration lli file_name fun_name (Some var_init) ins2id_hash id2dec_hash in

  let id_edge = incr_edge () in
  let decl_edge = DeclarationEdge {
                      edecl_edge_number = id_edge;
                      edecl_predecessor = ins_gen;
                      edecl_successor = succ_gen;
                      edecl_raw_statement = instr;
                      edecl_file_location = file_loc;
                      edecl_declaration = var_decl;
                    } in

  Hashtbl.add edge_hash id_edge decl_edge;


 (* 
  let lhs_id = getExpression lli itty file_name ins2id_hash id2dec_hash in
  let rhs_id = getExpression opr tty file_name ins2id_hash id2dec_hash in
  let expr_ass_stmt = {
      eassign_location = file_loc;
      eassign_lhs = (match lhs_id with
                     | LHSExpr lhs_expr -> lhs_expr
                     | _ -> failwith "expected an id expression for the left hand side of an assignment.");
      eassign_rhs = Expr (LHSExpr (PointerExpr {
                                       pointer_location = file_loc;
                                       pointer_type = tty;
                                       pointer_operand = rhs_id;
                      }));
    } in 
  
  let id_edge = incr_edge () in
  let stmt_edge = StatementEdge {
                      estmt_edge_number = id_edge;
                      estmt_predecessor = ins_gen;
                      estmt_successor = succ_gen;
                      estmt_raw_statement = instr^";";
                      estmt_file_location = file_loc;
                      estmt_statement = ExprAssignmentStatement expr_ass_stmt;
                    } in  
  Hashtbl.add edge_hash id_edge stmt_edge;
  *)
  let ins_gen' = tnode_to_general ins_gen in
  let succ_gen' = tnode_to_general succ_gen in
  ins_gen'.ngen_leaving_edges <- (decl_edge :: ins_gen'.ngen_leaving_edges);
  succ_gen'.ngen_entering_edges <-(decl_edge :: succ_gen'.ngen_entering_edges)

  

(*-------------------------------------------LLVM arithmetic operations translation-----------------------------------*)
let ins2tcfa_arithmetic file_name fun_name opcode lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash node_hash edge_hash =
  let n = num_operands lli in
  let instr = string_of_llvalue lli in
  Printf.printf "instr: %s\n" (value_name lli);
  Printf.printf "Instruction: %s\n" instr;
  printf "-------instruction value kind and type kind-------\n";
  let illty = type_of lli in
  let itty = llty_to_tty illty in
  print_value_kind lli;
  print_type illty;
  
  printf "number of operands: %s \n" (string_of_int n);
  let opr1 = operand lli 0 in
  let opr2 = operand lli 1 in
  printf "-----operand1: --------\n" ;
  Printf.printf "opr1: %s\n" (string_of_llvalue opr1);
  let llty1 = type_of opr1 in
  let tty1 = llty_to_tty llty1 in
  print_value_kind opr1;
  print_type llty1;
  printf "-----operand2: --------\n" ;
  (* Printf.printf "opr2: %s\n" (string_of_llvalue opr2);*)
  let llty2 = type_of opr2 in
  let tty2 = llty_to_tty llty2 in
  print_value_kind opr2;
  print_type llty2;
 
  let ins_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash lli) in
  let ins_succ =
    (match (instr_succ lli) with
     | Before ins_s -> ins_s
     | _ -> failwith "Condn't find next instuction!"
    ) in
  let succ_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash ins_succ) in
  let id_edge = incr_edge () in
(*  let dec1 = Hashtbl.find id2dec_hash (Hashtbl.find ins2id_hash opr1) in
 *)
  let ari_expr = createFromOpCode lli file_name opcode ins2id_hash id2dec_hash in
  let file_loc = getFileLocation file_name lli in
  (*Initializer?----*)
  let var_init = InitExpr {
      init_expr_location = file_loc;
      (*id_expression*)
      init_expression = ari_expr;
    } in
  let var_decl = getAssignedTcfaDeclaration lli file_name fun_name (Some var_init) ins2id_hash id2dec_hash in

  let id_edge = incr_edge () in
  let decl_edge = DeclarationEdge {
                      edecl_edge_number = id_edge;
                      edecl_predecessor = ins_gen;
                      edecl_successor = succ_gen;
                      edecl_raw_statement = instr^";";
                      edecl_file_location = file_loc;
                      edecl_declaration = var_decl;
                    } in

  Hashtbl.add edge_hash id_edge decl_edge;
  printf "tcfa: %s\n" (pr_tcfa_edge decl_edge);
  (*       Hashtbl.add var_hash lli var; *)
  let ins_gen' = tnode_to_general ins_gen in
  let succ_gen' = tnode_to_general succ_gen in
  ins_gen'.ngen_leaving_edges <- (decl_edge :: ins_gen'.ngen_leaving_edges);
  succ_gen'.ngen_entering_edges <-(decl_edge :: succ_gen'.ngen_entering_edges)

  
(*-----------------------------LLVM IR icmp instruction translation-----------------------------------------------------------*)
let ins2tcfa_icmp file_name fun_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash node_hash edge_hash =
  let n = num_operands lli in
  let instr = string_of_llvalue lli in
  Printf.printf "instr: %s\n" (value_name lli);
  Printf.printf "Instruction: %s\n" instr;
  printf "-------instruction type-------\n";
  let illty = type_of lli in
  let itty = llty_to_tty illty in
  print_type illty;

  let pre_icmp = (match (icmp_predicate lli) with
                  | Some pre -> pre
                  | None -> failwith "Not a Icmp instruction, cannot get its predicate.") in
  let pre_binary = icmp_to_bin pre_icmp in
  
  printf "number of operands: %s \n" (string_of_int n);
  let opr1 = operand lli 0 in
  let opr2 = operand lli 1 in
  printf "-----operand1: --------\n" ;
  let llty1 = type_of opr1 in
  let tty1 = llty_to_tty llty1 in
  print_value_kind opr1;
  print_type llty1;
  let operand1Exp = getExpression opr1 tty1 file_name ins2id_hash id2dec_hash in 

  Printf.printf "opr1: %s\n" (string_of_llvalue opr1);
  printf "-----operand2: --------\n" ;
  let llty2 = type_of opr2 in
  let tty2 = llty_to_tty llty2 in
  print_value_kind opr2;
  print_type llty2;
  let operand2Exp = getExpression opr2 tty2 file_name ins2id_hash id2dec_hash in 
 
  let ins_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash lli) in
  let ins_succ =
    (match (instr_succ lli) with
     | Before ins_s -> ins_s
     | _ -> failwith "Condn't find next instuction!"
    ) in
  let succ_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash ins_succ) in
  let id_edge = incr_edge () in
(*  let dec1 = Hashtbl.find id2dec_hash (Hashtbl.find ins2id_hash opr1) in
 *)

  let file_loc = getFileLocation file_name lli in
  let var_init = InitExpr {
      init_expr_location = file_loc;
      init_expression = BinExpr {
                            bin_location = file_loc;
                            bin_type = NumericType BOOL;
                            bin_operand_1 = operand1Exp;
                            bin_operand_2 = operand2Exp;
                            bin_operator = pre_binary;
                          };
    } in 
  let var_decl = getAssignedTcfaDeclaration lli file_name fun_name (Some var_init) ins2id_hash id2dec_hash in

  let decl_edge = DeclarationEdge {
                      edecl_edge_number = id_edge;
                      edecl_predecessor = ins_gen;
                      edecl_successor = succ_gen;
                      edecl_raw_statement = instr^";";
                      edecl_file_location = file_loc;
                      edecl_declaration = var_decl;
                    } in

  Hashtbl.add edge_hash id_edge decl_edge;
  printf "tcfa: %s\n" (pr_tcfa_edge decl_edge);
  (*       Hashtbl.add var_hash lli var; *)
  let ins_gen' = tnode_to_general ins_gen in
  let succ_gen' = tnode_to_general succ_gen in
  ins_gen'.ngen_leaving_edges <- (decl_edge :: ins_gen'.ngen_leaving_edges);
  succ_gen'.ngen_entering_edges <-(decl_edge :: succ_gen'.ngen_entering_edges)
  

  
(*-------------------------------------LLVM IR br instruction conditional branch-----------------------------------------*)
let ins2tcfa_conbr file_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash node_hash edge_hash =
  let n = num_operands lli in
  let instr = string_of_llvalue lli in
  Printf.printf "instr: %s\n" (value_name lli);
  Printf.printf "Instruction: %s\n" instr;
  printf "number of operands: %s \n" (string_of_int n);
  let cond = operand lli 0 in
  let des1 = operand lli 1 in
  let des2 = operand lli 2 in
  printf "-----branch condition: --------\n" ;
  let llty0 = type_of cond in
  let tty0 = llty_to_tty llty0 in
  print_value_kind cond;
  print_type llty0;
  printf "-----branch destination1: --------\n" ;
  let llty1 = type_of des1 in
  let tty1 = llty_to_tty llty1 in
  print_value_kind des1;
  print_type llty1;
  Printf.printf "-----des1: %s\n" (string_of_llvalue des1);
  printf "-----branch destination2: --------\n" ;
  let llty2 = type_of des2 in
  let tty2 = llty_to_tty llty2 in
  print_value_kind des2;
  print_type llty2;
  Printf.printf "-----des2: %s\n" (string_of_llvalue des2);

  let ins_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash lli) in
  let des1_gen = Hashtbl.find node_hash (Hashtbl.find b2node_hash (block_of_value des1)) in
  let des2_gen = Hashtbl.find node_hash (Hashtbl.find b2node_hash (block_of_value des2)) in

  let file_loc = getFileLocation file_name lli in
  let file_loc_cond = getFileLocation file_name cond in
  let cond_expr = getExpression cond tty0 file_name ins2id_hash id2dec_hash in
  let expr = BinExpr {
                 bin_location = file_loc;
                 bin_type = NumericType BOOL;
                 bin_operand_1 = LiteralExpr (IntLitExpr {
                                                  int_location = file_loc;
                                                  int_type = NumericType INT;
                                                  int_value = 1;
                                   });
                 bin_operand_2 = cond_expr;
                 bin_operator = EQ;
               } in

  let id_edge = incr_edge () in
  let assume_true = AssumeEdge {
                        eassume_edge_number = id_edge;
                        eassume_predecessor = ins_gen;
                        eassume_successor = des2_gen;
                        eassume_raw_statement = instr^";";
                        eassume_file_location = file_loc;
                        eassume_truth_assumption = true;
                        eassume_swapped = false;
                        eassume_artificial_intermediate = false;
                        eassume_expression = expr;
                    } in
  Hashtbl.add edge_hash id_edge assume_true;
  printf "tcfa: %s\n" (pr_tcfa_edge assume_true);
  let ins_gen' = tnode_to_general ins_gen in
  let des1_gen' = tnode_to_general des1_gen in
  ins_gen'.ngen_leaving_edges <- (assume_true :: ins_gen'.ngen_leaving_edges);
  des1_gen'.ngen_entering_edges <-(assume_true :: des1_gen'.ngen_entering_edges);
   
  let id_edge = incr_edge () in
  let assume_false = AssumeEdge {
                        eassume_edge_number = id_edge;
                        eassume_predecessor = ins_gen;
                        eassume_successor = des1_gen;
                        eassume_raw_statement = instr^";";
                        eassume_file_location = file_loc;
                        eassume_truth_assumption = false;
                        eassume_swapped = true;
                        eassume_artificial_intermediate = false;
                        eassume_expression = expr;
                    } in
  Hashtbl.add edge_hash id_edge assume_false;
  printf "tcfa: %s\n" (pr_tcfa_edge assume_false);
  let ins_gen' = tnode_to_general ins_gen in
  let des2_gen' = tnode_to_general des2_gen in
  ins_gen'.ngen_leaving_edges <- (assume_false :: ins_gen'.ngen_leaving_edges);
  des2_gen'.ngen_entering_edges <-(assume_false :: des2_gen'.ngen_entering_edges)


  
(*---------------------------------------------LLVM IR br instruction unconditional branch---------------------------------------*)
let ins2tcfa_unbr file_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash node_hash edge_hash =
  let n = num_operands lli in
  let instr = string_of_llvalue lli in
  Printf.printf "instr: %s\n" (value_name lli);
  Printf.printf "Instruction: %s\n" instr;
  printf "number of operands: %s \n" (string_of_int n);
  let des = operand lli 0 in
  printf "-----branch destination: --------\n" ;
  let llty = type_of des in
  let tty = llty_to_tty llty in
  print_value_kind des;
  print_type llty;
 
  let ins_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash lli) in
  let des_gen = Hashtbl.find node_hash (Hashtbl.find b2node_hash (block_of_value des)) in
(*  let id_var = ins2id_find ins2id_hash id2var_hash cond in
  let cond_var = Hashtbl.find id2var_hash id_var in
  let cond_dec = Hashtbl.find id2dec_hash id_var in
 *)  
  let file_loc = getFileLocation file_name lli in
  let id_edge = incr_edge () in
  let b_edge = BlankEdge {
                   eblank_edge_number = id_edge;
                   eblank_predecessor = ins_gen;
                   eblank_successor = des_gen;
                   eblank_raw_statement = instr^";";
                   eblank_file_location = file_loc;
                   eblank_description = "uncondtional branch";
                 } in
  Hashtbl.add edge_hash id_edge b_edge;
  let ins_gen' = tnode_to_general ins_gen in
  let des_gen' = tnode_to_general des_gen in
  ins_gen'.ngen_leaving_edges <- (b_edge :: ins_gen'.ngen_leaving_edges);
  des_gen'.ngen_entering_edges <-(b_edge :: des_gen'.ngen_entering_edges)

  
(*-------------------------------------------------LLVM IR Ret void instruction--------------------------------------------------*)
let ins2tcfa_ret_void file_name fun_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash node_hash edge_hash =
  let n = num_operands lli in
  let instr = string_of_llvalue lli in
  Printf.printf "instr: %s\n" (value_name lli);
  Printf.printf "Instruction: %s\n" instr;
  printf "number of operands: %s \n" (string_of_int n);

  (*  let fun_name = value_name (block_parent (instr_parent lli)) in*)
  let ins_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash lli) in
  let fun_exit = Hashtbl.find node_hash (Hashtbl.find fun_id_hash (fun_name^"_exit")) in
(*  let id_var = ins2id_find ins2id_hash id2var_hash cond in
  let cond_var = Hashtbl.find id2var_hash id_var in
  let cond_dec = Hashtbl.find id2dec_hash id_var in
 *)

  let file_loc = getFileLocation file_name lli in
  let rs = {
      ret_location = file_loc;
      ret_expression = None;
      ret_assignment = None;
    } in
  let id_edge = incr_edge () in
  let fun_exit' = tnode_to_exit fun_exit in
  let rs_edge = ReturnStatementEdge {
                   eret_edge_number = id_edge;
                   eret_predecessor = ins_gen;
                   eret_successor = fun_exit';
                   eret_raw_statement = instr^";";
                   eret_file_location = file_loc;
                   eret_raw_ast = rs;
                 } in
  Hashtbl.add edge_hash id_edge rs_edge;
  let ins_gen' = tnode_to_general ins_gen in
  ins_gen'.ngen_leaving_edges <- (rs_edge :: ins_gen'.ngen_leaving_edges);
  fun_exit'.fexit_entering_edges <-(rs_edge :: fun_exit'.fexit_entering_edges)

                                
                          

  
(*------------------------------------------LLVM IR Ret with an instruction value-------------------------------------------------------*)
let ins2tcfa_ret file_name fun_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash node_hash edge_hash =
  let n = num_operands lli in
  let instr = string_of_llvalue lli in
  Printf.printf "instr: %s\n" (value_name lli);
  Printf.printf "Instruction: %s\n" instr;
  printf "number of operands: %s \n" (string_of_int n);
  let opr = operand lli 0 in
  let llty = type_of opr in
  let tty = llty_to_tty llty in
  print_value_kind opr;
  print_type llty;

  (*  let fun_name = value_name (block_parent (instr_parent lli)) in*)
  let ins_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash lli) in
  let fun_exit = Hashtbl.find node_hash (Hashtbl.find fun_id_hash (fun_name^"_exit")) in

  let file_loc = getFileLocation file_name lli in
  let ret_expr = getExpression opr tty file_name ins2id_hash id2dec_hash in
  let rs = {
      ret_location = file_loc;
      ret_expression = Some ret_expr;
      ret_assignment = None;
    } in
  let id_edge = incr_edge () in
  let fun_exit' = tnode_to_exit fun_exit in
  let rs_edge = ReturnStatementEdge {
                   eret_edge_number = id_edge;
                   eret_predecessor = ins_gen;
                   eret_successor = fun_exit';
                   eret_raw_statement = instr^";";
                   eret_file_location = file_loc;
                   eret_raw_ast = rs;
                 } in
  Hashtbl.add edge_hash id_edge rs_edge;
  let ins_gen' = tnode_to_general ins_gen in
  ins_gen'.ngen_leaving_edges <- (rs_edge :: ins_gen'.ngen_leaving_edges);
  fun_exit'.fexit_entering_edges <-(rs_edge :: fun_exit'.fexit_entering_edges)

(*----------------------------------------LLVM IR other instructions, defualt with blank edge-------------------------------------------*)
let ins2tcfa_blank lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash node_hash edge_hash =
  let instr = string_of_llvalue lli in
  printf "instruction: \n %s \n" instr;
  let ins_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash lli) in
(*
  let opr = operand lli 0 in
  let llty = type_of opr in
  let tty = llty_to_tty llty in
 *)
  
  let ins_succ =
    (match (instr_succ lli) with
     | Before ins_s -> ins_s
     | _ -> failwith "Condn't find next instuction!"
    ) in
  let succ_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash ins_succ) in
  let id_edge = incr_edge () in
(*
  let id_var = ins2id_find ins2id_hash id2var_hash lli in
  let var = Hashtbl.find id2var_hash id_var in
 *)
  let file_loc = {
      file_name = "";
      nice_file_name = "";
      offset = 0;
      length = 0;
      starting_line = 0;
      ending_line = 0;
      starting_line_in_origin = 0;
      ending_line_in_origin = 0;
    } in
  let id_edge = incr_edge () in
  let b_edge = BlankEdge {
                   eblank_edge_number = id_edge;
                   eblank_predecessor = ins_gen;
                       eblank_successor = succ_gen;
                       eblank_raw_statement = instr^";";
                       eblank_file_location = file_loc;
                       eblank_description = "Instructions that we don't know how to convert.";
                 } in
  Hashtbl.add edge_hash id_edge b_edge;
  let ins_gen' = tnode_to_general ins_gen in
  let succ_gen' = tnode_to_general succ_gen in
  ins_gen'.ngen_leaving_edges <- (b_edge :: ins_gen'.ngen_leaving_edges);
  succ_gen'.ngen_entering_edges <-(b_edge :: succ_gen'.ngen_entering_edges)

 
  
 (*------------------------------------------LLVM IR call instruction translation-----------------------------------------------*)
let ins2tcfa_call file_name fun_name lli b2node_hash ins2node_hash fun_id_hash fun_hash node_hash edge_hash ins2id_hash id2dec_hash =
  let instr = string_of_llvalue lli in
  let illty = type_of lli in
  let itty = llty_to_tty illty in


  
  let caller_hash = Hashtbl.find node_hash fun_name in
  printf "instruction: %s \n" instr;
  printf "------------instruction value kind and type kind-------\n";
  print_value_kind lli;
  print_type illty;


  let ins_gen = Hashtbl.find caller_hash (Hashtbl.find ins2node_hash lli) in
  let ins_succ =
    (match (instr_succ lli) with
     | Before ins_s -> ins_s
     | _ -> failwith "Condn't find next instuction!"
    ) in
  let succ_gen = Hashtbl.find caller_hash (Hashtbl.find ins2node_hash ins_succ) in
  let succ_gen' = tnode_to_general succ_gen in

  let n =num_operands lli in
  printf "number of operands: %s \n" (string_of_int n);
  let callee = operand lli (n-1) in
  let callee_name = value_name callee in
  let callee_hash = Hashtbl.find node_hash callee_name in
 
  let llty = Llvm.element_type (type_of callee) in
  let tty = llty_to_tty llty in

  let ret_llty = return_type llty in
  let ret_tty = llty_to_tty ret_llty in

  printf "--------callee value kind and type kind: --------\n";
  print_value_kind callee;
  print_type llty;
 
  printf "--------callee return type: --------\n";
  print_type ret_llty;
 (* 
   (
    if (is_intrinsic callee)
    then
      printf "-----callee is an intrinsic function: %s\n " callee_name
    else
      printf "----callee is not an intrinsic function: %s\n" callee_name
  );
 (
    if (is_declaration callee)
    then
      printf "-----callee is an declaration function: %s\n " callee_name
    else
      printf "----callee is not an declaration function: %s\n" callee_name
 );
  *)

  let file_loc = getFileLocation fun_name lli in  
  let callee_entry = Hashtbl.find fun_hash (Hashtbl.find fun_id_hash callee_name) in
  let ret_decl = (match callee_entry.fentry_return_variable with
                  | Some var_decl -> var_decl
                  | None -> failwith "Viod function not handled currently in call site!") in 
  let ret_var_expr = IdExpr {
                         id_location = file_loc;
                         id_type = ret_tty;
                         id_name = fun_name^"_ret";
                         id_declaration = VariableDeclaration ret_decl;
                       } in
  
  
  let callee_exit = Hashtbl.find callee_hash (Hashtbl.find fun_id_hash (callee_name^"_exit")) in
(*
  let tcfa_dec = getAssignedTcfaDeclaration callee file_name fun_name None ins2id_hash id2dec_hash in
 *)
  let tcfa_dec = (match (Hashtbl.find_opt ins2id_hash callee) with
                  | Some id -> Hashtbl.find id2dec_hash id
                  | _ -> failwith "callee is not delcared previously!") in
  let fun_dec = to_fun_dec tcfa_dec in
  
  let callee_id_expr = getAssignedIdExpression callee file_name tty ins2id_hash id2dec_hash in
  
  let id_gen1 = incr_node () in
  let gen1 = {
      ngen_node_number = id_gen1;
      ngen_leaving_edges = [];
      ngen_entering_edges = [];
      ngen_is_loop_start = false;
      ngen_function_name = fun_name;
      ngen_leaving_summary_edge = None;
      ngen_entering_summary_edge = None;
    } in
  Hashtbl.add caller_hash id_gen1 (GeneralNode gen1);
  

(* We assume that it is always a function_call_assignment_statement in llvm  
  
  let fcs = {
      cstmt_location = file_loc;
      cstmt_expression = {
          call_location = file_loc;
          call_type = VoidType;
          call_function_name = LiteralExpr (
                                   StringLitExpr {
                                       string_location = file_loc;
                                       string_type = VoidType;
                                       string_value = callee_name;
                                 }) ;
          call_parameters = [];
          call_declaration = fun_dec;
        };
    } in  

 *)
  let id_gen2 = incr_node () in
  let gen2 = {
      ngen_node_number = id_gen2;
      ngen_leaving_edges = [];
      ngen_entering_edges = [];
      ngen_is_loop_start = false;
      ngen_function_name = fun_name;
      ngen_leaving_summary_edge = None;
      ngen_entering_summary_edge = None;
    } in
  Hashtbl.add caller_hash id_gen2 (GeneralNode gen2);

  (*------------------get callee arguments in the call site------------------------------*)
  let param_lvs = Array.to_list (params callee) in
  let param_num = List.length param_lvs in
  printf "number of parameters (in call sites): %s \n" (string_of_int param_num);
  let params_expr = List.map (fun param ->
                        LHSExpr (getAssignedIdExpression param file_name (llty_to_tty (type_of param)) ins2id_hash id2dec_hash)) param_lvs in
(*   
  let clhs = getAssignedIdExpression lli file_name itty ins2id_hash id2dec_hash in
  *)
  let crhs = CallExpr {
                 call_location = file_loc;
                 call_type = ret_tty;
                 call_function_name = LHSExpr callee_id_expr;
                 (*
                                        LiteralExpr (
                                          StringLitExpr {
                                              string_location = file_loc;
                                              string_type = ret_tty;
                                              string_value = callee_name;
                                        });*)
                 
                 call_parameters = params_expr;
                 call_declaration = fun_dec;                 
               } in
  let fcas = {
      cassign_location = file_loc;
      cassign_lhs = ret_var_expr;
      cassign_rhs = crhs;
    } in 
  let fc = (FunctionCallAssignmentStatement fcas) in

 (* 
  let id_edge = incr_edge () in
  let fss_edge = FunctionSummaryStatementEdge {
                     esstmt_edge_number = id_edge;
                     esstmt_predecessor = GeneralNode gen1;
                     esstmt_successor = GeneralNode gen2;
                     esstmt_raw_statement = instr;
                     esstmt_file_location = file_loc;
                     esstmt_statement = FuncCallAssignmentStatement fcas;
                     esstmt_function_name = callee_name;
                     esstmt_fcall = fc;
    } in
  Hashtbl.add edge_hash (id_edge) fss_edge;  
  gen1.ngen_leaving_edges <- (fss_edge :: gen1.ngen_leaving_edges);
  gen2.ngen_entering_edges <- (fss_edge :: gen2.ngen_entering_edges);
  *)
  
  let id_edge = incr_edge () in
  let fs_edge = {
      esummary_edge_number = id_edge;
      esummary_predecessor = GeneralNode gen1;
      esummary_successor = GeneralNode gen2;
      esummary_raw_statement = instr^";";
      esummary_file_location = file_loc;
      esummary_expression = fc;
      esummary_function_entry = callee_entry;
    } in
  Hashtbl.add edge_hash (id_edge) (FunctionSummaryEdge fs_edge);  
  gen1.ngen_leaving_summary_edge <- Some fs_edge;
  gen2.ngen_entering_summary_edge <- Some fs_edge;
  
  let id_edge = incr_edge () in
  let fc_edge = FunctionCallEdge {
                    ecall_edge_number = id_edge;
                    ecall_predecessor = (GeneralNode gen1);
                    ecall_successor = FunctionEntryNode callee_entry;
                    ecall_raw_statement = instr^";";
                    ecall_file_location = file_loc;
                    ecall_function_call = fc;
                    ecall_summary_edge = fs_edge;
                  } in
  Hashtbl.add edge_hash id_edge fc_edge;
  gen1.ngen_leaving_edges <- (fc_edge :: gen1.ngen_leaving_edges);
  callee_entry.fentry_entering_edges <- (fc_edge :: callee_entry.fentry_entering_edges);

  let id_edge = incr_edge () in
  let fr_edge = FunctionReturnEdge {
                    efret_edge_number = id_edge;
                    efret_predecessor = callee_exit;
                    efret_successor = GeneralNode gen2;
                    efret_raw_statement = "return to caller:" ^ fun_name^";";
                    efret_file_location = file_loc;
                    efret_summary_edge = fs_edge;
                  } in
  Hashtbl.add edge_hash id_edge fr_edge;
  let callee_exit' = tnode_to_exit callee_exit in
  callee_exit'.fexit_leaving_edges <- (fr_edge :: callee_exit'.fexit_leaving_edges);
  gen2.ngen_entering_edges <- (fr_edge :: gen2.ngen_entering_edges);

(*  
  if (is_tail_call lli)
  then ()
  else
    let id_edge = incr_edge () in
    let b_edge = BlankEdge {
                     eblank_edge_number = id_edge;
                     eblank_predecessor = (GeneralNode gen_node);
                     eblank_successor = succ_gen;
                     eblank_raw_statement = instr;
                     eblank_file_location = file_loc;
                     eblank_description = "null";
                   } in
 *)
  
  let id_edge = incr_edge () in
  let stmt_edge = StatementEdge {
                      estmt_edge_number = id_edge;
                      estmt_predecessor = (GeneralNode gen2);
                      estmt_successor = succ_gen;
                      estmt_raw_statement = instr^";";
                      estmt_file_location = file_loc;
                      estmt_statement = FuncCallAssignmentStatement fcas;
                    } in  
  Hashtbl.add edge_hash id_edge stmt_edge;
  gen2.ngen_leaving_edges <- (stmt_edge :: gen2.ngen_leaving_edges);
  succ_gen'.ngen_entering_edges <-(stmt_edge :: succ_gen'.ngen_entering_edges);


  let var_init = InitExpr {
                     init_expr_location = file_loc;
                     init_expression = LHSExpr ret_var_expr;
                   } in
  let var_decl = getAssignedTcfaDeclaration lli file_name fun_name (Some var_init) ins2id_hash id2dec_hash in 
  let id_edge = incr_edge () in
  let decl_edge = DeclarationEdge {
                      edecl_edge_number = id_edge;
                      edecl_predecessor = ins_gen;
                      edecl_successor = GeneralNode gen1;
                      edecl_raw_statement = "fresh variable for call site in llvm;";
                      edecl_file_location = file_loc;
                      edecl_declaration = var_decl;
                    } in
  Hashtbl.add edge_hash id_edge decl_edge;
  
  let ins_gen' = tnode_to_general ins_gen in
  ins_gen'.ngen_leaving_edges <- (decl_edge :: ins_gen'.ngen_leaving_edges);
  gen1.ngen_entering_edges <-(decl_edge :: gen1.ngen_entering_edges);
  
  Hashtbl.add node_hash fun_name caller_hash;
  Hashtbl.add node_hash callee_name callee_hash

    
 (*---------------------------------LLVM IR call with asm instruction conversion--------------------------------------*)  
let ins2tcfa_call_asm file_name fun_name lli b2node_hash ins2node_hash fun_id_hash fun_hash node_hash edge_hash =
  let instr = "asm_"^(string_of_llvalue lli) in
  let caller_hash = Hashtbl.find node_hash fun_name in
  printf "asm call instruction: \n %s \n" instr;
  let ins_gen = Hashtbl.find caller_hash (Hashtbl.find ins2node_hash lli) in
  let ins_succ =
    (match (instr_succ lli) with
     | Before ins_s -> ins_s
     | _ -> failwith "Condn't find next instuction!"
    ) in
  let succ_gen = Hashtbl.find caller_hash (Hashtbl.find ins2node_hash ins_succ) in
  let n =num_operands lli in
  let callee = operand lli (n-1) in
  printf "number of operands: %s \n" (string_of_int n);
  let callee_name = value_name callee in
  let llty = type_of callee in
  (*  let tty = llty_to_tty llty in*)
  print_value_kind callee;
  print_type llty;
  printf "-----callee name: %s\n " callee_name;

  let file_loc = getFileLocation file_name lli in
  let id_edge = incr_edge () in
  let b_edge = BlankEdge {
                   eblank_edge_number = id_edge;
                   eblank_predecessor = ins_gen;
                   eblank_successor = succ_gen;
                   eblank_raw_statement = instr^";";
                   eblank_file_location = file_loc;
                   eblank_description = "null";
                 } in
  Hashtbl.add edge_hash id_edge b_edge;
  let ins_gen' = tnode_to_general ins_gen in
  let succ_gen' = tnode_to_general succ_gen in
  ins_gen'.ngen_leaving_edges <- (b_edge :: ins_gen'.ngen_leaving_edges);
  succ_gen'.ngen_entering_edges <-(b_edge :: succ_gen'.ngen_entering_edges)
 
  
 (*--------------------------LLVM IR Select instruction translation--------------------------------------------*)
let ins2tcfa_select file_name fun_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash temp_hash edge_hash =
  let instr = string_of_llvalue lli in
  printf "instruction: \n %s \n" instr;
  printf "-------instruction type-------";
  let illty = type_of lli in
  let itty = llty_to_tty illty in
  print_type illty;

  let n =num_operands lli in
  printf "number of operands: %s \n" (string_of_int n);
  let cond = operand lli 0 in
  let des1 = operand lli 1 in
  let des2 = operand lli 2 in
  printf "-----select condition: --------\n" ;
  let llty0 = type_of cond in
  let tty0 = llty_to_tty llty0 in
  print_value_kind cond;
  print_type llty0;
  printf "-----select value1: --------\n" ;
  let llty1 = type_of des1 in
  let tty1 = llty_to_tty llty1 in
  print_value_kind des1;
  print_type llty1;
  Printf.printf "-----value1: %s\n" (string_of_llvalue des1);
  printf "-----select value2: --------\n" ;
  let llty2 = type_of des2 in
  let tty2 = llty_to_tty llty2 in
  print_value_kind des2;
  print_type llty2;
  Printf.printf "-----value2: %s\n" (string_of_llvalue des2);

  let ins_gen = Hashtbl.find temp_hash (Hashtbl.find ins2node_hash lli) in
  let ins_succ =
    (match (instr_succ lli) with
     | Before ins_s -> ins_s
     | _ -> failwith "Condn't find next instuction!"
    ) in
  let succ_gen = Hashtbl.find temp_hash (Hashtbl.find ins2node_hash ins_succ) in

  let file_loc = getFileLocation file_name lli in

  let id_node = incr_node () in
  let true_gen = GeneralNode {
      ngen_node_number = id_node;
      ngen_leaving_edges = [];
      ngen_entering_edges = [];
      ngen_is_loop_start = false;
      ngen_function_name = fun_name;
      ngen_leaving_summary_edge = None;
      ngen_entering_summary_edge = None;
    } in
  Hashtbl.add temp_hash id_node true_gen;

  let id_node = incr_node () in
  let false_gen = GeneralNode {
      ngen_node_number = id_node;
      ngen_leaving_edges = [];
      ngen_entering_edges = [];
      ngen_is_loop_start = false;
      ngen_function_name = fun_name;
      ngen_leaving_summary_edge = None;
      ngen_entering_summary_edge = None;
    } in
  Hashtbl.add temp_hash id_node false_gen;
 
  let cond_expr = getExpression cond tty0 file_name ins2id_hash id2dec_hash in
  let expr = BinExpr {
                 bin_location = file_loc;
                 bin_type = NumericType BOOL;
                 bin_operand_1 = LiteralExpr (IntLitExpr {
                                                  int_location = file_loc;
                                                  int_type = NumericType INT;
                                                  int_value = 1;
                                   });
                 bin_operand_2 = cond_expr;
                 bin_operator = EQ;
               } in

  let id_edge = incr_edge () in
  let assume_edge1 = AssumeEdge {
                        eassume_edge_number = id_edge;
                        eassume_predecessor = ins_gen;
                        eassume_successor = true_gen;
                        eassume_raw_statement = instr^";";
                        eassume_file_location = file_loc;
                        eassume_truth_assumption = true;
                        eassume_swapped = false;
                        eassume_artificial_intermediate = false;
                        eassume_expression = expr;
                    } in
  Hashtbl.add edge_hash id_edge assume_edge1;
  printf "tcfa: %s\n" (pr_tcfa_edge assume_edge1);
  let ins_gen' = tnode_to_general ins_gen in
  let true_gen' = tnode_to_general true_gen in
  ins_gen'.ngen_leaving_edges <- (assume_edge1 :: ins_gen'.ngen_leaving_edges);
  true_gen'.ngen_entering_edges <-(assume_edge1 :: true_gen'.ngen_entering_edges);
   
  let id_edge = incr_edge () in
  let assume_edge2 = AssumeEdge {
                        eassume_edge_number = id_edge;
                        eassume_predecessor = ins_gen;
                        eassume_successor = false_gen;
                        eassume_raw_statement = instr^";";
                        eassume_file_location = file_loc;
                        eassume_truth_assumption = false;
                        eassume_swapped = true;
                        eassume_artificial_intermediate = false;
                        eassume_expression = expr;
                    } in
  Hashtbl.add edge_hash id_edge assume_edge2;
  printf "tcfa: %s\n" (pr_tcfa_edge assume_edge2);
  let ins_gen' = tnode_to_general ins_gen in
  let false_gen' = tnode_to_general false_gen in
  ins_gen'.ngen_leaving_edges <- (assume_edge2 :: ins_gen'.ngen_leaving_edges);
  false_gen'.ngen_entering_edges <-(assume_edge2 :: false_gen'.ngen_entering_edges);

 
  let true_expr = getExpression des1 tty1 file_name ins2id_hash id2dec_hash in
  let false_expr = getExpression des2 tty2 file_name ins2id_hash id2dec_hash in
  let file_loc1 = getFileLocation file_name des1 in
  let file_loc2 = getFileLocation file_name des2 in

  (*
  (*Initializer?----*)
  let true_init = InitExpr {
                      init_expr_location = file_loc1;
                      (*id_expression*)
                      init_expression = true_expr;
                    } in
  let false_init = InitExpr {
                       init_expr_location = file_loc2;
                       (*id_expression*)
                       init_expression = false_expr;
                     } in

  let true_decl = getAssignedTcfaDeclaration lli file_name fun_name None ins2id_hash id2dec_hash in
   *)

  
  (* True Assignment *)
  let true_assignee_stmt = get_assign_stmt lli true_expr file_name fun_name ins2id_hash id2dec_hash in
  
  let id_edge = incr_edge () in
  let true_stmt_edge = StatementEdge {
                      estmt_edge_number = id_edge;
                      estmt_predecessor = true_gen;
                      estmt_successor = succ_gen;
                      estmt_raw_statement = instr^";";
                      estmt_file_location = file_loc;
                      estmt_statement = ExprAssignmentStatement true_assignee_stmt;
                    } in  
(*
  
  let id_edge = incr_edge () in
  let true_decl_edge = DeclarationEdge {
                      edecl_edge_number = id_edge;
                      edecl_predecessor = true_gen;
                      edecl_successor = succ_gen;
                      edecl_raw_statement = instr^";";
                      edecl_file_location = file_loc;
                      edecl_declaration = true_decl;
                    } in
 *)
  Hashtbl.add edge_hash id_edge true_stmt_edge;
  printf "tcfa: %s\n" (pr_tcfa_edge true_stmt_edge);
  (*       Hashtbl.add var_hash lli var; *)
  let true_gen' = tnode_to_general true_gen in
  let succ_gen' = tnode_to_general succ_gen in
  true_gen'.ngen_leaving_edges <- (true_stmt_edge :: true_gen'.ngen_leaving_edges);
  succ_gen'.ngen_entering_edges <-(true_stmt_edge :: succ_gen'.ngen_entering_edges);

  (* False assignment *)
  let false_assignee_stmt = get_assign_stmt lli false_expr file_name fun_name ins2id_hash id2dec_hash in
  
  let id_edge = incr_edge () in
  let false_stmt_edge = StatementEdge {
                      estmt_edge_number = id_edge;
                      estmt_predecessor = false_gen;
                      estmt_successor = succ_gen;
                      estmt_raw_statement = instr^";";
                      estmt_file_location = file_loc;
                      estmt_statement = ExprAssignmentStatement false_assignee_stmt;
                    } in  
(*
  let id_edge = incr_edge () in
  let false_decl_edge = DeclarationEdge {
                      edecl_edge_number = id_edge;
                      edecl_predecessor = false_gen;
                      edecl_successor = succ_gen;
                      edecl_raw_statement = instr^";";
                      edecl_file_location = file_loc;
                      edecl_declaration = false_decl;
                    } in
 *)
  
  Hashtbl.add edge_hash id_edge false_stmt_edge;
  printf "tcfa: %s\n" (pr_tcfa_edge false_stmt_edge);
  (*       Hashtbl.add var_hash lli var; *)
  let false_gen' = tnode_to_general false_gen in
  let succ_gen' = tnode_to_general succ_gen in
  false_gen'.ngen_leaving_edges <- (false_stmt_edge :: false_gen'.ngen_leaving_edges);
  succ_gen'.ngen_entering_edges <-(false_stmt_edge :: succ_gen'.ngen_entering_edges)

  
 (*LLVM IR PHI instruction translation*)
let ins2tcfa_phi file_name fun_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash node_hash edge_hash =
  let instr = string_of_llvalue lli in
  printf "instruction: \n %s \n" instr;
  printf "-------instruction type-------\n";
  let illty = type_of lli in
  let itty = llty_to_tty illty in
  print_type illty;
  
  let n =num_operands lli in
  printf "number of operands: %s \n" (string_of_int n);
  let opr0 = operand lli 0 in
  let opr1 = operand lli 1 in
  printf "-----operand 0: --------\n" ;
  Printf.printf "-----opr0: %s\n" (string_of_llvalue opr0);
  let llty0 = type_of opr0 in
  let tty0 = llty_to_tty llty0 in
  print_value_kind opr0;
  print_type llty0;
  printf "-----operand 1: --------\n" ;
  Printf.printf "-----opr 1: %s\n" (string_of_llvalue opr1);
  let llty1 = type_of opr1 in
  let tty1 = llty_to_tty llty1 in
  print_value_kind opr1;
  print_type llty1;

  let ins_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash lli) in
  let ins_succ =
    (match (instr_succ lli) with
     | Before ins_s -> ins_s
     | _ -> failwith "Condn't find next instuction!"
    ) in
  let succ_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash ins_succ) in

  let lli_expr = getExpression lli itty file_name ins2id_hash id2dec_hash in
  let file_loc = getFileLocation file_name lli in
  (*Initializer?----*)
  let var_init = InitExpr {
                     init_expr_location = file_loc;
                     (*id_expression*)
                     init_expression = lli_expr;
                   } in
  let var_decl = getAssignedTcfaDeclaration lli file_name fun_name (Some var_init) ins2id_hash id2dec_hash in
  let id_edge = incr_edge () in
  let decl_edge = DeclarationEdge {
                      edecl_edge_number = id_edge;
                      edecl_predecessor = ins_gen;
                      edecl_successor = succ_gen;
                      edecl_raw_statement = instr^";";
                      edecl_file_location = file_loc;
                      edecl_declaration = var_decl;
                    } in

  Hashtbl.add edge_hash id_edge decl_edge;
  printf "tcfa: %s\n" (pr_tcfa_edge decl_edge);
  (*       Hashtbl.add var_hash lli var; *)
  let ins_gen' = tnode_to_general ins_gen in
  let succ_gen' = tnode_to_general succ_gen in
  ins_gen'.ngen_leaving_edges <- (decl_edge :: ins_gen'.ngen_leaving_edges);
  succ_gen'.ngen_entering_edges <-(decl_edge :: succ_gen'.ngen_entering_edges)
  
 (*LLVM IR zext instruction translation*)
let ins2tcfa_zext file_name fun_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash node_hash edge_hash =
  let instr = string_of_llvalue lli in
  printf "instruction: \n %s \n" instr;
  printf "-------instruction type-------";
  let illty = type_of lli in
  let itty = llty_to_tty illty in
  print_type illty;

  let n =num_operands lli in
  printf "number of operands: %s \n" (string_of_int n);
  let opr = operand lli (n-1) in
  let llty = type_of opr in
  let tty = llty_to_tty llty in
  print_value_kind opr;
  print_type llty;

  let ins_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash lli) in
  let ins_succ =
    (match (instr_succ lli) with
     | Before ins_s -> ins_s
     | _ -> failwith "Condn't find next instuction!"
    ) in
  let succ_gen = Hashtbl.find node_hash (Hashtbl.find ins2node_hash ins_succ) in

 (* 
  let file_loc = getFileLocation file_name lli in  
  let rhs_assign = getExpression opr tty file_name ins2id_hash id2dec_hash in
  let assignee_stmt = get_assign_stmt lli rhs_assign file_name fun_name ins2id_hash id2dec_hash in
  
  let id_edge = incr_edge () in
  let stmt_edge = StatementEdge {
                      estmt_edge_number = id_edge;
                      estmt_predecessor = ins_gen;
                      estmt_successor = succ_gen;
                      estmt_raw_statement = instr^";";
                      estmt_file_location = file_loc;
                      estmt_statement = ExprAssignmentStatement assignee_stmt;
                    } in  
  Hashtbl.add edge_hash id_edge stmt_edge;
  let ins_gen' = tnode_to_general ins_gen in
  let succ_gen' = tnode_to_general succ_gen in
  ins_gen'.ngen_leaving_edges <- (stmt_edge :: ins_gen'.ngen_leaving_edges);
  succ_gen'.ngen_entering_edges <-(stmt_edge :: succ_gen'.ngen_entering_edges)

                                  *)
  


  
  let opr_expr = getExpression opr itty file_name ins2id_hash id2dec_hash in
  let file_loc = getFileLocation file_name lli in
  (*Initializer?----*)
  let var_init = InitExpr {
                     init_expr_location = file_loc;
                     (*id_expression*)
                     init_expression = opr_expr;
                   } in
  let var_decl = getAssignedTcfaDeclaration lli file_name fun_name (Some var_init) ins2id_hash id2dec_hash in
  let id_edge = incr_edge () in
  let decl_edge = DeclarationEdge {
                      edecl_edge_number = id_edge;
                      edecl_predecessor = ins_gen;
                      edecl_successor = succ_gen;
                      edecl_raw_statement = instr^";";
                      edecl_file_location = file_loc;
                      edecl_declaration = var_decl;
                    } in

  Hashtbl.add edge_hash id_edge decl_edge;
  printf "tcfa: %s\n" (pr_tcfa_edge decl_edge);
  (*       Hashtbl.add var_hash lli var; *)
  let ins_gen' = tnode_to_general ins_gen in
  let succ_gen' = tnode_to_general succ_gen in
  ins_gen'.ngen_leaving_edges <- (decl_edge :: ins_gen'.ngen_leaving_edges);
  succ_gen'.ngen_entering_edges <-(decl_edge :: succ_gen'.ngen_entering_edges)
 
