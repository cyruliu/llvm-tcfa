open List 
open Tast
open Tcfa
open Llou
open Itcfa
open Llvm
open Lldebug
open Printf
   
(*
  By Cyrus@AVTA  
  This module is buliding up tcfa nodes and edges for llvm bitecode.
*)
          
(*initial for all functions*)
let fun_init file_name callers llm cg_hash fun_id_hash fun_hash node_hash edge_hash ins2id_hash id2dec_hash =
  List.iter
    (fun caller ->
      let fun_llv =
       (match (lookup_function caller llm) with
        | Some lv -> lv
        | None -> failwith "Not a function name in the llvm module!"
       )in
      let temp_hash = Hashtbl.create 6 in
      let file_loc = getFileLocation file_name fun_llv in

      let tcfa_dec = getAssignedTcfaDeclaration fun_llv file_name caller None ins2id_hash id2dec_hash in
      let fun_dec = to_fun_dec tcfa_dec in

      let ret_llty = return_type (type_of fun_llv) in
      let ret_tty = llty_to_tty ret_llty in

      let id_node_en = incr_node () in
      let fentry_node = (match (classify_type ret_llty) with
                         | Llvm.TypeKind.Void ->
                            {
                              fentry_node_number = id_node_en;
                              fentry_leaving_edges = [];
                              fentry_entering_edges = [];
                              fentry_is_loop_start = false;
                              fentry_function_name = caller;
                              fentry_leaving_summary_edge = None;
                              fentry_entering_summary_edge = None;
                              fentry_location = file_loc;
                              fentry_function_definition = fun_dec;
                              fentry_return_variable = None;
                              fentry_exit_node = None;
                            }
                         | _ ->
                            let var_decl = {
                                var_location = file_loc;
                                var_type = ret_tty;
                                var_name = caller^"_ret";
                                var_orig_name = caller^"_ret";
                                var_is_global = true;
                                var_qualified_name = (caller^"::"^caller^"_ret");
                                var_initializer = None;
                              } in
                            {
                              fentry_node_number = id_node_en;
                              fentry_leaving_edges = [];
                              fentry_entering_edges = [];
                              fentry_is_loop_start = false;
                              fentry_function_name = caller;
                              fentry_leaving_summary_edge = None;
                              fentry_entering_summary_edge = None;
                              fentry_location = file_loc;
                              fentry_function_definition = fun_dec;
                              fentry_return_variable = Some var_decl;
                              fentry_exit_node = None;
                            }
                        ) in
      Hashtbl.add fun_id_hash caller id_node_en;    
      
      let id_node_gen = incr_node () in
      let gen_node = {
         ngen_node_number = id_node_gen;
         ngen_leaving_edges = [];
         ngen_entering_edges = [];
         ngen_is_loop_start = false;
         ngen_function_name = caller;
         ngen_leaving_summary_edge = None;
         ngen_entering_summary_edge = None;
        } in
      Hashtbl.add fun_id_hash (caller^"_gen") id_node_gen;
      Hashtbl.add temp_hash id_node_gen (GeneralNode gen_node);

      let id_edge = incr_edge () in
      let b_edge = BlankEdge {
                       eblank_edge_number = id_edge;
                       eblank_predecessor = (FunctionEntryNode fentry_node);
                       eblank_successor = (GeneralNode gen_node);
                       eblank_raw_statement = "fun_init_entry;";
                       eblank_file_location = file_loc;
                       eblank_description = "fun_init blank edge";
                     } in
      Hashtbl.add edge_hash id_edge b_edge;
      fentry_node.fentry_leaving_edges <- (b_edge :: fentry_node.fentry_leaving_edges);
      gen_node.ngen_entering_edges <-(b_edge :: gen_node.ngen_entering_edges);

      let id_node_exit = incr_node () in
      let fexit_node = {
          fexit_node_number = id_node_exit;
          fexit_leaving_edges = [];
          fexit_entering_edges = [];
          fexit_is_loop_start = false;
          fexit_function_name = caller;
          fexit_leaving_summary_edge = None;
          fexit_entering_summary_edge = None;
          fexit_function_entry_node = None;
        } in
      fentry_node.fentry_exit_node <- (Some fexit_node);
      fexit_node.fexit_function_entry_node <- (Some fentry_node);

      Hashtbl.add temp_hash id_node_en (FunctionEntryNode fentry_node);
      Hashtbl.add fun_hash id_node_en fentry_node;

      Hashtbl.add fun_id_hash (caller^"_exit") id_node_exit;
      Hashtbl.add temp_hash id_node_exit (FunctionExitNode fexit_node);
      Hashtbl.add node_hash caller temp_hash;
    )
    callers
  

(*close flow for declaration functions as they don't have any block inside*)  
let tcfa_dec_fun file_name fun_lv fun_id_hash fun_hash node_hash edge_hash =
  let fun_name = value_name fun_lv in
  let temp_hash = Hashtbl.find node_hash fun_name in
  let fun_gen = Hashtbl.find temp_hash (Hashtbl.find fun_id_hash (fun_name^"_gen")) in
  let fun_exit = Hashtbl.find temp_hash (Hashtbl.find fun_id_hash (fun_name^"_exit")) in
  
  let file_loc = getFileLocation file_name fun_lv in
  let id_edge = incr_edge () in
  let b_edge = BlankEdge {
                   eblank_edge_number = id_edge;
                   eblank_predecessor = fun_gen;
                   eblank_successor = fun_exit;
                   eblank_raw_statement = ";";
                   eblank_file_location = file_loc;
                   eblank_description = "blank edge";
                 } in
  Hashtbl.add edge_hash id_edge b_edge;
  let fun_gen' = tnode_to_general fun_gen in
  let fun_exit' = tnode_to_exit fun_exit in
  fun_gen'.ngen_leaving_edges <- (b_edge :: fun_gen'.ngen_leaving_edges);
  fun_exit'.fexit_entering_edges <-(b_edge :: fun_exit'.fexit_entering_edges);
  Hashtbl.add node_hash fun_name temp_hash

(*close flow for inrinsic functions as they don't have any block inside*)  
let tcfa_intrinsic_fun file_name fun_lv fun_id_hash fun_hash node_hash edge_hash =
  let fun_name = value_name fun_lv in
  printf "-------intrinsic function name iterated: %s\n" fun_name;
  let temp_hash = Hashtbl.find node_hash fun_name in
  let fun_gen = Hashtbl.find temp_hash (Hashtbl.find fun_id_hash (fun_name^"_gen")) in
  let fun_exit = Hashtbl.find temp_hash (Hashtbl.find fun_id_hash (fun_name^"_exit")) in
  let id_node_gen = incr_node () in
  let gen_node = {
      ngen_node_number = id_node_gen;
      ngen_leaving_edges = [];
      ngen_entering_edges = [];
      ngen_is_loop_start = false;
      ngen_function_name = fun_name;
      ngen_leaving_summary_edge = None;
      ngen_entering_summary_edge = None;
    } in
  Hashtbl.add fun_id_hash (fun_name^"intrinsic") id_node_gen;
  Hashtbl.add temp_hash id_node_gen (GeneralNode gen_node);

  let file_loc = getFileLocation file_name fun_lv in
  let id_edge = incr_edge () in
  let var_decl = {
      var_location = file_loc;
      var_type = NumericType INT;
      var_name = "x_intrinsic";
      var_orig_name = "" ;
      var_is_global = false;
      var_qualified_name = fun_name;
      var_initializer = None;
    } in
  let decl_edge = DeclarationEdge {
                      edecl_edge_number = id_edge;
                      edecl_predecessor = fun_gen;
                      edecl_successor = GeneralNode gen_node;
                      edecl_raw_statement = "intrinsic variable x;";
                      edecl_file_location = file_loc;
                      edecl_declaration = VariableDeclaration var_decl;
                    } in

  Hashtbl.add edge_hash id_edge decl_edge;
  let fun_gen' = tnode_to_general fun_gen in
  fun_gen'.ngen_leaving_edges <- (decl_edge :: fun_gen'.ngen_leaving_edges);
  gen_node.ngen_entering_edges <-(decl_edge :: gen_node.ngen_entering_edges);

  let ret_expr = LHSExpr (IdExpr {
                              id_location = file_loc;
                              id_type = var_decl.var_type;
                              id_name = var_decl.var_name;
                              id_declaration = VariableDeclaration var_decl;
                   }) in
  let rs = {
      ret_location = file_loc;
      ret_expression = Some ret_expr;
      ret_assignment = None;
    } in
  let id_edge = incr_edge () in
  let fun_exit' = tnode_to_exit fun_exit in
  let rs_edge = ReturnStatementEdge {
                   eret_edge_number = id_edge;
                   eret_predecessor = GeneralNode gen_node;
                   eret_successor = fun_exit';
                   eret_raw_statement = "inrinsic dummy body;";
                   eret_file_location = file_loc;
                   eret_raw_ast = rs;
                 } in
  Hashtbl.add edge_hash id_edge rs_edge;
  gen_node.ngen_leaving_edges <- (rs_edge :: gen_node.ngen_leaving_edges);
  fun_exit'.fexit_entering_edges <-(rs_edge :: fun_exit'.fexit_entering_edges);
  Hashtbl.add node_hash fun_name temp_hash

 
(*--------linker for basic blocks in a function-----*)  
let blinker file_name fun_lv b2node_hash ins2node_hash fun_id_hash fun_hash node_hash edge_hash =
  let fun_name = value_name fun_lv in
  let temp_hash = Hashtbl.find node_hash fun_name in
  Llvm.iter_blocks
    (fun llbb ->
      let id_node = incr_node () in
      let gen_node = {
          ngen_node_number = id_node;
          ngen_leaving_edges = [];
          ngen_entering_edges = [];
          ngen_is_loop_start = false;
          ngen_function_name = fun_name;
          ngen_leaving_summary_edge = None;
          ngen_entering_summary_edge = None;
        } in
      Hashtbl.add b2node_hash llbb id_node;
      Hashtbl.add temp_hash id_node (GeneralNode gen_node);
      iter_instrs
        (fun lli ->
          let id_node = incr_node () in
          let gen_node1 = {
              ngen_node_number = id_node;
              ngen_leaving_edges = [];
              ngen_entering_edges = [];
              ngen_is_loop_start = false;
              ngen_function_name = fun_name;
              ngen_leaving_summary_edge = None;
              ngen_entering_summary_edge = None;
            } in
          Hashtbl.add ins2node_hash lli id_node;
          Hashtbl.add temp_hash id_node (GeneralNode gen_node1);
        ) llbb
    ) fun_lv;

  (*create blank edge to link the function entry node and exit node with their corresponding blocks*)
  let fun_gen = Hashtbl.find temp_hash (Hashtbl.find fun_id_hash (fun_name^"_gen")) in
  let fun_exit = Hashtbl.find temp_hash (Hashtbl.find fun_id_hash (fun_name^"_exit")) in
  printf "look for the first block of the function: %s\n" fun_name;
(*  Hashtbl.iter (fun key value -> printf "b2node_hash keys: {%s} \n" (string_of_llvalue (value_of_block key))) b2node_hash;
 *)
  (*bug testing*)
  let tmp =  (Hashtbl.find b2node_hash (entry_block fun_lv)) in
  let block_entry = Hashtbl.find temp_hash tmp in
  let block_end =
        (match (block_end fun_lv) with
         |After end_block -> Hashtbl.find temp_hash (Hashtbl.find b2node_hash end_block)
         | _  -> failwith "block_end not found!") in
  
  let file_loc1 = getFileLocation file_name fun_lv in
  let id_edge = incr_edge () in
  let b_edge1 = BlankEdge {
      eblank_edge_number = id_edge;
      eblank_predecessor = fun_gen;
      eblank_successor = block_entry;
      eblank_raw_statement = "entry;";
      eblank_file_location = file_loc1;
      eblank_description = "Function start edge";
    } in
  Hashtbl.add edge_hash id_edge b_edge1;
  let fun_gen' = tnode_to_general fun_gen in
  let block_entry' = tnode_to_general block_entry in
  fun_gen'.ngen_leaving_edges <- (b_edge1 :: fun_gen'.ngen_leaving_edges);
  block_entry'.ngen_entering_edges <-(b_edge1 :: block_entry'.ngen_entering_edges);     

(*the last block conect to function exit node? no need since there is always a ret instuction at the end.*)
  (*  
  let file_loc2 = getFileLocation file_name fun_lv in
  let id_edge = incr_edge () in
  let b_edge2 = BlankEdge {
      eblank_edge_number = id_edge;
      eblank_predecessor = block_end;
      eblank_successor = fun_exit;
      eblank_raw_statement = "";
      eblank_file_location = file_loc2;
      eblank_description = "null";
    } in
  Hashtbl.add edge_hash (id_edge) b_edge2;
  let block_end' = tnode_to_general block_end in
  let fun_exit' = tnode_to_exit fun_exit in
  block_end'.ngen_leaving_edges <- (b_edge2 :: block_end'.ngen_leaving_edges);
  fun_exit'.fexit_entering_edges <-(b_edge2 :: fun_exit'.fexit_entering_edges);
 *)
  Hashtbl.add node_hash fun_name temp_hash

 
let tcfa_def_fun file_name fun_lv b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash node_hash edge_hash =
  (*initially create a general node for all the basic blocks, and link the basic block with its id through hash table*)
  let fun_name = value_name fun_lv in
  let fun_ty = type_of fun_lv in
  let param_tys = param_types fun_ty in
  let param_lv = params fun_lv in

  blinker file_name fun_lv b2node_hash ins2node_hash fun_id_hash fun_hash node_hash edge_hash;
  (*inspect instructions for each basic block*)   
  let temp_hash = Hashtbl.find node_hash fun_name in
  
  Llvm.iter_blocks
    (fun llbb ->
      let ins_begin = instr_begin llbb in
      (match ins_begin with
       | Before bb_begin ->
          let block_gen = Hashtbl.find temp_hash (Hashtbl.find b2node_hash llbb) in
          let ins_gen = Hashtbl.find temp_hash (Hashtbl.find ins2node_hash bb_begin) in
          let file_loc = getFileLocation file_name fun_lv in
          let id_edge = incr_edge () in
          let b_edge = BlankEdge {
              eblank_edge_number = id_edge;
              eblank_predecessor = block_gen;
              eblank_successor = ins_gen;
              eblank_raw_statement = "label_to_first;";
              eblank_file_location = file_loc;
              eblank_description = "edge to first instr";
            } in
          Hashtbl.add edge_hash id_edge (b_edge);
          let block_gen' = tnode_to_general block_gen in
          let ins_gen' = tnode_to_general ins_gen in
          block_gen'.ngen_leaving_edges <- (b_edge :: block_gen'.ngen_leaving_edges);
          ins_gen'.ngen_entering_edges <-(b_edge :: ins_gen'.ngen_entering_edges)
       | _ -> failwith "Couldn't find the begining instruciton!" 
      );
      printf "Instructions for block:\n ";
      Llvm.iter_instrs
        (fun lli ->
          let t = instr_opcode lli in
          printf "-----------------------------;\n";
          printf "instruction value kind: \n";
          print_value_kind lli;          
(*creating nodes and edges for different instructions*)          
          (match t with

           (*---- Memory Operators -----*)
           | Llvm.Opcode.Alloca ->
              ins2tcfa_alloca file_name fun_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash temp_hash edge_hash;
              Hashtbl.add node_hash fun_name temp_hash             
           | Llvm.Opcode.Store ->
              ins2tcfa_store file_name fun_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash temp_hash edge_hash;
              Hashtbl.add node_hash fun_name temp_hash
           | Llvm.Opcode.Load ->
              ins2tcfa_load file_name fun_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash temp_hash edge_hash;
              Hashtbl.add node_hash fun_name temp_hash
           (* Arithmetic operations *)
           | Llvm.Opcode.Add             
           | Llvm.Opcode.FAdd 
           | Llvm.Opcode.Sub 
           | Llvm.Opcode.FSub
           | Llvm.Opcode.Mul 
           | Llvm.Opcode.FMul
           | Llvm.Opcode.UDiv 
           | Llvm.Opcode.SDiv
           | Llvm.Opcode.FDiv
           | Llvm.Opcode.URem
           | Llvm.Opcode.SRem
           | Llvm.Opcode.FRem
           | Llvm.Opcode.AShr
           | Llvm.Opcode.LShr
           | Llvm.Opcode.Shl 
           | Llvm.Opcode.And
           | Llvm.Opcode.Or 
           | Llvm.Opcode.Xor
           | Llvm.Opcode.GetElementPtr
           | Llvm.Opcode.IntToPtr
           | Llvm.Opcode.PtrToInt
           | Llvm.Opcode.BitCast ->
              ins2tcfa_arithmetic file_name fun_name t lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash temp_hash edge_hash;
              Hashtbl.add node_hash fun_name temp_hash
           | Llvm.Opcode.Call ->
              let n = num_operands lli in
              let callee = operand lli (n-1) in
              let callee_name = value_name callee in
              let callee_kind = classify_value callee in
                (match callee_kind with
                 | ValueKind.Function ->
                    if (callee_name = "")
                    then
                      failwith "This is a function poitner, we are not handling it. :("
                    else
                      ins2tcfa_call file_name fun_name lli b2node_hash ins2node_hash fun_id_hash fun_hash node_hash edge_hash ins2id_hash id2dec_hash;
                 | ValueKind.InlineAsm -> ins2tcfa_call_asm file_name fun_name lli b2node_hash ins2node_hash fun_id_hash fun_hash node_hash edge_hash;
                 | _ -> failwith "Unexpected callee operand!"
                )  

           (*  Terminator Instructions       *)         
           | Llvm.Opcode.Br ->
              Printf.printf "Terminator instruction: %s\n" (string_of_llvalue lli);
              let n = num_operands lli in
              printf "number of operands: %s \n" (string_of_int n);
              if (is_conditional lli)
              then
                (ins2tcfa_conbr file_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash temp_hash edge_hash;
                 Hashtbl.add node_hash fun_name temp_hash
               )
              else
                (ins2tcfa_unbr file_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash temp_hash edge_hash;
                 Hashtbl.add node_hash fun_name temp_hash
                )
           | Llvm.Opcode.Ret ->
              printf "------------return-ins -------------\n";
              Printf.printf "Terminator instruction: %s\n" (string_of_llvalue lli);
              let n = num_operands lli in
              printf "number of operands: %s \n" (string_of_int n);
              if n = 1
              then
               (ins2tcfa_ret file_name fun_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash temp_hash edge_hash;
                Hashtbl.add node_hash fun_name temp_hash
               )
              else
                (ins2tcfa_ret_void file_name fun_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash temp_hash edge_hash;
                 Hashtbl.add node_hash fun_name temp_hash
                )

           (* Other Operators  *)
           | Llvm.Opcode.ICmp ->
              ins2tcfa_icmp file_name fun_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash temp_hash edge_hash;
              Hashtbl.add node_hash fun_name temp_hash
           | Llvm.Opcode.Select ->
              ins2tcfa_select file_name fun_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash temp_hash edge_hash;
              Hashtbl.add node_hash fun_name temp_hash
           | Llvm.Opcode.PHI ->
              ins2tcfa_phi file_name fun_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash temp_hash edge_hash;
              Hashtbl.add node_hash fun_name temp_hash
           | Llvm.Opcode.Trunc
           | Llvm.Opcode.SExt
           | Llvm.Opcode.ZExt ->
              ins2tcfa_zext file_name fun_name lli b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash temp_hash edge_hash;
              Hashtbl.add node_hash fun_name temp_hash

           | _ -> failwith ("Unhandling instructions:"^(string_of_llvalue lli))  
              (*
              ins2tcfa_blank lli b2node_hash ins2node_hash ins2id_hash id2var_hash id2dec_hash fun_id_hash fun_hash node_hash edge_hash
               *)      
          )
        )
        llbb
    )
    fun_lv

  (*
let tcfa_gen llm b2node_hash ins2node_hash ins2id_hash id2var_hash id2dec_hash global2var_hash fun_id_hash fun_hash node_hash edge_hash =
   *)
  
let tcfa_gen file_name callers llm fun_id_hash fun_hash node_hash edge_hash ins2id_hash id2dec_hash =
  List.iter
    (fun caller ->
      let fun_lv = (match (lookup_function caller llm) with
                | Some lv -> lv
                | None -> failwith "Function does not exist in llvm module!"
               ) in
      let b2node_hash = Hashtbl.create 6 in
      let ins2node_hash = Hashtbl.create 6 in
      
      (*
      let fun2args_hash = Hashtbl.create 6 in
      let params_len = 0 in
      let fun_name = value_name fun_lv in
      iter_params ( fun arg ->
                    let n = params_len + 1 in
                    Hashtbl.add fun2args_hash arg fun_name^"_arg_"^(string_of_int n)
        ) fun_lv; 
       *)
          
      if (is_intrinsic fun_lv)
      then
        tcfa_intrinsic_fun file_name fun_lv fun_id_hash fun_hash node_hash edge_hash
      else if (is_declaration fun_lv)
      then
        tcfa_dec_fun file_name fun_lv fun_id_hash fun_hash node_hash edge_hash
      else
        tcfa_def_fun file_name fun_lv b2node_hash ins2node_hash ins2id_hash id2dec_hash fun_id_hash fun_hash node_hash edge_hash 
    ) callers
                      
