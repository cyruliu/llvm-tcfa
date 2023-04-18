open List 
open Tast
open Tcfa
open Llvm
open Printf
open Llou
open Btcfa
open Str
open Cpa_utils
open Thrift_server
(*
  By Cyrus@AVTA  
  Abstraction over bianry code, llvm IR bridge.
*)

                            
let main () = 
 

(*---------------------------------small tcfa testing for bug------------------------------*)

  let temp_hash' = Hashtbl.create 6 in
  let fun_hash' = Hashtbl.create 6 in
  let node_hash' = Hashtbl.create 6 in
  let edge_hash' = Hashtbl.create 6 in

  let file_loc = {
          file_name = "simple test";
          nice_file_name = "";
          offset = 0;
          length = 0;
          starting_line = 0;
          ending_line = 0;
          starting_line_in_origin = 0;
          ending_line_in_origin = 0;
    } in
  
  let fun_dec = {
      func_location = file_loc;
      func_type = FunctionType {
                      return_type = NumericType INT;
                      parameters = [];
                      takes_var_args = false;
                    };
      func_name = "main";
      func_orig_name = "main" ;
      func_is_global = true;
      func_parameters = [];
    } in
  let id_node_en = incr_node () in
  let fentry_node = {
         fentry_node_number = id_node_en;
         fentry_leaving_edges = [];
         fentry_entering_edges = [];
         fentry_is_loop_start = false;
         fentry_function_name = "main";
         fentry_leaving_summary_edge = None;
         fentry_entering_summary_edge = None;
         fentry_location = file_loc;
         fentry_function_definition = fun_dec;
         fentry_return_variable = None;
         fentry_exit_node = None;
    } in
  let id_node_gen = incr_node () in
  let gen_node = {
      ngen_node_number = id_node_gen;
      ngen_leaving_edges = [];
      ngen_entering_edges = [];
      ngen_is_loop_start = false;
      ngen_function_name = "main";
      ngen_leaving_summary_edge = None;
      ngen_entering_summary_edge = None;
    } in
  let id_edge = incr_edge () in
  let b_edge = BlankEdge {
                   eblank_edge_number = id_edge;
                   eblank_predecessor = (FunctionEntryNode fentry_node);
                   eblank_successor = (GeneralNode gen_node);
                   eblank_raw_statement = "";
                   eblank_file_location = file_loc;
                   eblank_description = "blank edge";
                 } in
  Hashtbl.add edge_hash' id_edge b_edge;
  fentry_node.fentry_leaving_edges <- (b_edge :: fentry_node.fentry_leaving_edges);
  gen_node.ngen_entering_edges <-(b_edge :: gen_node.ngen_entering_edges);
  let id_node_exit = incr_node () in
  let fexit_node = {
      fexit_node_number = id_node_exit;
      fexit_leaving_edges = [];
      fexit_entering_edges = [];
      fexit_is_loop_start = false;
      fexit_function_name = "main";
      fexit_leaving_summary_edge = None;
      fexit_entering_summary_edge = None;
      fexit_function_entry_node = None;
    } in

  let id_edge = incr_edge () in
  let b_edge = BlankEdge {
                   eblank_edge_number = id_edge;
                   eblank_predecessor = (GeneralNode gen_node);
                   eblank_successor = (FunctionExitNode fexit_node);
                   eblank_raw_statement = "";
                   eblank_file_location = file_loc;
                   eblank_description = "blank edge";
                 } in
  Hashtbl.add edge_hash' id_edge b_edge;
  gen_node.ngen_leaving_edges <- (b_edge :: gen_node.ngen_leaving_edges);
  fexit_node.fexit_entering_edges <-(b_edge :: fexit_node.fexit_entering_edges);
  Hashtbl.add temp_hash' id_node_gen (GeneralNode gen_node);


  fentry_node.fentry_exit_node <- (Some fexit_node);
  fexit_node.fexit_function_entry_node <- (Some fentry_node);

  Hashtbl.add temp_hash' id_node_en (FunctionEntryNode fentry_node);
  Hashtbl.add fun_hash' id_node_en fentry_node;
  Hashtbl.add temp_hash' id_node_exit (FunctionExitNode fexit_node);
  Hashtbl.add node_hash' "main" temp_hash';
  let tcfa_test = {
      tcfa_functions = fun_hash';
      tcfa_all_nodes = node_hash';
      tcfa_all_edges = edge_hash';
      tcfa_main_function = fentry_node;
      (*
      tcfa_main_function = Hashtbl.find fun_hash (Hashtbl.find fun_id_hash "sub_4003b0_main");
       *)
      tcfa_file_names = [];
      tcfa_language = C;
    } in


(*------------------------Export and convert tcfa_bin------------------------------------------------*)

  let oc = open_out "examples/pi-meeting/bin.tcfa" in
  fprintf oc "%s\n" (pr_tcfa tcfa_test);
  close_out oc;
  printf "\n\n------------------Printing out tcfa from main entry function------------------------------------\n\n";
  
  print_string (pr_tcfa tcfa_test);
  
(*-----------------Connecting to cpa thrift server---------------------------------------------*)
  print_dot tcfa_test;
  printf "----tcfa_dot generated!-----\n"
(*
  let reach_result = check_reachability tcfa_bin in
  printf "Reachability result: %s \n" (pr_cpa_result reach_result)
  *)  
    
let () = Printexc.record_backtrace true
let () = main ()
             
  
   
