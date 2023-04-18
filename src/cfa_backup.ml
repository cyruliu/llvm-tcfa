open Yojson.Basic.Util 
open Yojson.Basic 
open List 


open Tast
open Tcfa

(*ptint out calls and claees one by one*)   
(*  
let rec calls lj =
  match lj with
  | [] -> print_endline "finish list"
  | h :: t ->
     let nm = to_string (member "name" h) in
     let im = filter_string (to_list (member "imports" h)) in
     print_endline ("name:" ^ nm);
     print_endline ("imports:");
     List.iter print_endline (im );
     calls t
 *)

   
(*list of calls from jason*)   
let rec calls_list lj =
  match lj with
  | [] -> []
  | h :: t ->
     let nm = to_string (member "name" h) in     
     nm::(calls_list t)

(*list of callees from jason*)   
let rec callees_list lj =
  match lj with
  | [] -> []
  | h :: t ->
     let im = filter_string (to_list (member "imports" h)) in
     im :: (callees_list t)
     

let rec tcfa_gen calls callees fun_hash node_hash edge_hash cfa fun_num id_node id_edge =
  match calls, callees with
  | [],[] -> cfa
  | h1::t1, h2::t1 ->
     let fentry_node = {
         fentry_node_number = (id_node+1);
         fentry_leaving_edges = [b_edge];
         fentry_entering_edges = [];
         fentry_is_loop_start = flase;
         fentry_function_name = h1;
       }
     Hashtbl.add fun_hash fun_num fentry_node;
     Hashtbl.add node_hash (id_node+1) fentry_node;
     and b_edge = {
         eblank_edge_number = (id_edge+1);
         eblank_predecessor = fentry_node;
         eblank_successor = gen1_node;
         eblank_raw_statement = "blank";
         eblank_file_location = {};
         ebalank_description = "null";
       }
     Hashtbl.add edge_hash (id_edge+1) b_edge;           
     and gen1_node = {
         ngen_node_number = (id_node+2);
         ngen_leaving_edges = [fc_edge];
         ngen_entering_edges = [b_edge];
         ngen_is_loop_start = false;
         ngen_function_name = h1;
       }
     Hashtbl.add node_hash (id_node+2) gen1_node;
     and fc = FunctionCallStatment
                {
                  cstmt_expression = {
                    call_location = {};
                    call_type = VoidType;
                    call_function_name = LiteralExpr (
                                             StringLitExpr {
                                                 string_location = {};
                                                 string_type = VoidType;
                                                 string_value = h1;
                                           }) ;
                    call_parameters = [];
                    call_declaration = {};
                  };
                }
     and fc_edge = {
         ecall_edge_numbr = (id_edge+2);
         ecall_predecessor = gen1_node;
         ecall_successor = gen2_node;
         ecall_raw_statement = "";
         ecall_file_location = {};
         ecall_fucntion_call = fc;
         ecall_summary_edge = {};
       }
     Hashtbl.add edge_hash (id_edge+2) fc_edge;            
     and gen2_node = {
         ngen_node_number = (id_node+3);
         ngen_leaving_edges = [fr_edge];
         ngen_entering_edges = [fc_edge];
         ngen_is_loop_start = false;
         ngen_function_name = h1;
       }
     Hashtbl.add node_hash (id_node+3) gen2_node;                   
     and fr_edge = {
         efret_edge_number = (id_edge+3);
         efret_predecessor = gen2_node;
         efret_successor = gen3_node;
         efret_raw_statement = "return to callees";
         efret_file_location = {};
         efret_summary_edge = {};
       }
     Hashtbl.add edge_hash (id_edge+3) fr_edge;
     and gen3_node = {
         ngen_node_number = (id_node+4);
         ngen_leaving_edges = [rc_edge];
         ngen_entering_edges = [fr_edge];
         ngen_is_loop_start = false;
         ngen_function_name = h1;
       }
     Hashtbl.add node_hash (id_node+4) gen3_node;
     and rs_edge = {
         eret_edge_bumber = (id_edge+4);
         eret_predecessor = gen3_node;
         eret_successor = fexit_node;
         sret_raw_statement = "null";
         eret_file_location = {};
         eret_raw_ast = {
             ret_location = {};
           };
       }
     Hashtbl.add edge_hash (id_edge+4) rs_edge;
     and fexit_node = {
         fexit_node_number = (id_node+5);
         fexit_leaving_edges = [];
         fentry_entering_edges = [rs_edge];
         fentry_is_loop_start = flase;
         fentry_function_name = h1;
       }
     Hashtbl.add node_hash (id_node+5) fexit_node;


     let tcfa' ={


                        } 

         
         
          

       
let main () =
  let json = to_list (from_channel stdin) in
  (*All the alls and callees are stored correspondingly in array*)
  let calls = calls_list json in 
  let callees = callees_list json in

  let m = length calls in
  let n = length callees in
  let n_num = List.map List.length callees in
  let n_lens = map string_of_int n_num in
  let sum_callees = List.fold_left (+) 0 n_num in
  let f_num = (m + sum_callees) in

  (*print our main call and its callees*)
  print_endline ("calls number 10:" ^ calls.(9) ^ " in " ^ string_of_int m);
  print_endline ("callees number 10:" ^ (String.concat ", " callees.(9))
                 ^ " in " ^ string_of_int n);
  print_endline ("number of callees in their calls:" ^ (String.concat ", " n_lens));
  print_endline ("sum callees in total:" ^ string_of_int sum_callees)
               
(*create a tcfa form calls and callees*)
  
  let fun_hash = Hashtbl.create f_num in
  let node_hash = Hashtbl.create (f_num + m) in
  let edge_hash = Hashtbl.create f_num in

  let tcfa_bin = {
      tcfa_functions = fun_hash;
      tcfa_all_nodes = node_hash;
      tcfa_all_edges = edge_hash;
      tcfa_main_function = fen_main;
      tcfa_file_names = [];
      tcfa_language = C;
    } in
  
  tcfa_gen calls callees tcfa_bin 
   

  
let () = main () 

  

