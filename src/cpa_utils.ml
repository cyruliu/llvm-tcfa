open Thrift_server
open Printf
(*
let cpa_hostname = ref "oban.cs.stevens.edu"
 *) 
  
let cpa_hostname = ref "localhost"


let run_cpa_service f =
  try
    let () = print_endline "Connecting to Cpachecker Thrift Server ..." in
    let cpa_addr = Thrift_server.dns_query !cpa_hostname Unix.PF_INET in
    let cpa_cli = Thrift_server.connect CpaService ~host:cpa_addr 9090 in
    let cpa = Thrift_server.get_cpa_client cpa_cli.client in
    printf "-------got cpa client :)------\n";

    let result = f cpa in
    let () = cpa_cli.trans#close in
    result
  with Thrift.Transport.E _ as e ->
    (print_endline "Cannot connect to CpaThriftServer";
     print_endline ("Exception: " ^ (Printexc.to_string e));
     raise e)

let check_reachability tcfa =
  let cfa = Trans_thrift.from_tcfa tcfa in
  printf "-------Transfered to cfa------\n";
  run_cpa_service (fun cpa -> cpa#reachability cfa)

let print_dot tcfa =
  let cfa = Trans_thrift.from_tcfa tcfa in
  let content = run_cpa_service (fun cpa -> cpa#print_dot cfa) in
  let oc = open_out "cfa.dot" in
  fprintf oc "%s" content;
  close_out oc

let pr_cpa_result trv =
  match trv with
  | Cresult_types.TVResult.RES_TRUE -> "TRUE"
  | Cresult_types.TVResult.RES_FALSE -> "FALSE"
  | Cresult_types.TVResult.RES_UNKNOWN -> "UNKNOWN"
  | Cresult_types.TVResult.RES_ERROR -> "ERROR"
