open Llvm
open Tast
open Tcfa
open Printf
open Llvm.ValueKind
(*
  By Cyrus@AVTA  
  Functions help iterate llvm IR.
 *)

(*identify llvm value kind and type kind*)   
let rec print_type llty =
  let ty = Llvm.classify_type llty in
  match ty with
  | Llvm.TypeKind.Function -> Printf.printf "  Function-type \n"
  | Llvm.TypeKind.Pointer  -> Printf.printf "  Pointer to :" ; print_type (Llvm.element_type llty);
  | Llvm.TypeKind.Struct -> Printf.printf "  Stuct-type \n" 
  | Llvm.TypeKind.Array -> Printf.printf "  Array-type \n" 
  | Llvm.TypeKind.Metadata -> Printf.printf "  Metadata-type \n"
  | Llvm.TypeKind.Integer -> Printf.printf "  Integer-type \n"
  | Llvm.TypeKind.Label   -> Printf.printf "  Label-type \n"
  | Llvm.TypeKind.Void   -> Printf.printf "  Void-type \n"
  | Llvm.TypeKind.Float   -> Printf.printf "  Float-type \n"
  | Llvm.TypeKind.Vector  -> Printf.printf "  Vector-type \n"
  | _                      -> Printf.printf "  other-type\n"
                           
let rec print_value_kind lv =
  let llvkind = Llvm.classify_value lv in
  match llvkind with
  | NullValue -> Printf.printf "  Null-Value \n"
  | Llvm.ValueKind.Argument -> Printf.printf "  Argument-value \n"
  | Function -> Printf.printf "  Fuction-Value \n" 
  | ConstantArray -> Printf.printf "  ConstantArrary-value \n" 
  | ConstantDataArray -> Printf.printf "  ConstantDataArray-value \n" 
  | ConstantDataVector -> Printf.printf "  ConstantDataVector-value \n" 
  | ConstantStruct -> Printf.printf "  ConstantStruct-value \n" 
  | ConstantAggregateZero -> printf "ConstantAggregateZero-vale \n"
  | ConstantVector -> Printf.printf "  ConstantVector-value \n" 
  | ConstantInt -> Printf.printf "  ConstantInt-value \n" 
  | ConstantExpr -> Printf.printf "  ConstantExpr-value \n" 
  | GlobalVariable -> Printf.printf "  Global variable-value \n" 
  | ConstantFP -> Printf.printf "  Constant Function Pointer-value \n" 
  | ConstantPointerNull -> Printf.printf "  Constant Pointer Null-value \n"
  | GlobalAlias -> Printf.printf "  GlobalAlias-value \n"
  | InlineAsm -> Printf.printf "  InlineAsm-value \n"
  | BlockAddress -> Printf.printf "  BlockAddress-value \n"
  | BasicBlock -> Printf.printf "  BasicBlock-value \n"
  | MDNode -> Printf.printf "  MDNode-value \n"
  | MDString -> Printf.printf "  MDString-value \n"
  | UndefValue -> Printf.printf "  UndefValue-value \n"
  | Instruction _    -> printf " Instruction value kind \n"
(* | _ -> failwith "value kind not exists in llvm ocaml bindings :("*)

       
       
(*---------------------remove duplicate elements in a list and preserve its order------------------------------------ *)
let uniq_cons x xs = if List.mem x xs then xs else x :: xs
let rm_r xs = List.fold_right uniq_cons xs []


(* -------------------------------------------look up funciton value in llvm module-------------------------------*)        
let print_val lv =
  Printf.printf "Value: %s\n" (string_of_llvalue lv) ;
  Printf.printf "  name %s\n" (Llvm.value_name lv) ;
  print_value_kind lv;
  let llty = Llvm.type_of lv in
  Printf.printf "  type %s\n" (Llvm.string_of_lltype llty) ;
  print_type llty ;
  ()
  
let fun_find name llm =
  let opt_lv = Llvm.lookup_function "main" llm in
  match opt_lv with
  | Some lv -> lv
  | None    -> failwith "function not found int the llvm module"



(*-----------------------------functions help deguging-------------------------------------*)  
let print_fun lv =
  Printf.printf "function %s\n" (value_name lv);
  (*  let en_bb = entry_block lv in *)
  
  Llvm.iter_blocks
    (fun llbb ->
      let blv = value_of_block llbb in
      let bllty = type_of blv in
      Printf.printf "\nbb_name: %s\n" (Llvm.value_name (Llvm.value_of_block (llbb))) ;
      print_value_kind blv;
      print_type bllty;
      printf "label for llbb: %s\n" (string_of_lltype bllty);
      let block_addv = block_address lv llbb in
      printf "address for llbb: %s\n" (value_name block_addv);
      Llvm.iter_instrs
        (fun lli ->
(*          let n = num_operands lli in
          Printf.printf " number of operands: " (string_of_int n);*)
          let t = instr_opcode lli in
          Printf.printf "instr: %s\n" (value_name lli);
          let lltyi = type_of lli in
          print_value_kind lli;
          print_type lltyi;
          (match t with
           | Llvm.Opcode.Alloca ->
              Printf.printf "Alocate instruction: %s\n" (string_of_llvalue lli);        
              let n = num_operands lli in
              printf "number of operands: %s \n" (string_of_int n);
              let opr = operand lli (n-1) in
              printf "operand value:  %s\n" (string_of_llvalue opr);
              let llty = type_of opr in
              print_value_kind opr;
              print_type llty
              (*let callee_use = operand_use lli n in *)
          | Llvm.Opcode.Call ->
              Printf.printf "Call instruction: %s\n" (string_of_llvalue lli);
              let n =num_operands lli in              
              printf "number of operands: %s \n" (string_of_int n);
              let callee = operand lli (n-1) in
              printf "callee:  %s\n" (value_name callee);
              (* printf "operand value:  %s\n" (string_of_llvalue callee);*)
              let llty = type_of callee in
              print_value_kind callee;
              print_type llty
              (*let callee_use = operand_use lli n in *)
           | Llvm.Opcode.Load ->
              Printf.printf "Load instruction: %s\n" (string_of_llvalue lli);
              let n =num_operands lli in              
              printf "number of operands: %s \n" (string_of_int n);
              (*let callee_use = operand_use lli n in *)
              let opr = operand lli (n-1) in
              printf "operand value:  %s\n" (string_of_llvalue opr);
              let llty = type_of opr in
              print_value_kind opr;
              print_type llty
           | Llvm.Opcode.Store ->
              Printf.printf "Store instruction: %s\n" (string_of_llvalue lli);
              let n = num_operands lli in
              printf "number of operands: %s \n" (string_of_int n);
              let opr = operand lli (n-1) in
              (* printf "operand value:  %s\n" (string_of_llvalue opr);*)
              let llty = type_of opr in
              print_value_kind opr;
              print_type llty
           | Llvm.Opcode.ICmp ->
              Printf.printf "ICmp instruction: %s\n" (string_of_llvalue lli);
              let n = num_operands lli in
              printf "number of operands: %s \n" (string_of_int n);
              let opr = operand lli (n-1) in
              printf "operand value:  %s\n" (string_of_llvalue opr);
              let llty = type_of opr in
              print_value_kind opr;
              print_type llty
              (*let callee_use = operand_use lli n in *)
           | Llvm.Opcode.Add ->
              Printf.printf "Add instruction: %s\n" (string_of_llvalue lli);
              let n = num_operands lli in
              printf "number of operands: %s \n" (string_of_int n);
              let opr = operand lli (n-1) in
              printf "operand value:  %s\n" (string_of_llvalue opr);
              let llty = type_of opr in
              print_value_kind opr;
              print_type llty
           | Llvm.Opcode.Br ->
              Printf.printf "Terminator instruction: %s\n" (string_of_llvalue lli);
              let n = num_operands lli in
              printf "number of operands: %s \n" (string_of_int n);
              let opr = operand lli (n-1) in
              (*printf "operand value:  %s\n" (string_of_llvalue opr);*)
              let llty = type_of opr in
              let opr0 = operand lli 0 in
              print_value_kind opr;
              print_type llty;
              if opr0 = opr
              then
                printf "True, same oprands!"
              else
                printf "False, different oprands!"
           | Llvm.Opcode.Ret ->
              Printf.printf "Terminator instruction: %s\n" (string_of_llvalue lli);
              let n = num_operands lli in
              printf "number of operands: %s \n" (string_of_int n);
              let opr = operand lli (n-1) in
              (*  printf "operand value:  %s\n" (string_of_llvalue opr);*)
              let llty = type_of opr in
              let opr0 = operand lli 0 in
              print_value_kind opr;
              print_type llty;
              if opr0 = opr
              then
                printf "True, same oprands!"
              else
                printf "False, different oprands!"
            
           | _ ->  printf "Not a concerned instruction, ingnore! \n"        
          )

       (*   if t = Llvm.Opcode.Call
          then
            let n =num_operands lli in
            printf "number of operands: %s \n" (string_of_int n);
            (*let callee_use = operand_use lli n in *)    
            Printf.printf "    Call instruction: %s\n" (string_of_llvalue lli)
            (*printf "Callee: %s\n" (string_of_llvalue (used_value callee_use))*)
            
          else
            printf "Not a call instruciton, ingnore! \n" *)
        )
        llbb
    )
    lv

                


                
