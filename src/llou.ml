open Llvm
open Llvm.Opcode
open Lldebug
open Tast
open Tcfa
open Printf
(*open Batteries*)
   
(*
  By Cyrus@AVTA  
  ocaml llvm binding utilities, linking to tcfa utilities.
 *)

   
(*generate a list of natural numbers in ocaml*)
let rec unfold_right f init =
  match f init with
  | None -> []
  | Some (x, next) -> x :: unfold_right f next

let range n =
  let irange x = if x > n then None else Some (x, x+1) in
  unfold_right irange 1
   
   
(*global counters for node id and edge id and variable id*)
let node_ctr = ref 0
let edge_ctr = ref 0
let var_ctr = ref 0
let struc_ctr = ref 0             
  
let incr_node () =
  node_ctr := !node_ctr + 1;
  !node_ctr

let incr_edge () =
  edge_ctr := !edge_ctr +1;
  !edge_ctr

let incr_var () =
  var_ctr := !var_ctr +1;
  !var_ctr

let incr_struct () =
  var_ctr := !var_ctr +1;
  !var_ctr


  
 (*
let varhash_add hash key bind =
  match (Hashtbl.find_opt hash key) with
  | bd -> ()../
  | None -> Hashtbl.add hash key bind
  *)
  
let ins2id_find ins2id_hash ins =
  match (Hashtbl.find_opt ins2id_hash ins) with
  | Some id -> id
  | None ->
     let id_var = incr_var () in
     Hashtbl.add ins2id_hash ins id_var;
     id_var

(* Rip off tast declaration constructors*)
let to_para_dec dec =
  match dec with
  | ParameterDeclaration p_dec -> p_dec
  | _  -> failwith "Expecting parameter delaration!"

let to_fun_dec dec =
  match dec with
  | FunctionDeclaration f_dec -> f_dec
  | _ -> failwith "Expecting function delaration!"

let to_var_dec dec =
  match dec with
  | VariableDeclaration v_dec -> v_dec
  | _ -> failwith "Expecting variable delaration!"
  
                            
(* llvm opcode to c binary operations *)     
let ins_binary_op opcode =
  match opcode with
  | Llvm.Opcode.Add | Llvm.Opcode.FAdd -> PLUS
  | Llvm.Opcode.Sub | Llvm.Opcode.FSub -> MINUS
  | Llvm.Opcode.Mul | Llvm.Opcode.FMul -> MUL
  | Llvm.Opcode.UDiv | Llvm.Opcode.SDiv | Llvm.Opcode.FDiv -> DIV
  | Llvm.Opcode.URem | Llvm.Opcode.SRem | Llvm.Opcode.FRem -> MODULO
  | Llvm.Opcode.AShr | Llvm.Opcode.LShr -> SHIFT_RIGHT
  | Llvm.Opcode.Shl -> SHIFT_LEFT
  | Llvm.Opcode.And -> AND
  | Llvm.Opcode.Or -> OR
  | Llvm.Opcode.Xor -> XOR
  | _ -> failwith "Unhandled operation from llvm to tcfa"

let icmp_to_bin icmp =
  match icmp with
  | Llvm.Icmp.Eq -> EQ
  | Llvm.Icmp.Ne -> NE
  | Llvm.Icmp.Ugt 
  | Llvm.Icmp.Sgt -> GT
  | Llvm.Icmp.Uge 
  | Llvm.Icmp.Sge -> GE
  | Llvm.Icmp.Ult 
  | Llvm.Icmp.Slt -> LT
  | Llvm.Icmp.Ule
  | Llvm.Icmp.Sle -> LE


(*-----------name of type-----*)
let name_of_llty llty =
  let tyk = Llvm.classify_type llty in
  match tyk with
  | Llvm.TypeKind.Integer -> "Integer"
  | Llvm.TypeKind.Void   -> "Void"
  | Llvm.TypeKind.Half -> "Half"
  | Llvm.TypeKind.Double -> "Double"
  | Llvm.TypeKind.X86fp80 -> "X86fp80"
  | Llvm.TypeKind.Fp128 -> "Fp128"
  | Llvm.TypeKind.Ppc_fp128 -> "Ppc_fp128"
  | Llvm.TypeKind.Float   -> "Float"
  | Llvm.TypeKind.Function  -> "Function"
  | Llvm.TypeKind.Array -> "Array"
  | Llvm.TypeKind.Vector -> "Vector"
  | Llvm.TypeKind.Pointer -> "Pointer"
  | Llvm.TypeKind.Struct ->
     let name = (match (struct_name llty) with
                | Some str -> str
                | None -> "no_name") in
    "Struct_"^name
  | Llvm.TypeKind.Label -> "Label"
  | Llvm.TypeKind.Metadata -> "Metadata"
  | Llvm.TypeKind.X86_mmx -> "X86_mmx"
  | _         -> failwith "Unknown type kind" 
                     
       
(*type converter from llvm to tcfa types*)
let rec llty_to_tty llty =
  let ty = Llvm.classify_type llty in
  match ty with
  | Llvm.TypeKind.Integer -> NumericType INT
  | Llvm.TypeKind.Void   -> VoidType
  | Llvm.TypeKind.Half
  | Llvm.TypeKind.Double
  | Llvm.TypeKind.X86fp80
  | Llvm.TypeKind.Fp128
  | Llvm.TypeKind.Ppc_fp128
  | Llvm.TypeKind.Float   -> NumericType FLOAT
  | Llvm.TypeKind.Function  ->
     let param_ty = Array.to_list (param_types llty) in
     let tparam_ty = List.map llty_to_tty param_ty in
     FunctionType {
         return_type = llty_to_tty (return_type llty);
         parameters = tparam_ty;
         takes_var_args = is_var_arg llty;
       }
  | Llvm.TypeKind.Array ->
     let element_ty = element_type llty in
     let array_len = array_length llty in
     ArrayType {
         array_element_type = llty_to_tty element_ty;
         array_length = array_len;
       }
  | Llvm.TypeKind.Vector ->
     let element_ty = element_type llty in
     let array_len = array_length llty in
     ArrayType {
         array_element_type = llty_to_tty element_ty;
         array_length = array_len;
       }
 | Llvm.TypeKind.Pointer ->
    let element_ty = element_type llty in
    PointerType {
        pointer_is_const = false;
        pointer_is_volatile = false;
        pointer_data_type = llty_to_tty element_ty;
      }    
 | Llvm.TypeKind.Struct ->
    let name = (match (struct_name llty) with
                | Some str ->
                   str
                   (*
                   let field_num = incr_struct () in
                   str^(string_of_int field_num)
                    *) 
                | None -> "No_name"
                   (*
                   let field_num = incr_struct () in
                   "struct_"^(string_of_int field_num)
                    *)
               ) in
    let llvm_element_typs = Array.to_list (struct_element_types llty) in
    let element_decls = List.map (fun llty ->
                            let field_num = incr_struct () in
                            {
                              struct_member_type = llty_to_tty llty;
                              struct_member_name = "struct_member"^(string_of_int field_num);
                            }
                          ) llvm_element_typs in
    StructType {
        struct_member_declarations = element_decls;
        struct_name = name;
        struct_orig_name = name;
        struct_is_const = false;
        struct_is_volatile = false;
      }
  | Llvm.TypeKind.Label
  | Llvm.TypeKind.Metadata
  | Llvm.TypeKind.X86_mmx -> VoidType  (* null *)
  | _         -> failwith "Unhandled type kind" 




(*-------------------General tcfa expressions instantiation--------------------------------*)
               
(*File location*)               
let getFileLocation file_name lv =
  (*  let llv_name = value_name lv in*)
  {
    file_name = file_name;
    nice_file_name = file_name;
    offset = 0;
    length = 1;
    starting_line = 0;
    ending_line = 0;
    starting_line_in_origin = 0;
    ending_line_in_origin = 0;
  }

(* tcfa defualt exprssion for llvm opcodes that we don't know how to translate *)
let getDefualtExp lli pFileName =
  let instr = string_of_llvalue lli in
  let opr = operand lli 0 in
  let llty = type_of opr in
  let tty = llty_to_tty llty in
  print_value_kind opr;
  print_type llty;
  let file_loc = getFileLocation pFileName lli in
  LiteralExpr (StringLitExpr {
                   string_location = file_loc;
                   string_type = tty;
                   string_value = instr;
    }) (* to do---complicated case *)


(* expressions for frithmetic instructions*)
let rec createFromArithmeticOp lli opcode pFileName ins2id_hash id2dec_hash =
  let ity = type_of lli in
  let itty = llty_to_tty ity in
  let opr1 = operand lli 0 in
  let opr2 = operand lli 1 in
  printf "-----create from arithmetic op -> operand1: --------\n" ;
  let llty1 = type_of opr1 in
  let tty1 = llty_to_tty llty1 in
  print_value_kind opr1;
  print_type llty1;
  let operand1Exp = getExpression opr1 tty1 pFileName ins2id_hash id2dec_hash in 
  Printf.printf "opr1: %s\n" (string_of_llvalue opr1);
  printf "-----create from arithmetic op -> operand2: --------\n" ;
  let llty2 = type_of opr2 in
  let tty2 = llty_to_tty llty2 in
  print_value_kind opr2;
  print_type llty2;
  let operand2Exp = getExpression opr2 tty2 pFileName ins2id_hash id2dec_hash in
  
  let operation = ins_binary_op opcode in
  BinExpr {
      bin_location = getFileLocation pFileName lli;
      bin_type = itty;
      bin_operand_1 = operand1Exp;
      bin_operand_2 = operand2Exp;
      bin_operator = operation;
    }

  
(*----------------------- tcfa exprssion for llvm GetElementPtr opcode -----------------------------*)
and createGetElementPtrExp lli file_name ins2id_hash id2dec_hash =
  let instr = string_of_llvalue lli in
  let illty = type_of lli in
  let itty = llty_to_tty illty in
  
  let n = num_operands lli in
  let start_ptr = operand lli 0 in
  let start_llty = type_of start_ptr in
  let start_tty = llty_to_tty start_llty in
  print_value_kind start_ptr;
  print_type start_llty;

  let file_loc = getFileLocation file_name lli in
  let dum_expr = getExpression lli itty file_name ins2id_hash id2dec_hash in
  dum_expr


(*
  let start_expr = getExpression start_ptr start_tty file_name ins2id_hash id2dec_hash in
  start_expr

  let current_type = ref start_tty in
  let current_expr = ref start_expr in

  if n >= 2
  then
    (let index = range (n-1) in
     List.iter (fun i ->
         let index_lv = operand lli i in
         printf "index value: %s /n" (string_of_llvalue index_lv);
         let index_llty = type_of index_lv in
         let index_tty = llty_to_tty index_llty in
         let index_expr = getConstant index_lv index_tty file_name in
         match (!current_type) with
         | PointerType ptr_ty ->
            let bin_expr = BinExpr {
                               bin_location = file_loc;
                               bin_type = !current_type;
                               bin_operand_1 = !current_expr;
                               bin_operand_2 = index_expr;
                               bin_operator = PLUS;                               
                             } in
            current_expr := (LHSExpr (PointerExpr {
                                          pointer_location = file_loc;
                                          pointer_type = !current_type;
                                          pointer_operand = bin_expr
                            }));
            current_type := ptr_ty.pointer_data_type            
         | ArrayType arr_ty ->
            current_expr := (LHSExpr (ArrayExpr {
                                          arr_location = file_loc;
                                          arr_type = !current_type;
                                          arr_expression = !current_expr;
                                          arr_subscript_expression = index_expr;
                            }));
            current_type := arr_ty.array_element_type
         | StructType struct_ty ->
            let member_decls = Array.of_list (struct_ty.struct_member_declarations) in
            let struct_index = (match index_expr with
                                | LiteralExpr IntLitExpr int_expr -> int_expr.int_value
                                | _ -> failwith "Index in elementptr should be an int_lit_expr :)") in
            let filed_decl = Array.get member_decls struct_index in
            let str_field = filed_decl.struct_member_name in
            current_expr := (LHSExpr (FieldReferenceExpr {
                                          field_location = file_loc;
                                          field_type = !current_type;
                                          field_name = str_field;
                                          field_expression = !current_expr;
                                          field_is_pointer_dereference = false;
                            }));
            current_type := filed_decl.struct_member_type           
         | _ -> failwith "Unhandled type in elementptr oerand 0!"
       ) index;
     !current_expr
    )
  else
    failwith ("Too few operands in GEP operation: "^(string_of_int n))
 *)
  
(*
      LiteralExpr (StringLitExpr {
                   string_location = file_loc;
                   string_type = tty;
                   string_value = ("to_be_initialized: "^instr);
  })
    *)
(* to do---complicated case *)

(* tcfa exprssion for llvm BitCast opcode *)
and createBitcast lli pFileName ins2id_hash id2dec_hash =
  let instr = string_of_llvalue lli in
  let insty = type_of lli in
  let instty = llty_to_tty insty in
  let opr = operand lli 0 in
  let llty = type_of opr in
  let tty = llty_to_tty llty in
  print_value_kind opr;
  print_type llty;
  let file_loc = getFileLocation pFileName lli in
  CastExpr {
      cast_location = getFileLocation pFileName lli;
      cast_type = tty;
      cast_operand = getExpression opr tty pFileName ins2id_hash id2dec_hash;
      cast_dst_type = instty;
    } (* to refine --- more complicated case *)

  
(*create exprssion for opcode instructions*)
and createFromOpCode lli pFileName opcode ins2id_hash id2dec_hash =
  match opcode with    
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
  | Llvm.Opcode.Xor ->
     createFromArithmeticOp lli opcode pFileName ins2id_hash id2dec_hash
  | Llvm.Opcode.GetElementPtr ->
     createGetElementPtrExp lli pFileName ins2id_hash id2dec_hash
  | Llvm.Opcode.BitCast ->
     createBitcast lli pFileName ins2id_hash id2dec_hash
  | Llvm.Opcode.PtrToInt
  | Llvm.Opcode.IntToPtr ->
     let n = num_operands lli in
     printf "number of operands: %s \n" (string_of_int n);

     let instty = llty_to_tty (type_of lli) in
     let opr = operand lli 0 in
     let tty = llty_to_tty (type_of opr) in
     CastExpr {
         cast_location = getFileLocation pFileName lli;
         cast_type = tty;
         cast_operand = getExpression opr tty pFileName ins2id_hash id2dec_hash;
         cast_dst_type = instty;
       }
     
  | Llvm.Opcode.Trunc
  | Llvm.Opcode.ZExt ->
     getDefualtExp lli pFileName
      
  | _ -> failwith ("Unsupported opertation to tcfa expression! -> "^(string_of_llvalue lli))

(*create null expression*)      
and getNull file_loc =
  LiteralExpr (IntLitExpr {
                   int_location = file_loc;
                   int_type = NumericType INT;
                   int_value = 0;
    })
  

(*------ use the largest types available in c and throw an exception if the values are too large?...

and getTypeforInteger cons_val =



 ----------*)  




  
(*create constant exprssion *)
and getConstant lli tty file_name ins2id_hash id2dec_hash =
  let opr_kind = classify_value lli in
  let file_loc = getFileLocation file_name lli in
  (match opr_kind with
   | ConstantInt ->
      let constantValue =
        (match (int64_of_const lli) with
         | Some n -> n
         | None -> failwith "Not a constant interger!"
        ) in
      printf "constant in llvm : %s\n" (string_of_llvalue lli);
      printf "its value in ocaml: %s\n" (string_of_int (Int64.to_int constantValue));
      
      LiteralExpr (IntLitExpr {
                      int_location = file_loc;
                      int_type = tty;
                      int_value = Int64.to_int constantValue;
                     }
        )
   | ConstantPointerNull ->
      let null_expr = getNull file_loc in
      LHSExpr (PointerExpr {
                   pointer_location = file_loc;
                   pointer_type = tty;
                   pointer_operand = null_expr;
        })
   | ConstantExpr ->
      getExpression lli tty file_name ins2id_hash id2dec_hash
   | UndefValue ->
      let illty = type_of lli in
      let itty = llty_to_tty illty in
      let undef_name = "__VERIFIER_undef_"^(string_of_llvalue lli) in
      let undef_decl = VariableDeclaration {
                           var_location = file_loc;
                           var_type = itty;
                           var_name = undef_name;
                           var_orig_name = undef_name;
                           var_is_global = true;
                           var_qualified_name = undef_name;
                           var_initializer = None;
                         } in
      LHSExpr (IdExpr {
                   id_location = file_loc;
                   id_type = itty;
                   id_name = undef_name;
                   id_declaration = undef_decl;
        })
   (*| GlobalConstant*)
   | ConstantArray | ConstantDataArray | GlobalVariable ->
      let id_expr = getAssignedIdExpression lli file_name tty ins2id_hash id2dec_hash in
      LHSExpr id_expr
   | _ -> failwith ("Unhandled constant value kinds: ->"^ (string_of_llvalue lli))
  )
  
(*create lhs -> I exprssion*)
and getAssignedIdExpression lli file_name tty ins2id_hash id2dec_hash =
  let file_loc = getFileLocation file_name lli in
  let illty = type_of lli in
  let itty = llty_to_tty illty in
  printf "---- instruction value kind and type kind-------\n";
  print_value_kind lli;
  print_type illty;
  printf "----function name: %s -----\n " (value_name lli);
    
  (match (Hashtbl.find_opt ins2id_hash lli) with
   | Some id_var ->
      let decl = Hashtbl.find id2dec_hash id_var in
      (match decl with
       | VariableDeclaration v_dec ->
          let var = v_dec.var_name in
          let tty_dec = v_dec.var_type in
          IdExpr {
                id_location = file_loc;
                id_type = tty_dec;
                id_name = var;
                id_declaration = decl;
              }
       | ParameterDeclaration p_decl ->
          let var = p_decl.param_name in
          let tty_dec = p_decl.param_type in
          IdExpr {
                id_location = file_loc;
                id_type = tty_dec;
                id_name = var;
                id_declaration = decl;
              }
       | FunctionDeclaration f_decl ->
          let var = f_decl.func_name in
          let tty_dec = f_decl.func_type in
          IdExpr {
                id_location = file_loc;
                id_type = tty;
                id_name = var;
                id_declaration = decl;
              }
     (*     else (match tty with
                | PointerType tty_ptr->
                   IdExpr {
                       id_location = file_loc;
                       id_type = tty_ptr.pointer_data_type;
                       id_name = var;
                       id_declaration = decl;
                     }
                | _ -> failwith "Function type not matched!"
    *)
     )     
   | None ->
      let new_decl = getAssignedTcfaDeclaration lli file_name "" None ins2id_hash id2dec_hash in
      (match new_decl with
       | VariableDeclaration v_dec ->
          let var = v_dec.var_name in
          let tty_dec = v_dec.var_type in
          IdExpr {
              id_location = file_loc;
              id_type = tty_dec;
              id_name = var;
              id_declaration = new_decl;
            }
       | ParameterDeclaration p_decl ->
          let var = p_decl.param_name in
          let tty_dec = p_decl.param_type in
          IdExpr {
              id_location = file_loc;
              id_type = tty;
              id_name = var;
              id_declaration = new_decl;
            }
       | FunctionDeclaration f_decl ->
          let var = f_decl.func_name in
          let tty_dec = f_decl.func_type in          
          IdExpr {
              id_location = file_loc;
              id_type = tty;
              id_name = var;
              id_declaration = new_decl;
            }
      )
  )
 (* we assume all the function value has a type of pointer to function_type in llvm *)
(*          else (match tty with
                | PointerType tty_ptr->
                   IdExpr {
                       id_location = file_loc;
                       id_type = tty_ptr.pointer_data_type;
                       id_name = var;
                       id_declaration = new_decl;
                     }
                | _ -> failwith "Function type not matched (in new_decl)!")
*) 



      (*     failwith ("Id hasn't been declared yet: -> "^(string_of_llvalue lli))  *)
  

(*get exprssion for opcode *)
and getExpression opr tty pFileName ins2id_hash id2dec_hash =
  let opr_kind = classify_value opr in
  match opr_kind with
  | ConstantExpr -> createFromOpCode opr pFileName (constexpr_opcode opr) ins2id_hash id2dec_hash
  | ConstantInt | ConstantFP | ConstantPointerNull | ConstantStruct
    -> getConstant opr tty pFileName ins2id_hash id2dec_hash
  | _ -> LHSExpr (getAssignedIdExpression opr pFileName tty ins2id_hash id2dec_hash)


(*-----------------------Get tcfa declarations-------------------------------------- *)
and getAssignedTcfaDeclaration lli file_name fun_name init ins2id_hash id2dec_hash =
  match (Hashtbl.find_opt ins2id_hash lli) with
  | Some id -> Hashtbl.find id2dec_hash id 
  | None ->
     let id_var = ins2id_find ins2id_hash lli in
     let var = "V_"^(string_of_int id_var) in
     let isGlobal = (match (classify_value lli) with
                     | Llvm.ValueKind.GlobalAlias
                     | Llvm.ValueKind.Function
                     | Llvm.ValueKind.GlobalVariable -> true
                     | _ -> false
                    ) in
     let lli_kind = classify_value lli in
     let llty = type_of lli in
     let tty = llty_to_tty llty in

     let file_loc = getFileLocation file_name lli in
     (match lli_kind with
      | Llvm.ValueKind.Argument ->
         printf "---------parameters type ---------:%s:" fun_name;
         print_type llty;
         let param_decl = ParameterDeclaration {
                            param_location = file_loc;
                            param_type = tty;
                            param_name =("param_"^var);
                            param_orig_name = ("param_"^var);
                            param_qualified_name = (fun_name^"::"^"param_"^var);
                          } in
         Hashtbl.add id2dec_hash id_var param_decl;
         param_decl
      | Llvm.ValueKind.Function ->
         let llty_fun = element_type (type_of lli) in
         let tty_fun = llty_to_tty llty_fun in
        
         let param_lvs = Array.to_list (params lli) in
         let param_num = List.length param_lvs in
         printf "number of parameters (in function declaration): %s \n" (string_of_int param_num);
  
         let params_decl = List.map (fun param ->
                               let decl = getAssignedTcfaDeclaration param file_name fun_name init ins2id_hash id2dec_hash in
                               (match decl with
                                | ParameterDeclaration p_decl -> p_decl
                                | _ -> failwith ("Not a function argument delcaration: -> "^ string_of_llvalue param)
                               )
                             ) param_lvs in         
         let fun_decl = FunctionDeclaration {
                            func_location = file_loc;
                            func_type = tty_fun;
                            func_name = fun_name;
                            func_orig_name = fun_name;
                            func_is_global = isGlobal;
                            func_parameters = params_decl;
                          } in
         Hashtbl.add id2dec_hash id_var fun_decl;
         fun_decl
      | _ ->
         let var_tty = ((*match (instr_opcode lli) with
                        | Alloca ->
                           (match tty with
                            | PointerType t -> t.pointer_data_type 
                            | _ -> failwith "for alloca instruction, we are expecting pointer type reult!")   
                        | _ -> *)tty
                       ) in
         let var_decl = VariableDeclaration {
                            var_location = file_loc;
                            var_type = var_tty;
                            var_name = var;
                            var_orig_name = var;
                            var_is_global = isGlobal;
                            var_qualified_name = (fun_name^"::"^var);
                            var_initializer = init;
                          } in
         Hashtbl.add id2dec_hash id_var var_decl;
         var_decl
     )

(*-----------------------Get tcfa assign statement-------------------------------------- *)
let get_assign_stmt lli ass_expr file_name fun_name ins2id_hash id2dec_hash =
  let illty = type_of lli in
  let itty = llty_to_tty illty in
  let file_loc = getFileLocation file_name lli in
  let lhs_assignee = getExpression lli itty file_name ins2id_hash id2dec_hash in

  let expr_ass_stmt = {
      eassign_location = file_loc;
      eassign_lhs = (match itty with
                     | PointerType ptr -> PointerExpr {
                                              pointer_location = file_loc;
                                              pointer_type = itty;
                                              pointer_operand = lhs_assignee;
                                            }
                     | NumericType INT -> (match lhs_assignee with
                                           | LHSExpr lid_expr -> lid_expr
                                           | _ -> failwith "expecting an id expression for assignee.")
                     | _ -> failwith "Assingee type: pointer type expected."
                    );
      eassign_rhs = Expr ass_expr;
    } in
  expr_ass_stmt

  
(*
  let assignee_decl = getAssignedTcfaDeclaration lli file_name fun_name None ins2id_hash id2dec_hash in
  let ins_expr = get
                        
  let assignee = PointerExpr {
                     pointer_location = file_loc;
                     pointer_type = itty;
                     pointer_operand = ins_expr;
                   } in
 *)

 (* 
  match (Hashtbl.find_opt ins2id_hash lli) with
  | Some id ->
     let assignee_id_expr = getAssignedIdExpression lli file_name itty ins2id_hash id2dec_hash in
     {
       eassign_location = file_loc;
       eassign_lhs = assignee_id_expr;
       eassign_rhs = expr;                                         
     }     
  | None -> failwith ("store to an undeclared id/var! ->"^(string_of_llvalue lli))

  *)


(*tcfa nodes transform*)            
let tnode_to_termination tcfa_node =
  match tcfa_node with
  | TerminationNode n -> n
  | _ -> failwith "expected a tcfa_termination node"

let tnode_to_entry tcfa_node =
  match tcfa_node with
  | FunctionEntryNode n -> n
  | _ -> failwith "expected a tcfa_function entry node"
     
let tnode_to_exit tcfa_node =
  match tcfa_node with
  | FunctionExitNode n -> n
  | _ -> failwith "expected a tcfa_function exit node"
     
let tnode_to_general tcfa_node =
  match tcfa_node with
  | GeneralNode n -> n
  | _ -> failwith "expected a tcfa_genral node"




(*------------functions that used for global vriables---------------*)           
let global_var global2var_hash lli =
  let id_var = incr_var () in
  Hashtbl.add global2var_hash lli id_var;
  global2var_hash

  
let getLength lli =
  let illty = type_of lli in
  let itty = llty_to_tty illty in
  match itty with
  | ArrayType arrty -> arrty.array_length
  | StructType struty ->
     let element_decls = struty.struct_member_declarations in
     List.length element_decls
  | _ -> failwith "not a ConstantAggregate, couldn't get its size!"


let rec getZeroInitializer lli tty file_name =
  let file_loc = getFileLocation file_name lli in
  match tty with
  | ArrayType arrty->
     let length = arrty.array_length in
     let element_ty = arrty.array_element_type in
     let zeroInitializer = getZeroInitializer lli element_ty file_name in
     let initializers = List.map (fun i -> zeroInitializer) (range (length-1)) in
     InitList {
         init_list_location = file_loc;
         init_list = initializers;
       }     
  | StructType struty ->
     let members = struty.struct_member_declarations in
     let initializers = List.map (fun m ->
                            let memberType = m.struct_member_type in
                            let memberInit = getZeroInitializer lli memberType file_name in
                            memberInit
                          ) members in
     InitList {
         init_list_location = file_loc;
         init_list = initializers;
       }       
  | _ ->
     let  zeroExpression =
       (match tty with
        | NumericType FLOAT ->
           LiteralExpr (FloatLitExpr {
               float_location = file_loc;
               float_type = tty;
               float_value = 0.0; (*float 0*)
             })
        | NumericType _ ->
           LiteralExpr (IntLitExpr {
               int_location = file_loc;
               int_type = tty;
               int_value = 0;
             })
        | _ ->
           let int_expr = LiteralExpr (IntLitExpr {
                                           int_location = file_loc;
                                           int_type = tty;
                                           int_value = 0;
                            }) in
           CastExpr {
               cast_location = file_loc;
               cast_type = tty;
               cast_operand = int_expr;
               cast_dst_type = tty;
             }
       ) in
     InitExpr {
         init_expr_location = file_loc;
         init_expression = zeroExpression;
       }
     
     
       
let rec getConstantAggregateInitializer lli file_name ins2id_hash id2dec_hash=
  let length = getLength lli in
  let init_list = range (length-1) in
  let val_kind = classify_value lli in
  let illty = type_of lli in
  let itty = llty_to_tty illty in
  let file_loc = getFileLocation file_name lli in
  let initializers = List.map (fun index ->
                         let element = (match val_kind with
                                        | ConstantArray | ConstantStruct ->
                                           operand lli index
                                        | _ ->
                                           const_element lli index
                                       ) in
                         let file_loc = getFileLocation file_name element in
                         let ele_kind = classify_value element in
                         let ellty = type_of element in
                         let etty = llty_to_tty ellty in
                         let elementInitializer = (match ele_kind with
                                                   | ConstantArray | ConstantVector | ConstantStruct ->
                                                      getConstantAggregateInitializer element file_name ins2id_hash id2dec_hash
                                                   | ConstantAggregateZero ->
                                                      getZeroInitializer element itty file_name
                                                   | _ ->
                                                      let ele_expr = getConstant element etty file_name ins2id_hash id2dec_hash in
                                                      InitExpr {
                                                          init_expr_location = file_loc;
                                                          init_expression = ele_expr;
                                                        }
                                                  ) in
                         elementInitializer
                       ) init_list in
  InitList {
      init_list_location = file_loc;
      init_list = initializers;
    }



 
(*-----------------------------------get all the callees from a funciton value---------------------------------------*) 
let fun_callee llm caller callee =
  let fv = (match (lookup_function caller llm) with
            | Some lv -> lv
            | None -> failwith "Function does not exist in llvm module!"
           ) in
  fold_left_blocks
    (fun callee llbb ->
      fold_left_instrs
        (fun callee lli ->
          let opcty = instr_opcode lli in
          (match opcty with
           | Llvm.Opcode.Call ->
              let n =num_operands lli in              
              let opr = operand lli (n-1) in
              let opr_name = value_name opr in
              if (List.mem opr_name callee)
              then
                callee
              else
                callee @ [opr_name]
           | _ -> callee
          )
        ) callee llbb
    ) callee fv

let rec list_dup ls =
  match ls with
  | [] -> ls
  | h :: t ->
     if (List.mem h t)
     then
       list_dup t
     else
       h :: (list_dup t)

    
(*------------------------------get the call graph for the entire binary llvm module------------------------- *)  
let llcg cg_hash llm =
  iter_functions
    (fun lv ->
      let callees = fun_callee llm (value_name lv) [] in
      Hashtbl.add cg_hash (value_name lv) (list_dup callees)
    ) llm
 
(* extract call graph for main entry function *)
let rec llcg_main cg_hash llm caller callee =
  match callee with
  | [] -> Hashtbl.add cg_hash caller callee
  | h :: t ->
     Hashtbl.add cg_hash caller (list_dup callee);
     List.iter
       (fun caller' ->
         let callee' = fun_callee llm caller' [] in
         llcg_main cg_hash llm caller' (list_dup callee')
       ) callee
     
 
                         
 
