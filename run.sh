#! /bin/sh

echo "----- Example 1: filtering -----"
# ocamlfind ocamlopt -o calls_cfa calls_cfa.ml -package radare2 yojson utils.ml tast.ml rcfa.ml -linkpkg
#oasis setup-clean
#oasis  setup -setup-update dynamic
#make clean
#make
#./llvm_tcfa.native "examples/exampleProgram/exampleProgram-llvm4.bc"
#./llvm_tcfa.native "examples/pi-meeting/origin_mcsema_4_0.bc" > temp.tcfa
#./llvm_tcfa.native "examples/pi-meeting/origin_mcsema_4_0.opt.bc" > temp.tcfa
#./llvm_tcfa.native "examples/simple1/simple1_mcsema_4_0.bc" # >examples/pi-meeting/origin.tcfa
#./llvm_tcfa.native "examples/simple2/simple2_mcsema_4_0.bc" # >temp.tcfa
#./llvm_tcfa.native "examples/simple3/simple3_mcsema_4_0.bc" #>temp.tcfa

#./llvm_tcfa.native "examples/simple10/simple10_fail_gcc81_O0_mcsema40.bc" > examples/simple10/succeed_gcc81_O0.tcfa
#cp cfa.dot examples/simple10/cfa_fail_gcc81_O0.dot

./llvm_tcfa.native "examples/simple10/simple10_fail_gcc81_O0_mcsema40.bc" #>temp.tcfa
#./llvm_tcfa.native "examples/simple10/simple10_fail_gcc81_O0_mcsema40.bc" #>temp.tcfa
#./llvm_tcfa.native "examples/simple6/simple6_succeed_81_mcsema40.bc" #>temp.tcfa
#./llvm_tcfa.native "examples/example_mcsema/example_ref_mcsema_4.0.bc" #>temp.tcfa
#./llvm_tcfa.native "examples/example_mcsema/example_switch_mcsema_4.0.bc" #>temp.tcfa
#./llvm_tcfa.native "examples/pi-meeting/modify_mcsema_4_0.bc" #>examples/pi-meeting/modify.tcfa
#./llvm_tcfa.native "../bit-Ako9/gcd01-2/a.bc"
#./llvm_tcfa.native "../bit-Ako9/Addition01-1/a.bc"
#./llvm_tcfa.native "../bit-Ako9/Binomial/a.bc"
#./llvm_tcfa.native "../bit-Ako9/TwoWay/a.bc"
#./llvm_tcfa.native "../bit-Ako9/twisted/a.bc"
#./llvm_tcfa.native "../bit-Ako9/Et1_true/a.bc"
#./llvm_tcfa.native "../bit-Ako9/Parts/a.bc"
