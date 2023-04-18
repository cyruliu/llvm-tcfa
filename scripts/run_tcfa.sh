#! /bin/sh

echo "----- Running : Output -----"
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
: '
./llvm_tcfa.native "examples/simple2/simple2_succeed_gcc81_O0_opt.bc" > examples/simple2/results/simple2_succeed_gcc81_O0_opt_raw.tcfa
cp cfa.dot examples/simple2/results/cfa_simple2_succeed_gcc81_O0_opt.dot
cp bin.tcfa examples/simple2/results/simple2_succeed_gcc81_O0_opt.tcfa
'

./llvm_tcfa.native "examples/simple2/simple2_succeed_gcc81_O0_opt.bc" #>temp.tcfa

#./llvm_tcfa.native "examples/simple2/simple2_succeed_gcc81_O0_opt_debloat.bc" #>temp.tcfa

#./llvm_tcfa.native "examples/simple4/simple4_succeed_gcc81_O0_mcsema40.bc" #>temp.tcfa
#./llvm_tcfa.native "examples/simple6/simple6_succeed_81_mcsema40.bc" #>temp.tcfa
#./llvm_tcfa.native "examples/example_mcsema/example_ref_mcsema_4.0.bc" #>temp.tcfa
#./llvm_tcfa.native "examples/example_mcsema/example_switch_mcsema_4.0.bc" #>temp.tcfa
#./llvm_tcfa.native "examples/pi-meeting/modify_mcsema_4_0.bc" #>examples/pi-meeting/modify.tcfa


: '
for bc in examples/simple*/*.bc; do
    echo $bc
    path = pwd $bc
    echo $path
   # ./llvm_tcfa.native "$bc"
done
'



: '
./llvm_tcfa.native "../bit-Ako9/gcd01-2/a.bc"
./llvm_tcfa.native "../bit-Ako9/Addition01-1/a.bc"
./llvm_tcfa.native "../bit-Ako9/Binomial/a.bc"
./llvm_tcfa.native "../bit-Ako9/TwoWay/a.bc"
./llvm_tcfa.native "../bit-Ako9/twisted/a.bc"
./llvm_tcfa.native "../bit-Ako9/Et1_true/a.bc"
./llvm_tcfa.native "../bit-Ako9/Parts/a.bc"
'
