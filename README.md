
This is an OCaml Module that translates LLVM Instructions into a control flow automaton, that is compatible for the Static Analysis Framework [CPAChecker](https://cpachecker.sosy-lab.org/), this is a sub-module of the project, do not compile as it has dependency libraries, which are private. Check the `src` instead. 

## Install llvm-4.0 & ocaml llvm-4.0 binding
```
sudo apt install llvm-4.0 cmake
opam init
opam switch 4.05.0
opam install llvm.4.0.0
opam install thrift oasis

```
 
## Compile and run llvm-tcfa
```
cd llvm-tcfa
oasis setup
make clean
make 
./run.sh (you can edit this script to change the example you want to test)

```
 
