# ocamlbuild partialc.native -pkgs llvm,llvm.analysis
./partialc.native test/test2_assign_string.pc > test.ll
llc -relocation-model=pic test.ll
cc -o test.exe test.s
./test.exe