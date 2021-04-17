# ocamlbuild partialc.native -pkgs llvm,llvm.analysis
./partialc.native test/print_int.pc > test.ll
llc -relocation-model=pic test.ll
cc -o test.exe test.s
./test.exe