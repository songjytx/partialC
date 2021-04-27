./partialc.native test/test7_array_coinchange.pc > test.ll
llc -relocation-model=pic test.ll
cc -o test.exe test.s
./test.exe