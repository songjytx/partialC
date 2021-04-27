./partialc.native test/test8_struct.pc > test.ll
llc -relocation-model=pic test.ll
cc -o test.exe test.s
./test.exe