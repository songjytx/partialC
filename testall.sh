./partialc.native test/test3_return.pc > test.ll
llc test.ll
cc -o test.exe test.s