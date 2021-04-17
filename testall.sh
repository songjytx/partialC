./partialc.native test/test1_hello_world.pc > test.ll
llc test.ll
cc -o test.exe test.s