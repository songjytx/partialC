# What is this
partialC with "Hello World" test file. Currently we can see the generated llvm machine code if we run the following.

# How to build?
ocamlbuild partialc.native -pkgs llvm,llvm.analysis

# How to run?
./partialc.native test/hello_world.pc 
