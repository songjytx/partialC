# ocamlbuild partialc.native -pkgs llvm,llvm.analysis
for file in ./test/*.pc; 
	do
		echo "=========================="
		echo $file
		./partialc.native $file > test.ll
		llc -relocation-model=pic test.ll
		cc -o test.exe test.s
		./test.exe
	done