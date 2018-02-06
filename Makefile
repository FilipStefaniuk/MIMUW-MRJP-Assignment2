all:
	clang -o ./lib/runtime.ll -emit-llvm -S ./lib/runtime.c
	llvm-as -o ./lib/runtime.bc ./lib/runtime.ll
	cabal configure
	cabal build
	cp ./dist/build/latc_llvm/latc_llvm .

clean:
	cabal clean
	rm -f ./lib/runtime.ll ./lib/runtime.bc
	rm -f ./latc_llvm
