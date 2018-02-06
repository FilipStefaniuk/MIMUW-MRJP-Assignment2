all:
	clang -o ./lib/runtime.ll -emit-llvm -S ./lib/runtime.c
	llvm-as -o ./lib/runtime.bc ./lib/runtime.ll
	cabal configure
	cabal build
	cp ./dist/build/latc/latc .

clean:
	cabal clean
	rm ./lib/runtime.ll ./lib/runtime.bc
	rm ./latc
