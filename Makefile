all:
	./bin/bnfc --functor -haskell -o src/BNFC-gen/ ./src/latte.cf
	./bin/bnfc -haskell -o src/BNFC-gen/ ./src/LLVM.cf
	cabal build
	cp ./dist/build/latc/latc .

clean:
	cabal clean
	rm ./latc
