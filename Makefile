all:
	./bin/bnfc --functor -haskell -o src/BNFC-gen/ latte.cf
	./bin/bnfc -haskell -o src/BNFC-gen/ LLVM.cf
	cabal build
	cp ./dist/build/latc/latc .

clean:
	cabal clean
	rm ./latc
