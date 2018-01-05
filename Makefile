all:
	bnfc -haskell -o src/BNFC-gen/ latte.cf
	bnfc -haskell -o src/BNFC-gen/ LLVM.cf
	cabal build
	cp ./dist/build/latc/latc .

clean:
	cabal clean
	rm ./latc
