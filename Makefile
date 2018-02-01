all:
	./bin/bnfc --functor -haskell -o src/BNFC-gen/ ./src/latte.cf
	cabal configure
	cabal build
	cp ./dist/build/latc/latc .

clean:
	cabal clean
	rm ./latc
