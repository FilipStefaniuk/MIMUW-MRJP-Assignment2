all:
	bnfc -haskell -o src/BNFC-gen/ latte.cf
	cabal build
	cp ./dist/build/latc/latc .

clean:
	cabal clean
	rm ./latc
