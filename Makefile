all:
	cabal configure
	cabal build
	cp ./dist/build/latc/latc .

clean:
	cabal clean
	rm ./latc
