package = bf
match =
flags =

configure:
	cabal configure -v0
	rm cabal.project.local*

build: configure
	cabal build -v0
	cp `find dist-newstyle/ -executable -type f -name 'bf' | head -n 1` .inplace/bf

run: configure build
	.inplace/bf $(flags)

test: configure
	cabal test

clean:
	cabal clean
	rm -f *.out

ghci: configure
	cabal repl exe:bf

ghcid: configure
ifeq ($(match),)
	ghcid -c "cabal repl" --allow-eval --warnings
else
	ghcid -c "cabal repl" --allow-eval --warnings --test $(match)
endif

.PHONY : configure build test clean ghci ghcid
