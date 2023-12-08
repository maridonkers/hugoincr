all: help

# Run build from within nix-shell (first use `make develop`).
build:  package.yaml
	stack build --fast

hugoincr.cabal: package.yaml
	hpack

develop:
	nix-shell

hpack:
	hpack

repl:
	stack repl

edit:
	emacs &

lint:
	hlint `ag -l --haskell`

format:
	ormolu -i `ag -l --haskell`

docs:
	stack haddock

clean:
	stack clean

run:
	stack run hugoincr

# see local-bin-path in stack.yaml for destination path
install:
	stack install

# binutils-gold curl gcc g++ git gmp-dev ncurses-dev ncurses-static
# libffi-dev make xz tar perl zlib-dev zlib-static
release:
	hpack
	cabal build --enable-executable-static exe:hugoincr

help:
	@grep '^[^      #:]\+:' Makefile | sed -e 's/:[^:]*//g'
	@echo "Use make -s for silent execution (e.g. make -s ls)"
