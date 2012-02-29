.PHONY: build

build:
	cabal-dev build

install: build
	cabal-dev install --disable-documentation
