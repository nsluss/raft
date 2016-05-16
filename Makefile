.DEFAULT_GOAL := install

test:
	stack test

build:
	stack build

run: build
	stack exec sudo mond

install:
	stack install

watch-tests:
	stack test --file-watch

