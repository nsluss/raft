test:
	stack test

build:
	stack build

run: build
	stack exec sudo mond

