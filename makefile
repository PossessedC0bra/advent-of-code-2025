.PHONY: build build-watch run test fmt clean deps install uninstall

build:
	@dune build

build-watch:
	@dune build --watch

run: build
	@dune exec aoc2025

# execute tests using --force to always run them
test:
	@dune runtest --force 

fmt:
	@dune fmt

clean:
	@dune clean

deps:
	@opam install . --deps-only --with-test -y

install:
	@dune install

uninstall:
	@dune uninstall
