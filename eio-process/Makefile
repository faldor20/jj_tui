.PHONY: all
all: build

.PHONY: build
build:
	opam exec -- dune build

.PHONY: test
test:
	opam exec -- dune runtest

.PHONY: fmt
fmt:
	opam exec -- dune build @fmt --auto-promote

.PHONY: lint
lint:
	opam lint
	opam exec -- opam-dune-lint

.PHONY: deps
deps:
	opam install . --deps-only --with-doc --with-test

.PHONY: doc
doc:
	opam exec -- dune build @doc

.PHONY: clean
clean:
	opam exec -- dune clean

.PHONY: check-all
check-all: deps all test doc clean lint fmt
