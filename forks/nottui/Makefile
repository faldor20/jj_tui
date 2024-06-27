all:
	dune build

clean:
	dune clean

TESTS=minimal misc reranger stress pretty \
RUN_TESTS_BC=$(patsubst %, run-%, $(TESTS))
RUN_TESTS_EXE=$(patsubst %, run-%.exe, $(TESTS))

$(TESTS):
	dune build examples/$@.bc

examples:
	dune build $(patsubst	%,examples/%.bc,$(TESTS))

$(RUN_TESTS_BC):
	dune exec examples/$(patsubst run-%,%,$@.bc)

$(RUN_TESTS_EXE):
	dune exec examples/$(patsubst run-%,%,$@)


.PHONY: all clean examples $(RUN_TESTS_BC) $(RUN_TESTS_EXE)
