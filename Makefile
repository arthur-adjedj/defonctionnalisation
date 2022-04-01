all:
	dune build

test:
	dune exec src/runtests.exe -- tests/

clean:
	dune clean

.PHONY: all test clean
