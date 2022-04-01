## Dependencies

In order to build the project, one needs to install `dune` and `pprint`. Using `opam`, run:

```
opam install dune pprint
```

## Usage

Building the project:

```
    make
```

Running the tests (this uses all .ml files in the `tests/` directory as test files):

```
    make test
```

Looking at the defunctionalized output for a given file `file.ml`:

```
    dune exec src/main.exe -- file.ml
```

## Structure of the sources

In the `lib/` subdirectory:

Abstract syntax trees (type definitions):
- `asttypes.mli`: auxiliary type definitions 
- `parsetree.mli`: untyped AST after parsing
- `typedtree.mli`: typed AST, after typechecking
- `tannot.mli`: typed AST, where functions are annotated with a unique ID + free variables
- `target.mli`: target AST after defunctionalization

Parsing, typechecking (already implemented):
- `lexer.mll`: lexer
- `parser.mly`: parser
- `typing.{ml,mli}`: type inference / typechecking

Pretty-printing (already implemented):
- `targetpp.{ml,mli}`: pretty-printing of a `Target` AST

**Defunctionalization (TO IMPLEMENT)**:
- `tannotgen.{ml,mli}`: transformation from a `Typedtree` AST to a `Tannot` AST
- `defunc.ml`: defunctionalization, from a `Tannot` AST to a `Target` AST
