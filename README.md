# molasses
`molasses` aims to allow the SML/NJ Compilation and Library Manager (CM) to
support ML Basis (MLB) files.

## Usage

You can build `molasses` using `make`, then passing in a MLB file to the
executable:

```sh
make
./molasses test/either/either.mlb
```
This will generate some files in the `test/either/.molasses` directory. Using CM
to load the `molasses-sources1.cm` file will load all the files into SML/NJ. To
use the REPL, you can also pass in the `--repl` flag, which will generate the
appropriate files and launch the SML/NJ REPL (using the command `rlwrap sml`),
with the appropriate toplevel declarations already loaded.

## Limitations

Currently there are a number of limitations, that will hopefully be reduced in
the future:

- MLB: files with annotation declarations, open declarations, and basis
declarations/expressions are not supported.
- SML: Top-level `open` declarations are not supported.
- SML: Top-level `abstype` declarations are not supported.
- Most [NJ language extensions](https://www.smlnj.org/doc/features.html) are
not supported.
- The [`MLton` structure](http://mlton.org/MLtonStructure) is not supported.


## Naming

The name of this project follows a similar convention as seen in
[millet](https://github.com/azdavis/millet) and
[mulligan](https://github.com/brandonspark/mulligan). Namely, molasses has the
letters M and L in it and in the same order as Standard ML. Also, molasses is
sticky and this "sticks" two compliation systems together.
