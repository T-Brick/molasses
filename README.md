# molasses
`molasses` aims to allow the SML/NJ Compilation and Library Manager (CM) to
support ML Basis (MLB) files.

## Usage

First make sure you clone this repo including submodules. The command should something like this:

```
git clone --recurse-submodules -j8 https://github.com/T-Brick/molasses
```

If you have MLton installed, you can build `molasses` using `make molasses`,
then passing in a MLB file to the executable:

```sh
make
./molasses test/either/either.mlb
```
This will generate some files in the `test/either/.molasses` directory. Using CM
to load the `molasses-sources1.cm` file will load all the files into SML/NJ. To
use the REPL, you can also pass in the `--repl` flag, which will generate the
appropriate files and launch the SML/NJ REPL (using the command `rlwrap sml`),
with the appropriate toplevel declarations already loaded.

If you only have SML/NJ installed, you can load `molasses` by running:
```sh
make repl
```

This will load an SML/NJ REPL with all of Molasses already loaded. Running this
will also generate a heap image `molasses-repl.arch-opsys`. You can then use
this heap image to load the SML/NJ REPL with Molasses anywhere by running:

```sh
sml @SMLload=molasses-repl
```

This should open the SML/NJ REPL with molasses loaded in. You can then use the
`Molasses` structure to generate molasses files. The `MolassesNJ` structure may
be used for generating heap images. Also, the implementation defined
`use : string -> unit` has been defined to use Molasses.

## Mapping Specific MLB Files

It maybe the case that there are specific libraries that you already have CM's
for, and thus making converting them unnecessary. Molasses provides a way to
map specific files in MLB to a pre-defined CM counterpart.

The file [example-libmap](example-libmap) demonstrates how to construct a
mapping (in this case, the example is the SML basis, which is already loaded by
default). The first line in the file consists of a list of space separated
MLB Path Variables, which if they appear will trigger a lookup. The remaining
lines are the file path as it appears in the MLB file, followed by one or more
spaces, then the corresponding CM equivalent of the file.

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
