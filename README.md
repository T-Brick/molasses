# molasses
`molasses` aims to allow the SML/NJ Compilation and Library Manager (CM) to support ML Basis (MLB) files. 

Currently, not much is implemented and much will likely change.

## Usage

You can build `molasses` using `make`, then passing in a MLB file to the executable

```sh
make
./molasses test/either/either.mlb
```
This will generate some files in the `test/either/.molasses` directory. You can then load the files into the SML/NJ REPL using the associated `molasses-sources1.cm` file.

## Naming

The name of this project follows a similar convention as seen in [millet](https://github.com/azdavis/millet) and [mulligan](https://github.com/brandonspark/mulligan).
Namely, molasses has the letters M and L in it and in the same order as Standard ML. Also, molasses is sticky and this "sticks" two compliation systems together.
