# Generators
## Description/Motivation

We break-up compilation into numerous different "generators" which create the
desired output. We do this because there are various different potential modes
we'd like to output.

For instance, if we are working with a program that only has expression level
declarations, then it doesn't make sense to do a bunch of extra work to generate
the appropriate files, when we can just load in the files in sequential order.

It also means that if we have a program with no top-level expression then we can
also simplify the generated output.

While this may not be practical for many real SML programs (particular the first
case), it does mean that students who attempting to learn SML can work with a
language subset that entirely excludes structure level declarations, then
the generated molasses output will also exclude structure declarations, which
hopefully will limit confusion for students.

## Implementation

The signature [`GENERATOR`](Generator.sig) defines an external generator and the
signature [`INTERNAL_GENERATOR`](internal/InternalGenerator.sig) defines an
internal generator. The file [`Generator.fun`](Generator.fun) takes in an
internal representation and creates the external representation.

This is all managed by the [`GenDriver`](GenDriver.sml) which selects whichever
generator to run when a file is needed to be converted.

Currently two internal generators are defined:
- [`CMGenerator`](internal/CMGenerator.sml) which is the fully functional
transpiler and generates the appropriate SML and CM files to run.
- [`SeqGenerator`](internal/SeqGenerator.sml) which defines a generator which
does not generate CM files, rather a list of SML files to load.
