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

The file [`Generator.sig`](Generator.sig) defines two generators, the
`INTERNAL_GENERATOR` and the (external) `GENERATOR`. The file
[`Generator.fun`](Generator.fun) takes in an internal representation and
creates the external representation.

Currently two internal generators are defined:
- [`CMGenerator`](CMGenerator.sml) which is the fully functional transpiler
- [`SeqGenerator`](SeqGenerator.sml) which defines a generator which does not
generate CM files, rather a list of SML files to load.
