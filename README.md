# Causes benchmark for paper Causality & Control flow

To compile, install the haskell platform and run:

cabal sandbox init -v
cabal install -j

This should create a user-local copy of the benchmark and put it in:

./.cabal-sandbox/bin/cause-benchmark

You can run -h to get an overview of all the options. Running with -t
reproduces the table in the paper.
