This directory contains the middle portion of the compiler, which
successively lowers a very high level representation of Harlan
programs to something that closely resembles C.

The passes are all listed in `compile-middle.scm` in the order that
they run.
