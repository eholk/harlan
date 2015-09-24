This directory includes nanopass language definitions for all of the
languages used by the Harlan compiler. They are broken out into
separate files to try and keep compile times more reasonable. It seems
that having too many in one file hurts performance, probably due to
the compile-time environment getting too big. Anyway, It's easy to get
lost in all of these, so I'm including some notes here about how the
languages evolve.

## Naming Scheme

The languages are all named M, for middle, followed by a
number. Decimals are added when we need to add new intermediate
languages, to avoid having to renumber everything. Thus, if we had M0
and M1 but we needed a language between them, we would call the new
language M0.0. The next language between M0.0 and M1 would be M0.1.

## Statement/Expression/Body distinction

This is one of the messier parts, which in part reflects the evolution
of the language. Originally Harlan was a C-like language, with a clear
distinction between statements and expressions. As the language has
become more Scheme-like, we've tried to lessen the
distinction. Towards the beginning we basically only have Expressions,
but since we are compiling to C, these get split into statements and
expressions as the process advances.

The `Stmt` nonterminal is first introduced in M7.0. This replaces what
was known as `Body` since M0.
