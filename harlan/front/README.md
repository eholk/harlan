This directory includes the Harlan compiler's front end. This includes
tasks such as parsing, library loading, hygienic macro expansion and
type/region inference.

The file `compile-front.scm` declares the passes and the order in
which they run. Each pass is defined in a `.scm` file by the same name
as the pass. The passes are:

1. `expand-include` - Loads libraries used by the program and includes
them in the AST.
2. `expand-macros` - Performs hygienic macro expansion.
3. `parse-harlan` - Verifies that the AST at this point is a
well-formed Harlan program. This pass also adds tags to mark symbols
as literals, variable references, function calls, etc., so that
parsing in later passes is simpler.
4. `returnify` - Replaces the last value of a function with a return statement.
5. `typecheck` - Infers the type of the Harlan program and also
assigns objects to regions.
6. `expand-primitives` - Desugars some Harlan syntax into simpler
forms. Much of the work of this pass has been moved into macros
included in the standard library.
