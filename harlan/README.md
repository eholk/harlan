This directory contains the bulk of the source code for the Harlan
compiler. It is divided into three sections: the front end, the middle
end, and the backend.

The front end is mainly responsible for things such as parsing, module
loading, macro expansion and type checking.

The bulk of the compiler takes place in the middle end. This consists
of a number of passes to lower Harlan's high level language into a
progressively more and more C-like language. The middle end is also
responsible for Harlan's optimizations.

The backend relies on [Elegant Weapons] for pretty printing the output
of the middle end into C++ code that is compiled by the system C++
compiler.

[Elegant Weapons]: https://github.com/eholk/elegant-weapons

## Directory Structure

* `backend` - the back end of the compiler. This mostly consists of
  code to transform Harlan's S-Expression-based intermediate forms
  into C code.
* `front` - the front end of the compiler. This includes tasks such as
  loading Harlan libraries, expanding macros and inferring types.
* `middle` - the middle end of the compiler. This consists of a number
  of passes to lower Harlan's high level forms into something that
  resembles C. The middle end includes several optimization passes
  too.

