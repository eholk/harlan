(library
  (harlan middle compile-middle)
  (export compile-harlan-middle)
  (import
    (rnrs)
    (harlan compile-opts)
    (harlan verification-passes)

    (harlan middle flatten-lets)
    (harlan middle lift-complex)
    (harlan middle remove-nested-kernels)
    (harlan middle optimize-lift-lets)
    (harlan middle returnify-kernels)
    (harlan middle make-vector-refs-explicit)
    (harlan middle annotate-free-vars)
    (harlan middle lower-vectors)
    (harlan middle uglify-vectors)
    (harlan middle hoist-kernels)
    (harlan middle generate-kernel-calls)
    (harlan middle compile-module)
    (harlan middle convert-types)
    (harlan middle make-kernel-dimensions-explicit))

;; The "middle end" of a compiler. No one ever knows what's supposed
;; to go here. This goes from TFC to something we can give to print-c.
(define compile-harlan-middle
  (passes
    make-kernel-dimensions-explicit
    verify-make-kernel-dimensions-explicit
    lift-complex
    verify-lift-complex
    remove-nested-kernels
    verify-remove-nested-kernels
    optimize-lift-lets
    verify-optimize-lift-lets
    returnify-kernels
    verify-returnify-kernels
    make-vector-refs-explicit
    verify-make-vector-refs-explicit
    annotate-free-vars
    verify-annotate-free-vars
    lower-vectors
    verify-lower-vectors
    uglify-vectors
    verify-uglify-vectors
    flatten-lets
    verify-flatten-lets
    hoist-kernels
    verify-hoist-kernels
    generate-kernel-calls
    verify-generate-kernel-calls
    compile-module
    verify-compile-module
    convert-types
    verify-convert-types)))

