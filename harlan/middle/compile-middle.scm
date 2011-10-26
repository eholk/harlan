(library
  (harlan middle compile-middle)
  (export compile-harlan-middle)
  (import
    (rnrs)
    (harlan compile-opts)
    (harlan verification-passes)
    (harlan middle returnify)
    (harlan middle lift-vectors)
    (harlan middle lower-vectors)
    (harlan middle remove-nested-kernels)
    (harlan middle returnify-kernels)
    (harlan middle uglify-vectors)
    (harlan middle annotate-free-vars)
    (harlan middle kernels)
    (harlan middle move-gpu-data)
    (harlan middle generate-kernel-calls)
    (harlan middle compile-module)
    (harlan middle convert-types))

;; The "middle end" of a compiler. No one ever knows what's supposed
;; to go here. This goes from TFC to something we can give to print-c.
(define compile-harlan-middle
  (passes
    returnify
    verify-returnify
    lift-vectors
    verify-lift-vectors
    lower-vectors
    verify-lower-vectors
    remove-nested-kernels
    verify-remove-nested-kernels
    returnify-kernels
    verify-returnify-kernels
    uglify-vectors
    verify-uglify-vectors
    annotate-free-vars
    hoist-kernels
    verify-hoist-kernels
    move-gpu-data
    generate-kernel-calls
    verify-generate-kernel-calls
    compile-module
    convert-types
    compile-kernels
    verify-compile-kernels)))

