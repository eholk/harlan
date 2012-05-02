(library
  (harlan middle compile-middle)
  (export compile-harlan-middle)
  (import
    (rnrs)
    (harlan compile-opts)
    (harlan verification-passes)
    (harlan middle lifting)

    (harlan middle make-kernel-dimensions-explicit)
    (harlan middle make-work-size-explicit)
    (harlan middle optimize-fuse-kernels)
    (harlan middle remove-nested-kernels)
    (harlan middle returnify-kernels)
    (harlan middle make-vector-refs-explicit)
    (harlan middle lift-complex)
    (harlan middle annotate-free-vars)
    (harlan middle lower-vectors)
    (harlan middle insert-let-regions)
    (harlan middle infer-regions)
    (harlan middle uglify-vectors)
    (harlan middle remove-let-regions)
    (harlan middle flatten-lets)
    (harlan middle hoist-kernels)
    (harlan middle generate-kernel-calls)
    (harlan middle compile-module)
    (harlan middle convert-types))

;; The "middle end" of a compiler. No one ever knows what's supposed
;; to go here. This goes from TFC to something we can give to print-c.
(define compile-harlan-middle
  (passes
   (make-kernel-dimensions-explicit
    verify-make-kernel-dimensions-explicit)
   (make-work-size-explicit
    verify-make-work-size-explicit)
   (optimize-lift-lets
    verify-optimize-lift-lets
    1)
   (optimize-fuse-kernels
    verify-optimize-fuse-kernels
    1)
   (remove-nested-kernels
    verify-remove-nested-kernels)
   (returnify-kernels
    verify-returnify-kernels)
   (lift-complex
    verify-lift-complex)
   (optimize-lift-allocation
    verify-optimize-lift-allocation
    1)
   (make-vector-refs-explicit
    verify-make-vector-refs-explicit)
   (annotate-free-vars
    verify-annotate-free-vars)
   (lower-vectors
    verify-lower-vectors)
   (insert-let-regions
    verify-insert-let-regions)
   (infer-regions
    verify-infer-regions)
   (uglify-vectors
    verify-uglify-vectors)
   (remove-let-regions
    verify-remove-let-regions)
   (flatten-lets
    verify-flatten-lets)
   (hoist-kernels
    verify-hoist-kernels)
   (generate-kernel-calls
    verify-generate-kernel-calls)
   (compile-module
    verify-compile-module)
   (convert-types
    verify-convert-types)))

)

