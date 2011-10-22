(library
  (harlan compiler)
  (export compile-harlan)
  (import
    (rnrs)
    (harlan compile-opts)
    (harlan front compile-front)
    (harlan middle compile-middle))

(define compile-harlan
  (passes
    compile-harlan-frontend
    compile-harlan-middle)))

