(library
  (harlan compiler)
  (export compile-harlan harlan->c++)
  (import
   (rnrs)
   (harlan compile-opts)
   (harlan front compile-front)
   (harlan middle compile-middle)
   (harlan backend print-c))

(define compile-harlan
  (passes
   (compile-harlan-frontend)
   (compile-harlan-middle)))

(define harlan->c++
  (passes
   (compile-harlan)
   (harlan-format-c)))

)
