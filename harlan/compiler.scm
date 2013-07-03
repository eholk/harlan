(library
  (harlan compiler)
  (export compile-harlan harlan->c++)
  (import
   (rnrs)
   (harlan compile-opts)
   (harlan front compile-front)
   (harlan middle compile-middle)
   (elegant-weapons insert-prototypes)
   (harlan backend print-c))

(define compile-harlan
  (passes
   (compile-harlan-frontend)
   (compile-harlan-middle)
   (insert-prototypes)))

(define harlan->c++
  (passes
   (compile-harlan)
   (harlan-format-c)))

)
