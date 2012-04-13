(library
  (harlan compiler)
  (export compile-harlan harlan->c++)
  (import
   (rnrs)
   (only (elegant-weapons helpers) join)
   (only (harlan verification-passes)
     verify-print-c)
   (harlan compile-opts)
   (harlan front compile-front)
   (harlan middle compile-middle)
   (harlan backend print-c))

(define compile-harlan
  (passes
    compile-harlan-frontend
    compile-harlan-middle))

(define harlan->c++
  (passes compile-harlan harlan-format-c))

)
