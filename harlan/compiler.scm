(library
  (harlan compiler)
  (export compile-harlan harlan->c++)
  (import
   (chezscheme)
   (only (elegant-weapons helpers) join)
   (only (harlan verification-passes)
     verify-print-c)
   (harlan compile-opts)
   (harlan front compile-front)
   (harlan middle compile-middle)
   (elegant-weapons print-c))

(define compile-harlan
  (passes
    compile-harlan-frontend
    compile-harlan-middle))

(define harlan->c++
  (passes compile-harlan format-c))

)
