(library
  (harlan compiler)
  (export compile-harlan harlan->c++)
  (import
   (chezscheme)
   (only (util helpers) join)
   (harlan compile-opts)
   (harlan front compile-front)
   (harlan middle compile-middle)
   (harlan back print-c))

(define compile-harlan
  (passes
    compile-harlan-frontend
    compile-harlan-middle))

(define harlan->c++
  (passes compile-harlan format-c))

)
