(library
    (harlan middle languages M7)
  (export M7 unparse-M7 parse-M7)
  (import (rnrs)
          (nanopass)
          (harlan middle languages M6))

  (define-language M7
    (extends M6)
    (entry Module)
    
    (Expr
     (e)
     (- (kernel t r i (((x0 t0) (e1 t1) i*) ...) e)
        (kernel t r i (e* ...) (((x0 t0) (e1 t1) i*) ...) e))
     (+ (kernel t r (e* ...) (((x0 t0) (e1 t1) i*) ...) e)
        (make-vector t r e))))
  
   (define-parser parse-M7 M7))
