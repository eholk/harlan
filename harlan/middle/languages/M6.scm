(library
    (harlan middle languages M6)
  (export M6 unparse-M6)
  (import (rnrs)
          (nanopass)
          (harlan middle languages M5))

  ;; After make-kernel-dimensions-explicit
  (define-language M6
    (extends M5)

    (entry Module)
    
    (Expr
     (e)
     (- (kernel t r (((x0 t0) (e1 t1)) ...) e)
        (iota-r r e))
     (+ (kernel t r i (((x0 t0) (e1 t1) i*) ...) e)
        (kernel t r i (e* ...) (((x0 t0) (e1 t1) i*) ...) e))))

  (define-language M7
    (extends M6)
    (entry Module)
    
    (Expr
     (e)
     (- (kernel t r i (((x0 t0) (e1 t1) i*) ...) e)
        (kernel t r i (e* ...) (((x0 t0) (e1 t1) i*) ...) e))
     (+ (kernel t r (e* ...) (((x0 t0) (e1 t1) i*) ...) e)
        (make-vector t r e)))))
