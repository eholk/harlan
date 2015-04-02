(library
    (harlan middle languages M7)
  (export M7 unparse-M7 parse-M7
          M7.0 unparse-M7.0 parse-M7.0
          M7.0.0 unparse-M7.0.0
          M7.1 unparse-M7.1)
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

    
    ;; After remove-danger
  (define-language M7.0.0
    (extends M7)

    (Expr (e)
          (+ (error x))
          (- (unsafe-vector-ref t e0 e1))))
    
  ;; before lower-vectors
  (define-language M7.0
    (extends M7.0.0)

    (FreeVars
     (fv)
     (+ (free-vars (x t) ...)))
    
    (LetBinding
     (lbind)
     (+ (x t)))
    
    (Stmt
     (stmt)
     (+ (for (x e1 e2 e3) stmt)
        (print e)
        (print e1 e2)
        (assert e)
        (set! e1 e2)
        (error x)
        (begin stmt ...)
        (if e stmt1 stmt2)
        (if e stmt)
        (while e stmt)
        (do e)
        (kernel t (e* ...) fv stmt)
        (let (lbind* ...) stmt)
        (return e)
        (return)))

    (Body
     (body)
     (+ stmt)
     (- (begin body* ... body)
        (let (lbind* ...) body)
        (return)
        (return e)
        (set! e1 e2)
        (assert e)
        (while e1 e2)
        (do e)
        (print e)
        (print e1 e2)
        (if e body1 body2)))

    (Expr
     (e)
     (- (kernel t r (e* ...) (((x0 t0) (e1 t1) i*) ...) e)
        (error x))
     (+ (addressof e)
        (deref e))))
  
  ;; before uglify-vectors
  (define-language M7.1
    (extends M7.0)

    (Expr
     (e)
     (- (box r t e))
     (+ (box r t)))
    )

  ;; after uglify-vectors
  ;;(define-language M7.2
  ;;  (extends M7.1)
  ;;
  ;;  )

  (define-parser parse-M7 M7)
   (define-parser parse-M7.0 M7.0))
