(library
    (harlan middle languages M7)
  (export M7 unparse-M7 parse-M7
          M7.0 unparse-M7.0 parse-M7.0
          M7.0.0 unparse-M7.0.0 parse-M7.0.0
          M7.0.1 unparse-M7.0.1
          M7.0.2 unparse-M7.0.2 parse-M7.0.2
          M7.0.3 unparse-M7.0.3
          M7.1 unparse-M7.1 parse-M7.1)
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

  ;; Before returnify-kernels
  (define-language M7.0.1
    (extends M7.0.0)

    (Body (body)
          (+ (transaction (r ...) body)
             (retry-transaction))))

  ;; Before remove-transactions / After returnify-kernels
  (define-language M7.0.2
    (extends M7.0.1)

    (LetBinding
     (lbind)
     (+ (x t)))
    
    (Danger
     (danger)
     (+ (danger: x t)))

    (Expr (e)
          ;; Kernels were turned into statements by returnify-kernels
          (- (kernel t r (e* ...) (((x0 t0) (e1 t1) i*) ...) e)
             (error x)))
    
    (Body (body)
          (+ stmt)
          (- (if e body1 body2)
             (return)
             (return e)
             (assert e)
             (set! e1 e2)
             (print e)
             (print e1 e2)
             (while e1 e2)
             (let (lbind* ...) body)
             (transaction (r ...) body)
             (retry-transaction)))

    (Stmt (stmt)
          (+ (for (x e1 e2 e3) stmt)
             (do e)
             (assert e)
             (error x)
             (set! e1 e2)
             (print e)
             (print e1 e2)
             (if e stmt1 stmt2)
             (if e stmt)
             (begin stmt ...)
             (let (lbind* ...) stmt)
             (transaction (r ...) stmt)
             (retry-transaction)
             (kernel t (e* ...) danger (((x0 t0) (e1 t1) i*) ...) stmt)
             (while e stmt)
             (return)
             (return e))))
  
  ;; After remove-transactions
  (define-language M7.0.3
    (extends M7.0.2)

    (Expr (e)
          (+ (addressof e)))
    
    (Stmt (stmt)
          (+ (goto name)
             (label name))
          (- (transaction (r ...) stmt)
             (retry-transaction))))
  
  ;; before lower-vectors
  (define-language M7.0
    (extends M7.0.3)

    (FreeVars
     (fv)
     (+ (free-vars (x t) ...)))
    
    (Stmt
     (stmt)
     (- (kernel t (e* ...) danger (((x0 t0) (e1 t1) i*) ...) stmt))
     (+ (kernel t (e* ...) danger fv stmt)
        ))

    (Expr
     (e)
     (+ (deref e))))
  
  ;; after remove-let-regions
  (define-language M7.1
    (extends M7.0)

    (Rho-Type
     (t)
     (- (vec r t))
     (+ (vec t)))

    (Stmt
     (stmt)
     (- (kernel t (e* ...) danger fv stmt))
     (+ (kernel (e* ...) fv stmt)))
    
    (Expr
     (e)
     (- (box r t e)
        (set! e1 e2)
        (while e1 e2)
        (if e0 e1)
        (for (x e0 e1 e2) e)
        (let-region (r ...) e)
        (do e)
        (print e1 e2)
        (print e))
     (+ (box r t)
        (sizeof t)
        (region-ref t e1 e2))))

  (define-parser parse-M7 M7)
  (define-parser parse-M7.0 M7.0)
  (define-parser parse-M7.0.0 M7.0.0)
  (define-parser parse-M7.0.2 M7.0.2)
  (define-parser parse-M7.1 M7.1))
