(library
    (harlan middle languages2)
  (export M7.0.0 unparse-M7.0.0
          M7.0 unparse-M7.0 parse-M7.0
          M7.1 unparse-M7.1
          M8 unparse-M8 parse-M8)
  (import (nanopass)
          (only (rnrs) ...)
          (only (harlan middle languages1) M7))

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
  
  ;; after hoist-kernels
  (define-language M8
    (extends M7.1)

    (entry Module)
    
    (Decl
     (decl)
     (+ (gpu-module k* ...)
        (global x t e)
        cdecl)
     (- (fn name (x ...) t body)))

    (CommonDecl
     (cdecl)
     (+ (fn x (x* ...) t stmt)
        (typedef x t)
        (extern x (t* ...) -> t)))

    (Kernel
     (k)
     (+ (kernel x ((x* t*) ...) stmt)
        cdecl))

    (Stmt
     (stmt)
     (+ (apply-kernel x (e1* ...) e* ...)
        (let x t e)
        (let x t)))
    
    (Expr
     (e)
     (+ (sizeof t)
        (alloc e1 e2)
        (region-ref t e1 e2))
     (- (box r t)
        (unbox t r e)
        (vector t r e* ...)
        (do e)
        (print e)
        (print e1 e2)))

    (Body
     (body)
     (- (let-region (r ...) body)))
    
    (Rho-Type
     (t)
     (+ (vec t))
     (- (vec r t)
        (box r t)
        (adt x r))))

  (define-parser parse-M7.0 M7.0)
  (define-parser parse-M8 M8))

