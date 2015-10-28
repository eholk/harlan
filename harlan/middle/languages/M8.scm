(library
    (harlan middle languages M8)
  (export M8 unparse-M8 parse-M8)
  (import (nanopass)
          (rnrs)
          (harlan middle languages M7))

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
     (+ (alloc e1 e2))
     (- (box r t)
        (unbox t r e)
        (vector t r e* ...)))

    (Body
     (body)
     (- (let-region (r ...) body)))
    
    (Rho-Type
     (t)
     (- (box r t)
        (adt x r))))

  (define-parser parse-M8 M8))

