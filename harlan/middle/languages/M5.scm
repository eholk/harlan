(library
    (harlan middle languages M5)
  (export M5 unparse-M5 parse-M5)
  (import (rnrs)
          (nanopass)
          (harlan middle languages M0-3)) ;; where is M4?

    ;; After desugar-match
  (define-language M5
    (extends M3)

    (entry Module)
    
    (Decl
     (decl)
     (- (define-datatype (x r) pt ...)
        (define-datatype x pt ...))
     (+ (typedef x t)))
    
    (Rho-Type
     (t)
     (+ (struct (x t) ...)
        (union  (x t) ...)
        (box r t)))

    (Expr
     (e)
     (- (match t e arm ...))
     (+ (c-expr t x)
        (empty-struct)
        (unbox t r e)
        (box   r t e)
        (field e x)))

    (MatchArm
     (arm)
     (- (mbind e)))

    (MatchBindings
     (mbind)
     (- (x x* ...)))

    (AdtDeclPattern
     (pt)
     (- (x t* ...))))

  (define-parser parse-M5 M5))
