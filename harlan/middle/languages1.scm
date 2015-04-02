;; In an effort to reign in Harlan's memory usage, I've split
;; languages.scm into a couple files.

(library
    (harlan middle languages1)
  (export M0 unparse-M0 parse-M0
          M1 unparse-M1
          M2 unparse-M2
          M3 unparse-M3 parse-M3
          M5 unparse-M5 parse-M5
          M6 unparse-M6
          M7 unparse-M7 parse-M7)
  (import
   (rnrs)
   (nanopass)
   (only (elegant-weapons helpers) binop? scalar-type? relop?)
   (harlan middle languages M0-3))

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
        (make-vector t r e))))
  
  (define-parser parse-M5 M5)
  (define-parser parse-M7 M7))
