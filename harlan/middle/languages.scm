;; Definition of all the Nanopass Languages.

(library
    (harlan middle languages)
  (export M0 unparse-M0 parse-M0
          M1 unparse-M1
          M2 unparse-M2
          M3 unparse-M3 parse-M3
          M4 unparse-M4
          M5 unparse-M5 parse-M5
          M6 unparse-M6
          M7 unparse-M7
          M8 unparse-M8 parse-M8)
  (import
   (rnrs)
   (nanopass)
   (only (elegant-weapons helpers) binop? scalar-type? relop?))

  (define variable? symbol?)
  (define region-var? symbol?)
  (define (operator? x) (or (binop? x) (relop? x) (eq? x '=)))
  (define (arrow? x) (eq? x '->))
  (define (any? x) (lambda _ #t))
  
  ;; The first language in the middle end.
  (define-language M0
    (terminals
     (any (any ?))
     (scalar-type (bt))
     (operator (op))
     (region-var (r))
     (arrow (->))
     (integer (i))
     (flonum (f))
     (boolean (b))
     (char (c))
     (string (str-t))
     (variable (x name)))
    (Module
     (m)
     (module decl ...))
    (Rho-Type
     (t)
     bt
     (ptr t)
     (fn (t* ...) -> t)
     (adt x r)
     (adt x)
     (vec r t)
     (closure r (t* ...) -> t)
     x)
    (Decl
     (decl)
     (extern name (t* ...) -> t)
     (define-datatype (x r) pt ...)
     (define-datatype x pt ...)
     (fn name (x ...) t body))
    (AdtDeclPattern
     (pt)
     (x t* ...))
    (Body
     (body)
     (begin body* ... body)
     (let-region (r ...) body)
     (let ((x* t* e*) ...) body)
     (if e body1 body2)
     (print e)
     (print e1 e2)
     (do e)
     (while e1 e2)
     (assert e)
     (set! e1 e2)
     (return)
     (return e))
    (MatchBindings
     (mbind)
     (x x* ...))
    (MatchArm
     (arm)
     (mbind e))
    (Expr
     (e)
     (int i)
     (float f)
     (bool b)
     (char c)
     (str str-t)
     (vector t r e* ...)
     (do e)
     (print e)
     (print e1 e2)
     (begin e e* ...)
     (assert e)
     (let ((x* t* e*) ...) e)
     (let-region (r ...) e)
     (kernel t r (((x0 t0) (e1 t1)) ...) e)
     (iota-r r e)
     (make-vector t r e)
     (for (x e0 e1 e2) e)
     (lambda t0 ((x t) ...) e)
     (invoke e e* ...)
     (call e e* ...)
     (match t e arm ...)
     (vector-ref t e0 e1)
     (unsafe-vector-ref t e0 e1)
     (length e)
     (int->float e)
     (if e0 e1)
     (if e0 e1 e2)
     (set! e1 e2)
     (while e1 e2)
     (op e1 e2)
     (var t x)))

  (define-language M1
    (extends M0)
    (entry Closures)
    (Closures
     (cl)
     (+ (closures (ctag ...) m)))
    (ClosureTag
     (ctag)
     (+ (x t (x0 ...) e (x* t*) ...)))
    (Expr
     (e)
     (- (lambda t0 ((x t) ...) e))
     ;; make a closure of type t, with variant x and bind the free
     ;; variables to e ...
     (+ (make-closure t x e ...))))

  (define-language M2
    (extends M1)
    (entry Closures)

    (Closures
     (cl)
     (- (closures (ctag ...) m))
     (+ (closures (cgroup ...) m)))
    (ClosureGroup
     (cgroup)
     (+ (x0 x1 t ctag ...)))
    (ClosureTag
     (ctag)
     (- (x t (x0 ...) e (x* t*) ...))
     (+ (x (x0 ...) e (x* t*) ...))))
  
  (define-language M3
    (extends M2)
    (entry Module)
    (Expr
     (e)
     (- (make-closure t x e ...)
        (invoke e e* ...)))
    (Closures
     (cl)
     (- (closures (cgroup ...) m)))
    (ClosureGroup
     (cgroup)
     (- (x0 x1 t ctag ...))
     (+ (x0 x1 any ctag ...)))
    (Rho-Type
     (t)
     (- (closure r (t* ...) -> t))))  
  
  (define-language M4
    (extends M3)
    (entry CallGraph)
    (CallGraph
     (cg)
     (+ (call-graph ? m))))

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
        (union  (x t) ...)))

    (Expr
     (e)
     (- (match t e arm ...))
     (+ (c-expr t x)
        (empty-struct)
        (unbox t r e)
        (box   r t e)
        (field e x))))
  
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
     (+ (kernel t r (e* ...) (((x0 t0) (e1 t1) i*) ...) e))))

  ;; after hoist-kernels
  (define-language M8
    (extends M7)

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
     (+ (print e)
        (print e1 e2)
        (assert e)
        (set! e1 e2)
        (apply-kernel x (e1* ...) e* ...)
        (let x t e)
        (let x t)
        (begin stmt ...)
        (if e stmt)
        (if e stmt1 stmt2)
        (for (x e1 e2 e3) stmt)
        (while e stmt)
        (do e)
        (error x)
        rstmt))

    (RetStmt
     (rstmt)
     (+ (return e)
        (return)))

    (Expr
     (e)
     (+ (cast t e)
        (sizeof t)
        (alloc e1 e2)
        (addressof e)
        (deref e)
        (region-ref t e1 e2)))

    (Rho-Type
     (t)
     (+ (vec t))
     (- (vec r t)
        (adt x r)))
    )
  
  (define-parser parse-M0 M0)
  (define-parser parse-M3 M3)
  (define-parser parse-M5 M5)
  (define-parser parse-M8 M8)

  )
