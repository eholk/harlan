;; Definition of all the Nanopass Languages.

(library
    (harlan middle languages)
  (export M0 unparse-M0 parse-M0
          M1 unparse-M1
          M2 unparse-M2
          M3 unparse-M3 parse-M3
          M5 unparse-M5 parse-M5
          M6 unparse-M6
          M7 unparse-M7
          M7.0 unparse-M7.0 parse-M7.0
          M7.1 unparse-M7.1
          ;;M7.2 unparse-M7.2
          M8 unparse-M8 parse-M8
          M9 unparse-M9
          M9.1 unparse-M9.1
          M9.2 unparse-M9.2
          M9.2.1 unparse-M9.2.1
          M9.2.2 unparse-M9.2.2
          M9.3 unparse-M9.3
          M10 unparse-M10)
  (import
   (rnrs)
   (nanopass)
   (only (elegant-weapons helpers) binop? scalar-type? relop?))

  (define variable? symbol?)
  (define region-var? symbol?)
  (define (operator? x) (or (binop? x) (relop? x) (eq? x '=)))
  (define (arrow? x) (eq? x '->))
  (define (any? x) (lambda _ #t))
  (define (address-space? x) (memq x '(local global private)))
  
  ;; The first language in the middle end.
  (define-language M0
    (terminals
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
    (LetBinding
     (lbind)
     (x t e))
    (Body
     (body)
     (begin body* ... body)
     (let-region (r ...) body)
     (let (lbind* ...) body)
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
     (let (lbind* ...) e)
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
     (unsafe-vec-ptr t e)
     (length e)
     (int->float e)
     (float->int e)
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
     (- (x0 x1 t ctag ...)))
    (ClosureTag
     (ctag)
     (- (x (x0 ...) e (x* t*) ...)))
    (Rho-Type
     (t)
     (- (closure r (t* ...) -> t))))

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
     (+ (kernel t r (e* ...) (((x0 t0) (e1 t1) i*) ...) e))))

  ;; before lower-vectors
  (define-language M7.0
    (extends M7)

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
        (begin stmt ...)
        (if e stmt1 stmt2)
        (if e stmt)
        (error x)
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
     (- (kernel t r (e* ...) (((x0 t0) (e1 t1) i*) ...) e))
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
     (+ (cast t e)
        (sizeof t)
        (alloc e1 e2)
        (region-ref t e1 e2))
     (- (box r t)
        (unbox t r e)
        (vector t r e* ...)
        (do e)
        (print e)
        (print e1 e2)))

    (Rho-Type
     (t)
     (+ (vec t))
     (- (vec r t)
        (box r t)
        (adt x r))))
  
  (define-language M9
    (extends M8)
    (entry Module)

    (Stmt
     (stmt)
     (- (apply-kernel x (e1* ...) e* ...))))
  
  (define-language M9.1
    (extends M9)
    (entry Module)

    (terminals
     (+ (any (?))))
    
    (Decl
     (decl)
     (- (gpu-module k* ...))
     (+ (gpu-module cg k* ...)))
    
    (CallGraph
     (cg)
     (+ (call-graph ?0 ?1))))

  (define-language M9.2
    (extends M9.1)
    (entry Module)

    (terminals
     (- (any (?))))
    
    (Decl
     (decl)
     (+ (gpu-module k* ...))
     (- (gpu-module cg k* ...)))
    
    (CommonDecl
     (cdecl)
     (- (fn x (x* ...) t stmt))
     (+ (fn x (x* ...) t body)))

    (LabeledBlock
     (lbl)
     (+ (name ((x t) ...) stmt)))

    (CallGraph
     (cg)
     (- (call-graph ?0 ?1)))
    
    (Expr
     (e)
     (+ (call-label name t e* ...)))
    
    (Body
     (body)
     (+ (with-labels (lbl ...) stmt))
     (- (let-region (r ...) body))))

  (define-language M9.2.1
    (extends M9.2)
    (entry Module)
    
    (Expr
     (e)
     (- (call-label name t e* ...))
     (+ (call-label name t ((x* t*) ...) e* ...))))

  (define-language M9.2.2
    (extends M9.2.1)
    (entry Module)

    (Rho-Type
     (t)
     (+ (fixed-array t i)))
    
    (Stmt
     (stmt)
     (+ (push! e0 e1 t e2) ;; push e2 with type t onto e0 + e1
        (pop! e0 e1 t e2)
        (goto name)
        (label name)))

    (Body
     (body)
     ;; like begin, bit doesn't conflict with the existing
     ;; (begin stmt* ... stmt)
     (+ (seq stmt* ... body)))
    
    (Expr
     (e)
     (+ (call-label name e* ...))
     (- (call-label name t ((x* t*) ...) e* ...))))
  
  (define-language M9.3
    (extends M9.2.2)
    (entry Module)

    (terminals
     (+ (address-space (space))))
    
    (Body
     (body)
     (- (with-labels (lbl ...) stmt)
        (seq stmt* ... body)))

    (Stmt
     (stmt)
     (- (push! e0 e1 t e2)
        (pop! e0 e1 t e2)))
    
    (Expr
     (e)
     (- (call-label name e* ...)))

    (Rho-Type
     (t)
     (+ (ptr space t)))

    (LabeledBlock
     (lbl)
     (- (name ((x t) ...) stmt))))
  
  (define-language M10
    (extends M9))
  
  (define-parser parse-M0 M0)
  (define-parser parse-M3 M3)
  (define-parser parse-M5 M5)
  ;;(define-parser parse-M7 M7)
  (define-parser parse-M7.0 M7.0)
  ;;(define-parser parse-M7.1 M7.1)
  (define-parser parse-M8 M8)

  )
