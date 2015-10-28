(library
    (harlan middle languages M0-3)
  (export M0 unparse-M0 parse-M0
          M1 unparse-M1
          M2 unparse-M2
          M3 unparse-M3 parse-M3)
  (import (rnrs)
          (nanopass)
          (only (elegant-weapons helpers) binop? scalar-type? relop?))

  (define variable? symbol?)
  (define region-var? symbol?)
  (define (operator? x) (or (binop? x) (relop? x) (eq? x '=)))
  (define (arrow? x) (eq? x '->))
  
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
     (if e body1)
     (print e)
     (print e1 e2)
     (do e)
     (while e1 body)
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
     (u64 i)
     (float f)
     (bool b)
     (char c)
     (str str-t)
     (vector t r e* ...)
     (cast t e)
     (do e)
     (print e)
     (print e1 e2)
     (begin e e* ...)
     (assert e)
     (let (lbind* ...) e)
     (let-region (r ...) e)
     (kernel t r (((x0 t0) (e1 t1)) ...) e)
     (iota-r r e)
     (for (x e0 e1 e2) e)
     (lambda t0 ((x t) ...) e)
     (invoke e e* ...)
     (call e e* ...)
     (match t e arm ...)
     (vector-ref t e0 e1)
     (unsafe-vector-ref t e0 e1)
     (unsafe-vec-ptr t e)
     (length e)
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

  (define-parser parse-M0 M0)
  (define-parser parse-M3 M3))
