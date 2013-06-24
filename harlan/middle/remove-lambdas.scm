(library
    (harlan middle remove-lambdas)
  (export remove-lambdas
          M0 parse-M0 unparse-M0
          M1 parse-M1 unparse-M1)
  (import
   (rnrs)
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
     (closure r (t* ...) -> t)
     x)
    (Decl
     (decl)
     (extern name (t* ...) -> t)
     (fn name (x ...) t body))
    (Body
     (body)
     (begin e ... body)
     (let-region (r ...) body)
     (return)
     (return e))
    (Expr
     (e)
     (int i)
     (str str-t)
     (print e)
     (print e1 e2)
     (begin e e* ...)
     (assert e)
     (let ((x* t* e*) ...) e)
     (let-region (r ...) e)
     (lambda ((x t) ...) e)
     (invoke e e* ...)
     (call e e* ...)
     (op e1 e2)
     (var t x)))

  (define-language M1
    (extends M0)
    (entry Module)
    (Expr
     (e)
     (- (lambda ((x t) ...) e)
        (invoke e e* ...))))
  
  (define-parser parse-M0 M0)
  (define-parser parse-M1 M1)
  
  (define-pass np-remove-lambdas : M0 (m) -> M1 ()
    (Expr : Expr (expr) -> Expr ()
          ((lambda ((,x* ,t*) ...) ,e)
           `(int 5))
          ((invoke ,[e] ,[e*] ...)
           `(call ,e ,e* ...))))

  ;; I'm a horrible person for defining this.
  (define-syntax ->
    (syntax-rules ()
      ((_ e) e)
      ((_ e (x a ...) e* ...)
       (-> (x e a ...) e* ...))
      ((_ e x e* ...)
       (-> (x e) e* ...))))
  
  (define (remove-lambdas module)
    (-> module
        parse-M0
        np-remove-lambdas
        unparse-M1))
  )

