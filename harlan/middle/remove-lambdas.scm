(library
    (harlan middle remove-lambdas)
  (export remove-lambdas
          M0 unparse-M0 parse-M0
          M1 unparse-M1)
  (import
   (rnrs)
   (nanopass)
   (only (elegant-weapons helpers) binop? scalar-type? relop? gensym))

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
     (lambda t0 ((x t) ...) e)
     (invoke e e* ...)
     (call e e* ...)
     (op e1 e2)
     (var t x)))

  (define-parser parse-M0 M0)

  (define-language M1
    (extends M0)
    (entry Closures)
    (Closures
     (cl)
     (+ (closures (ctag ...) m)))
    (ClosureTag
     (ctag)
     (+ (x t (x* t*) ...)))
    (Expr
     (e)
     (- (lambda t0 ((x t) ...) e))
     ;; make a closure of type t, with variant x and bind the free
     ;; variables to e ...
     (+ (make-closure t x e ...))))

  (define-language M2
    (extends M1)
    (entry Module)
    (Expr
     (e)
     (- (make-closure t x e ...)
        (invoke e e* ...))))
  
  ;; We probably want two passes here, one to find all the lambdas,
  ;; and one to replace the invokes, and probably generate the ADTs
  ;; too.
  
  (define-pass uncover-lambdas : M0 (m) -> M1 ()
    (definitions
      (define closure-defs '())

      (define (free-vars e)
        (nanopass-case
         (M1 Expr) e
         
         ((var ,t ,x) (list x t))

         ;; TODO: We'll need to add lots more cases here.
         
         (else '()))))

    (Module : Module (m) -> Module ())
    
    (Expr : Expr (expr) -> Expr ()
          ((lambda ,[t] ((,x* ,t*) ...) ,[e])
           (let* ((tag (gensym 'lambda))
                  (fv (free-vars e))
                  (fvx (map car fv))
                  (fvt (map cadr fv)))
             (set! closure-defs
                   (cons (with-output-language
                          (M1 ClosureTag)
                          `(,tag ,t (,fvx ,fvt) ...)) closure-defs))
             `(make-closure ,t ,tag ,(map (lambda (x)
                                            `(var ,(car x) ,(cadr x)))
                                          fv) ...))))
    ;; We need a body too, which defines ADTs and dispatch functions
    ;; for all the closures.
    (with-output-language
     (M1 Closures)
     (let ((m (Module m)))
       `(closures (,closure-defs ...) ,m))))

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
        uncover-lambdas
        unparse-M1))
  )

