(library
    (harlan front expand-macros)
  (export expand-macros)
  (import
   (except (chezscheme) gensym)
   (only (elegant-weapons helpers) gensym define-match)
   (elegant-weapons match))

  ;; This is basically going to be syntax-case as given in Dybvig et
  ;; al, 1992. We'll have a notion of primitive syntax and extended
  ;; syntax. We need to be sure that there is primitive syntax for
  ;; every binding form in Harlan, or else we'll get all the capture
  ;; wrong.

  (define-record-type ident (fields name binding marks))
  
  (trace-define (match-pat kw* p e sk fk)
    (cond
      ((and (pair? p) (pair? e))
       (match-pat kw* (car p) (car e)
                  (lambda (bindings)
                    (match-pat kw* (cdr p) (cdr e)
                               (lambda (bindings^)
                                 (sk (append bindings bindings^)))
                               fk))
                  fk))
      ((and (memq p kw*) (eq? p e))
       (sk '()))
      ((symbol? p)
       (sk (list (cons p e))))
      ((and (null? p) (null? e))
       (sk '()))
      (else (fk))))

  (define (subst* e bindings)
    (match e
      (,x (guard (symbol? x))
          (let ((t (assq x bindings)))
            (if t
                (cdr t)
                x)))
      ((,[e] . ,[e*]) `(,e . ,e*))
      (() '())))
  
  
  (trace-define (apply-macro kw* patterns e)
    (if (null? patterns)
        (error 'apply-macro "Invalid syntax")
        (match-pat kw* (caar patterns) e
                   (lambda (bindings)
                     (subst* (cadar patterns) bindings))
                   (lambda ()
                     (apply-macro kw* (cdr patterns) e)))))
  
  (define (expand-one e env)
    (match e
      ((,m . ,args) (guard (assq m env))
       (expand-one ((cdr (assq m env)) `(,m . ,args)) env))
      ((,[e*] ...) e*)
      (,x x)))

  (define-match reify
    (,x (guard (symbol? x))
        (getprop x 'rename x))
    ((,[e*] ...) e*)
    (,e e))
  
  ;; This is the main expander driver. It combines parsing too.
  (define (expand-top e env)
    (match e
      (((define-syntax (,name ,x)
          (syntax-case ,x
              (,kw* ...)
            ,patterns ...)) . ,rest)
       (guard (and (symbol? name) (symbol? x)))
       (expand-top rest (cons (cons name (lambda (e)
                                           (apply-macro kw* patterns e)))
                              env)))
      ((,e . ,[e*])
       (cons (reify (expand-one e env)) e*))
      (() '())))
       
  (define (expand-let e)
    (match e
      ((let ((,x ,e)) ,b ...)
       (let ((let (gensym 'let))
             (x^ (gensym x)))
         (putprop let 'rename 'let)
         `(,let ((,x^ ,e)) ,@(map (lambda (b) `(subst ,b ,x ,x^)) b))))))

  (define (expand-subst e)
    (match e
      ((subst ,e ,x ,x^) (guard (and (symbol? x) (eq? e x))) x^)
      ((subst (subst ,e ,y ,y^) ,x ,x^)
       `(subst ,(expand-subst `(subst ,e ,y ,y^)) ,x ,x^))
      ((subst (,e ,e* ...) ,x ,x^)
       (guard (and (symbol? e) (symbol? x)))
       (cons (if (eq? e x) x^ e) (map (lambda (e) `(subst ,e ,x ,x^)) e*)))
      ((subst (,e ...) ,x ,x^) (guard (symbol? x))
       (map (lambda (e) `(subst ,e ,x ,x^)) e))
      ((subst ,e ,x ,x^) (guard (and (symbol? x) (not (pair? e)))) e)))
    
  (define primitive-env `((let . ,expand-let)
                          (subst . ,expand-subst)))
    
  (define (expand-macros x)
    ;; Assume we got a (module decl ...) form
    `(module . ,(expand-top (cdr x) primitive-env))))
