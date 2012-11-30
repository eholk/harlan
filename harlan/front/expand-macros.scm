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
  
  (define (match-pat kw* p e sk fk)
    (let ((e (expose e)))
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
        (else (fk)))))

  (define (subst* e bindings)
    (match e
      (,x (guard (symbol? x))
          (let ((t (assq x bindings)))
            (if t
                (cdr t)
                x)))
      ((,[e] . ,[e*]) `(,e . ,e*))
      (() '())))
  
  
  (define (apply-macro kw* patterns e)
    (if (null? patterns)
        (error 'apply-macro "Invalid syntax")
        (match-pat kw* (caar patterns) e
                   (lambda (bindings)
                     (subst* (cadar patterns) bindings))
                   (lambda ()
                     (apply-macro kw* (cdr patterns) e)))))
  
  (define (expand-one e env)
    (match (expose e)
      ((,m . ,args)
       (guard (assq (expose m) env))
       (expand-one ((cdr (assq (expose m) env)) `(,(expose m) . ,args)) env))
      ((,[(lambda (x) (expand-one x env)) -> e*] ...) e*)
      (,x x)))

  (define-match reify
    (,x (guard (symbol? x))
        (getprop x 'rename x))
    ((,[e*] ...) e*)
    (,e e))
  
  ;; This is the main expander driver. It combines parsing too.
  (define (expand-top e env)
    (match e
      (((define-macro ,name (,kw* ...)
          ,patterns ...) . ,rest)
       (guard (symbol? name))
       (expand-top rest (cons (cons name (lambda (e)
                                           (apply-macro kw* patterns e)))
                              env)))
      ((,e . ,[e*])
       (cons (reify (expand-one e env)) e*))
      (() '())))
       
  (define (expand-let e)
    (match-pat
     '()
     `(_ ((x e)) . b)
     e
     (lambda (env)
       (let ((x (cdr (assq 'x env)))
             (e (cdr (assq 'e env)))
             (b (cdr (assq 'b env))))
         (let ((let (gensym 'let))
               (x^ (gensym x)))
           (putprop let 'rename 'let)
           `(,let ((,x^ ,e))
              ,@(map (lambda (b) `(subst ,b (,x . ,x^))) b)))))
     (lambda () (error 'expand-let "invalid syntax" e))))

  ;;(subst y (x . x^))       => y
  ;;(subst x (x . x^))       => (subst x^ (x . x^))
  ;;(subst (e ...) (x . x^)) => ((subst e x x^) ...)
  ;;(subst (subst e (x . x^)) (y . y^)) =>
  ;;  (expose (subst e (x . x^) (y . y^)))
  
  (define (expose e)
    (match e
      ((subst ,x . ,s*)
       (guard (symbol? x))
       (let ((match (assq x s*)))
         (if match
             (expose `(subst ,(cdr match) . ,s*))
             x)))
      ((subst (subst ,e . ,s1) . ,s2)
       (expose `(subst ,e . ,(append s1 s2))))
      ((subst ,atom . ,s*)
       (guard (not (pair? atom)))
       atom)
      ((subst (,e* ...) . ,s*)
       (map (lambda (e)
              `(subst ,e . ,s*))
            e*))
      ((,e . , e*)
       (guard (not (eq? e 'subst)))
       `(,e . ,e*))
      (,x (guard (not (pair? x))) x)))
    
  (define primitive-env `((let . ,expand-let)))
    
  (define (expand-macros x)
    ;; Assume we got a (module decl ...) form
    `(module . ,(expand-top (cdr x) primitive-env))))
