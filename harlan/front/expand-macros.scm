(library
    (harlan front expand-macros)
  (export expand-macros)
  (import
   (chezscheme)
   (elegant-weapons match))

  ;; This is basically going to be syntax-case as given in Dybvig et
  ;; al, 1992. We'll have a notion of primitive syntax and extended
  ;; syntax. We need to be sure that there is primitive syntax for
  ;; every binding form in Harlan, or else we'll get all the capture
  ;; wrong.

  (define (match-pat kw* p e sk fk)
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
      (else (error 'match-path "Couldn't match pattern" kw* p e))))

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
    (match e
      ((,m . ,args) (guard (assq m env))
       (expand-one ((cdr (assq m env)) `(,m . ,args)) env))
      ((,[e*] ...) e*)
      (,x x)))
  
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
       (cons (expand-one e env) e*))
      (() '())))
       

  (define primitive-env '())
    
  (define (expand-macros x)
    ;; Assume we got a (module decl ...) form
    `(module . ,(expand-top (cdr x) primitive-env))))
