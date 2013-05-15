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

  (define substk (gensym 'subst))
  
(define (match-pat kw* p e sk fk)
  (let ((e (expose e)))
    (cond
      ((and (pair? p) (pair? (cdr p)) (eq? '... (cadr p)))
       (let loop ((e e)
                  (b '()))
         (if (null? e)
             (match-pat kw* (cddr p) e
                        (lambda (b^) (sk `((... . ,b) . ,b^)))
                        fk)
             (match-pat kw* (car p) (car e)
                        (lambda (b^)
                          (loop (cdr e) (snoc b b^)))
                        (lambda ()
                          (match-pat kw* (cddr p) e
                                     (lambda (b^) (sk `((... . ,b) . ,b^)))
                                     fk))))))
      ((and (pair? p) (pair? e))
       (match-pat kw* (car p) (car e)
                  (lambda (b)
                    (match-pat kw* (cdr p) (cdr e)
                               (lambda (b^) (sk (append b b^)))
                               fk))
                  fk))
      ((eq? p '_)
       (sk '()))
      ((and (memq p kw*) (eq? p e))
       (sk '()))
      ((and (symbol? p) (not (memq p kw*)))
       (if (memq e kw*)
           (error 'match-pat "misplaced aux keyword" e)
           (sk (list (cons p e)))))
      ((and (null? p) (null? e))
       (sk '()))
      (else (fk)))))

  (define (subst* p bindings)
    (cond
      ((and (pair? p) (pair? (cdr p)) (eq? '... (cadr p)))
       (let ((bindings... (extract-... (car p) bindings)))
         (if (null? bindings...)
             (subst* (cddr p) bindings)
             (append
              (apply map (cons (lambda b*
                                 (subst* (car p)
                                         (append (apply append b*)
                                                 bindings)))
                               bindings...))
              (subst* (cddr p) bindings)))))
      ((pair? p)
       (cons (subst* (car p) bindings)
             (subst* (cdr p) bindings)))
      ((assq p bindings) => cdr)
      (else p)))

(define (snoc d a)
  (if (null? d)
      (list a)
      (cons (car d) (snoc (cdr d) a))))

(define (extract-... p bindings)
  (if (null? bindings)
      '()
      (let ((rest (extract-... p (cdr bindings)))
            (b (car bindings)))
        (if (and (eq? (car b) '...) (not (null? (cdr b))))
            (let ((names (map car (cadr b))))
              (if (ormap (lambda (x) (mem* x p)) names)
                  (cons (cdr b) rest)
                  rest))
            rest))))

(define (mem* x ls)
  (cond
    ((and (pair? ls) (pair? (cdr ls)) (eq? (cadr ls) '...))
     #f)
    ((pair? ls)
     (or (mem* x (car ls)) (mem* x (cdr ls))))
    (else (eq? x ls))))

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
   `(_ ((x e) ...) b ...)
   e
   (lambda (env)
     (let ((x (get-... 'x env))
           (e (get-... 'e env))
           (b (get-... 'b env)))
       (let ((let (gensym 'let))
             (x^ (map gensym x)))
         (putprop let 'rename 'let)
         (match #t
           (#t
            `(,let ((,x^ ,e) ...)
               (,substk (begin ,b ...) (,x . ,x^) ...)))))))
   (lambda () (error 'expand-let "invalid syntax" e))))

(define (expand-define e)
  (match-pat
   '()
   `(_ (f x ...) b ...)
   e
   (lambda (env)
     (let ((x (get-... 'x env))
           (b (get-... 'b env)))
       (let ((define (gensym 'define))
             (x^ (map gensym x)))
         (putprop define 'rename 'define)
         (match #t
           (#t
            `(,define (,(lookup 'f env) ,x^ ...)
               (,substk (begin ,b ...) (,x . ,x^) ...)))))))
   (lambda () (error 'expand-define "invalid syntax" e))))

(define (expand-let-region e)
  (match-pat
   '()
   `(_ (r ...) b ...)
   e
   (lambda (env)
     (let ((r (get-... 'r env))
           (b (get-... 'b env)))
       (let ((let-region (gensym 'let-region))
             (r^ (map gensym r)))
         (putprop let-region 'rename 'let-region)
         (match #t
           (#t
            `(,let-region (,r^ ...)
               (,substk (begin ,b ...) (,r . ,r^) ...)))))))
   (lambda () (error 'expand-let-region "invalid syntax" e))))

(define (expand-match e)
  (match-pat
   '()
   '(_ e arms ...)
   e
   (lambda (env)
     (let ((arms (get-... 'arms env)))
       `(,(rename-sym 'match) ,(lookup 'e env) . ,(map expand-match-arm arms))))
   (lambda () (error 'expand-match "invalid syntax" e))))

(define (expand-match-arm arm)
  (match-pat
   '()
   '((tag x ...) b ...)
   arm
   (lambda (env)
     (let* ((x (get-... 'x env))
            (b (get-... 'b env))
            (x^ (map gensym x))
            (s* (map cons x x^)))
       `((,(lookup 'tag env) . ,x^)
         . ,(map (lambda (b)
                   `(,substk ,b . ,s*))
                 b))))
   (lambda () (error 'expand-match-arm "invalid syntax" arm))))

(define (rename-sym x)
  (let ((x^ (gensym x)))
    (putprop x^ 'rename x)
    x^))

(define (get-... x env)
  (match env
    (() '())
    (((... . ,env) . ,rest)
     (let ((env (map (lambda (e) (assq x e)) env)))
       (if (ormap (lambda (x) x) env)
           (map cdr env)
           (get-... x rest))))
    ((,a . ,d)
     (get-... x d))))

(define (lookup x env)
  (cdr (assq x env)))

  ;;(subst y (x . x^))       => y
  ;;(subst x (x . x^))       => (subst x^ (x . x^))
  ;;(subst (e ...) (x . x^)) => ((subst e x x^) ...)
  ;;(subst (subst e (x . x^)) (y . y^)) =>
  ;;  (expose (subst e (x . x^) (y . y^)))
  
  (define (expose e)
    (match e
      ((,subst ,x . ,s*)
       (guard (and (symbol? x) (eq? subst substk)))
       (let ((match (assq x s*)))
         (if match
             (expose `(,substk ,(cdr match) . ,s*))
             x)))
      ((,subst1 (,subst2 ,e . ,s1) . ,s2)
       (guard (and (eq? subst1 substk) (eq? subst2 substk)))
       (expose `(,substk ,e . ,(append s1 s2))))
      ((,subst ,atom . ,s*)
       (guard (and (eq? subst substk) (not (pair? atom))))
       atom)
      ((,subst (,e* ...) . ,s*)
       (guard (eq? subst substk))
       (map (lambda (e)
              `(,substk ,e . ,s*))
            e*))
      ((,e . , e*)
       (guard (not (eq? e substk)))
       `(,e . ,e*))
      (,x (guard (not (pair? x))) x)))
    
  (define primitive-env `((let . ,expand-let)
                          (define . ,expand-define)
                          (let-region . ,expand-let-region)
                          (match . ,expand-match)))
    
  (define (expand-macros x)
    ;; Assume we got a (module decl ...) form
    `(module . ,(expand-top (cdr x) primitive-env))))
