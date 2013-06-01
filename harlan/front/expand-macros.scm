(library
    (harlan front expand-macros)
  (export expand-macros)
  (import
   (except (chezscheme) gensym record-case)
   (only (elegant-weapons helpers) gensym define-match)
   (elegant-weapons match)
   (elegant-weapons sets))

  (define (match-pat kw* p e sk fk)
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
      ((and (memq p kw*) (symbol? e) (eq? p e))
       (sk '()))
      ((and (symbol? p) (not (memq p kw*)))
       (if (memq e kw*)
           (error 'match-pat "misplaced aux keyword" e)
           (sk (list (cons p e)))))
      ((and (null? p) (null? e))
       (sk '()))
      (else (fk))))

  (define (is-...? x)
    (cond
      ((symbol? x)
       (eq? x '...))
      (else #f)))
  
  (define (subst* p bindings)
    (cond
      ((and (pair? p) (pair? (cdr p)) (is-...? (cadr p)))
       (let ((bindings... (extract-... (car p) bindings)))
         (if (null? bindings...)
             (subst* (cddr p) bindings)
             (append
              (apply map (lambda b*
                           (subst* (car p)
                                   (append (apply append b*)
                                           bindings)))
                     bindings...)
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
          (if (and (is-...? (car b)) (not (null? (cdr b))))
              (let ((names (map car (cadr b))))
                (if (ormap (lambda (x) (mem* x p)) names)
                    (cons (cdr b) rest)
                    rest))
              rest))))

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

  (define (mem* x ls)
    (cond
      ((and (pair? ls) (pair? (cdr ls)) (is-...? (cadr ls)))
       #f)
      ((pair? ls)
       (or (mem* x (car ls)) (mem* x (cdr ls))))
      (else (or (eq? x ls)))))

  ;; Record-case is pretty handy, we should probably move it over to
  ;; Elegant Weapons.
  (define-syntax bind-record
    (syntax-rules ()
      ((_ rtd t i (x x* ...) b ...)
       (let ((access (record-accessor rtd i)))
         (let ((x (access t)))
           (bind-record rtd t (+ 1 i) (x* ...) b ...))))
      ((_ rtd t i () b ...)
       (begin b ...))))

  (define-syntax match-record
    (lambda (x)
      (syntax-case x ()
        ((_ t ((name x ...) b ...) rest)
         #`(if (#,(datum->syntax #'name (string->symbol
                                          (string-append
                                           (symbol->string
                                            (syntax->datum #'name))
                                           "?"))) t)
               (let ((rtd (record-rtd t)))
                  (bind-record rtd t 0 (x ...) b ...))
               rest)))))
  
  (define-syntax record-case
    (syntax-rules (else)
      ((_ e ((name x ...) b ...) rest ...)
       (let ((t e))
         (match-record t ((name x ...) b ...)
                       (record-case t rest ...))))
      ((_ e (else b ...))
       (begin b ...))
      ((_ e) (void))))

  ;; Mutable environments... this is horrible.
  (define-record-type environ
    (fields (mutable env)))

  (define (empty-env)
    (make-environ '()))

  (define (clone-env e)
    (make-environ (environ-env e)))

  (define (push-env e x v)
    (environ-env-set! e (cons (cons x v)
                              (environ-env e)))
    e)

  (define (extend-env e x v)
    (record-case e
      ((environ env)
       (make-environ (cons (cons x v) env)))))

  (define (extend-env* e extension)
    (record-case e
      ((environ env)
       (make-environ (append extension env)))))
  
  (define-syntax lookup
    (syntax-rules (else)
      ((_ x e
          (y => b)
          (else er))
       (let ((t (assq x (environ-env e))))
         (if t
             (let ((y (cdr t))) b)
             er)))))

  ;; Syntactic closures and identifiers.

  (define-record-type sc (fields expr env))

  (define-record-type ident (fields symbol env))

  ;; Now, stuff to manipulate syntactic closures and such.

  ;; Takes a syntactic object and transports it to a new environment.
  (define (capture e env)
    (record-case e
      ((sc e env^)
       (make-sc e env))
      ((ident x env^)
       (make-ident x env))
      (else
       (make-sc e env))))
  
  (define (expose e)
    (record-case e
      ((sc e env)
       (cond
         ((pair? e) (map expose e))
         ((symbol? e) (make-ident e env))
         ((sc? e) (expose e))
         (else e)))
      ((ident x env)
       e)
      (else e)))

  (define (reify e)
    (record-case e
      ((ident x e)
       (lookup x e
               (x => x)
               (else x)))
      (else
       (cond
         ((pair? e) (map reify e))
         (else e)))))
    
  ;; This is a macro expander inspired by a typical metacircular
  ;; interpreter.
  ;;
  ;; The environment will either contain symbols, pairs or
  ;; procedures. Procedures are syntax transformers. The define-macro
  ;; transformer will be a special transformer that creates new
  ;; transformers.
  (define (expander x env)
    (record-case x
      ((ident x env^)
       (lookup x env^
               (x => x)
               (else x)))
      ((sc x env^)
       (expander (expose x) env^))
      (else       
       (cond
         ((symbol? x)
          (lookup x env
                  (x => x)
                  (else x)))
         ((pair? x)
          (let ((a (expander (car x) env)))
            (if (procedure? a)
                (a (cdr x) env)
                (cons a (map (lambda (x) (expander x env)) (cdr x))))))
         (else x)))))

  (define (expand-define x env)
    (match x
      (((,f . ,x*) . ,b)
       (let* ((x*^ (map gensym x*))
              (env (extend-env* env (map cons x* x*^))))
         `(define (,f . ,x*^) .
            ,(map (lambda (b) (expander (capture b env) env)) b))))))
  
  (define (expand-let x env)
    (match x
      ((((,x ,e) ...) . ,b)
       (let* ((x^ (map gensym x))
              (env^ (extend-env* env (map cons x x^)))
              (e (map (lambda (e) (expander (capture e env) env)) e)))
         `(let ,(map list x^ e)
            . ,(map (lambda (b) (expander (capture b env^) env^)) b))))))

  (define (expand-kernel x env)
    (match x
      ((((,x ,e) ...) . ,b)
       (let* ((x^ (map gensym x))
              (env^ (extend-env* env (map cons x x^)))
              (e (map (lambda (e) (expander (capture e env) env)) e)))
         `(kernel ,(map list x^ e)
            . ,(map (lambda (b) (expander (capture b env^) env^)) b))))))

  ;; These are all the macros that are brought into scope with the
  ;; module form.
  (define module-prelude
    (make-environ
     `((define . ,expand-define)
       (let . ,expand-let)
       (kernel . ,expand-kernel))))
    
  (define (expand-module x env)
    (cons
     'module
     (let loop ((x x)
                (env module-prelude))
       (match x
         (((define-macro ,name ,kw . ,patterns) . ,rest)
          (loop rest (extend-env env name (make-expander kw patterns env))))
         ((,a . ,rest)
          (cons (expander a env) (loop rest env)))
         (() '())))))

  (define (make-expander kw* patterns env)
    (lambda (e env^)
      (let loop ((patterns patterns))
        (if (null? patterns)
            (error 'apply-macro "Invalid syntax")
            (match-pat kw* (caar patterns) (cons '_ e)
                       (lambda (bindings)
                         (let* ((template (cadar patterns))
                                (e (subst* template bindings)))
                           (expander (make-sc e env) env^)))
                       (lambda ()
                         (loop (cdr patterns))))))))

  ;;(expand-macros '(module
  ;;                  (define-macro or ()
  ;;                    ((_ e1 e2) (let ((t e1)) (if t t e2))))
  ;;                  (define (add a b)
  ;;                    (or a b))))
  
  ;;(expand-macros '(module
  ;;                  (define-macro or ()
  ;;                    ((_ e1 e2) (let ((t e1)) (if t t e2))))
  ;;                  (define (add a b)
  ;;                    (let ((if 5))
  ;;                      (or if b)))))
  
  (define (expand-macros x)
    (reify (expander x (make-environ `((module . ,expand-module))))))

  ;; end library
  )
