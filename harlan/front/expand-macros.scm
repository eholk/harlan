(library
    (harlan front expand-macros)
  (export expand-macros)
  (import
   (except (chezscheme) gensym)
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

  (define (mem* x ls)
    (cond
      ((and (pair? ls) (pair? (cdr ls)) (is-...? (cadr ls)))
       #f)
      ((pair? ls)
       (or (mem* x (car ls)) (mem* x (cdr ls))))
      (else (or (eq? x ls)))))

  (define-match reify
    (,x (guard (symbol? x))
        (getprop x 'rename x))
    ((,[e*] ...) e*)
    (,e e))
  
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

  (define (subst-only e env)
    (cond
      ((symbol? e)
       (let ((t (assq e env)))
         (if (and t (symbol? (cdr t)))
             (cdr t)
             e)))
      ((pair? e)
       (cons (subst-only (car e) env)
             (subst-only (cdr e) env)))
      (else e)))
  
  ;; This is a macro expander inspired by a typical metacircular
  ;; interpreter.
  ;;
  ;; The environment will either contain symbols, pairs or
  ;; procedures. Procedures are syntax transformers. The define-macro
  ;; transformer will be a special transformer that creates new
  ;; transformers.
  (define (expander x env)
    (cond
      ((symbol? x)
       (let ((t (assq x env)))
         (if t (cdr t) x)))
      ((pair? x)
       (let ((a (expander (car x) env)))
         (if (procedure? a)
             (expander (a (subst-only (cdr x) env) env) env)
             (cons a (map (lambda (x) (expander x env)) (cdr x))))))
      (else x)))

  (define (expand-define x env)
    (match x
      (((,f . ,x*) . ,b)
       (let* ((x*^ (map gensym x*))
              (define (rename-sym 'define))
              (env (append (map cons x* x*^)
                           env)))
         `(,define (,f . ,x*^) .
            ,(map (lambda (b) (subst-only b env)) b))))))
  
  (define (expand-let x env)
    (match x
      ((((,x ,e) ...) . ,b)
       (let* ((x^ (map gensym x))
              (let (rename-sym 'let))
              (env^ (append (map cons x x^) env)))
         `(,let ,(map list x^ e)
            . ,(map (lambda (b) (subst-only b env^)) b))))))

  (define (expand-kernel x env)
    (match x
      ((((,x ,e) ...) . ,b)
       (let* ((x^ (map gensym x))
              (let (rename-sym 'kernel))
              (env^ (append (map cons x x^) env)))
         `(,let ,(map list x^ e)
            . ,(map (lambda (b) (subst-only b env^)) b))))))

  ;; These are all the macros that are brought into scope with the
  ;; module form.
  (define module-prelude
    `((define . ,expand-define)
      (let . ,expand-let)
      (kernel . ,expand-kernel)))
    
  (define (expand-module x env)
    (cons
     (rename-sym 'module)
     (let loop ((x x)
                (env module-prelude))
       (match x
         (((define-macro ,name ,kw . ,patterns) . ,rest)
          (loop rest (cons (cons name (make-expander kw patterns))
                           env)))
         ((,a . ,rest)
          (cons (expander a env) (loop rest env)))
         (() '())))))

  (define (make-expander kw* patterns)
    (lambda (e env)
      (let loop ((patterns patterns))
        (if (null? patterns)
            (error 'apply-macro "Invalid syntax")
            (match-pat kw* (caar patterns) (cons '_ e)
                       (lambda (bindings)
                         (let ((template (cadar patterns)))
                           (subst* template bindings)))
                       (lambda ()
                         (loop (cdr patterns))))))))

  ;;(expand-macros '(module
  ;;                  (define-macro or ()
  ;;                    ((_ e1 e2) (let ((t e1)) (if t t e2))))
  ;;                  (define (add a b)
  ;;                    (or a b))))
  ;;
  ;;(expand-macros '(module
  ;;                  (define-macro or ()
  ;;                    ((_ e1 e2) (let ((t e1)) (if t t e2))))
  ;;                  (define (add a b)
  ;;                    (let ((if 5))
  ;;                      (or if b)))))
  
  (define (expand-macros x)
    (reify (expander x `((module . ,expand-module))))))
