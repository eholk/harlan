(library
    (harlan front expand-macros)
  (export expand-macros)
  (import
   (rnrs)
   (only (elegant-weapons helpers) gensym define-match ormap)
   (elegant-weapons record-case)
   (elegant-weapons match)
   (elegant-weapons sets))

  (define (match-pat kw* p e sk fk)
    (cond
      ((and (pair? p) (pair? (cdr p)) (is-...? (cadr p)))
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
      ((and (memq p kw*) (or (and (symbol? e) (eq? p e))
                             (and (ident? e) (eq? p (ident-symbol e)))))
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
      ((ident? x)
       (eq? (ident-symbol x) '...))
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
      ((and (ident? p) (assq (ident-symbol p) bindings)) => cdr)
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
      (else (or (eq? x ls)
                (and (ident? ls) (eq? x (ident-symbol ls)))))))

  ;; This time, we start by marking all identifiers with a color. The
  ;; idea is that once colored, any two identifiers that are eq?
  ;; should lexically refer to the same variable.

  (define-record-type ident (fields symbol id)
    (protocol (let ((next-id 0))
                (lambda (new)
                  (lambda (x)
                    (let ((id next-id))
                      (set! next-id (+ 1 next-id))
                      (new x id)))))))

  (define (color e env)
    (cond
      ((symbol? e)
       (let ((t (assq e env)))
         (if t
             (values (cdr t) env)
             (let ((id (make-ident e)))
               (values id (cons (cons e id) env))))))
      ((ident? e) (values e env))
      ((pair? e)
       (match e
         ((define-macro ,name . ,rest)
          (let-values (((name env) (color name env)))
            (let-values (((rest env^) (color rest env)))
              (values `(define-macro ,name . ,rest)
                      env))))
         (,else
          (let-values (((a env) (color (car e) env)))
            (let-values (((d env) (color (cdr e) env)))
              (values (cons a d) env))))))
      (else (values e env))))

  (define (uncolor e)
    (cond
      ((pair? e)
       (cons (uncolor (car e))
             (uncolor (cdr e))))
      ((ident? e)
       (ident-symbol e))
      (else e)))
    
  ;; I think we'll want two environments. The color environment maps
  ;; symbols onto identifiers. The macro environment will then map
  ;; identifiers onto macros. Macro expanders will definitely need to
  ;; capture the color environment, but I'm not sure about the macro
  ;; environment.

  
  
  ;; This is a macro expander inspired by a typical metacircular
  ;; interpreter.
  ;;
  ;; The environment will either contain symbols, pairs or
  ;; procedures. Procedures are syntax transformers. The define-macro
  ;; transformer will be a special transformer that creates new
  ;; transformers.
  (define (expander x colors macros)
    (cond
      ((pair? x)
       (let ((a (car x)))
         (cond
           ((assq a macros) =>
            (lambda (a)
              ((cdr a) (cdr x) colors macros)))
           (else (map (lambda (x) (expander x colors macros)) x)))))
      (else x)))

  (define (expand-module x colors)
    (cons
     'module
     (let loop ((x (cdr x))
                (macros '()))
       (match x
         (((define-macro ,name ,kw . ,patterns) . ,rest)
          (guard ;(eq? define-macro (cdr (assq 'define-macro colors)))
                 (ident? name))
          (loop rest
                (cons (cons name
                            (make-expander kw patterns colors macros))
                      macros)))
         ((,a . ,rest)
          (cons (expander a colors macros) (loop rest macros)))
         (() '())))))

  (define (make-expander kw* patterns colors macros)
    (let ((patterns (map (lambda (p)
                           (cons (uncolor (car p))
                                 (cdr p)))
                         patterns))
          (kw* (map ident-symbol kw*)))
      ;;(pretty-print patterns) (newline)
      ;;(display "colors ") (pretty-print colors) (newline)
      (lambda (e colors^ macros^)
        ;;(display "colors^ ") (pretty-print colors^) (newline)
        ;;(display "matching pattern ")
        ;;(pretty-print e)
        (let loop ((patterns patterns))
          (if (null? patterns)
              (error 'apply-macro "Invalid syntax")
              (match-pat kw* (caar patterns) (cons '_ e)
                         (lambda (bindings)
                           ;;(display bindings) (newline)
                           (let* ((template (cadar patterns))
                                  (e (subst* template bindings)))
                             ;;(display "precolor ") (display e) (newline)
                             (let-values
                                 (((e colors) (color e colors)))
                               ;;(display "postcolor ") (display e) (newline)
                               (expander e colors macros^))))
                         (lambda ()
                           (loop (cdr patterns)))))))))

;;  (expand-macros '(module
;;                    (define-macro or ()
;;                      ((_ e1 e2) (let ((t e1)) (if t t e2))))
;;                    (define (add a b)
;;                      (or a b))))
;;  
;;  (expand-macros '(module
;;                    (define-macro or ()
;;                      ((_ e1 e2) (let ((t e1)) (if t t e2))))
;;                    (define (add a b)
;;                      (let ((if 5))
;;                        (or if b)))))
  
  (define (reify x env)
    (cond
      ((pair? x)
       (let ((a (reify (car x) env)))
         (case a
           ((lambda)
            (match x
              ((,_ (,x* ...) ,b* ...)
               (let ((env (append (map (lambda (x)
                                         (cons x (gensym (ident-symbol x))))
                                       x*)
                                  env)))
                 `(lambda ,(reify x* env) . ,(reify b* env))))))
           ((define)
            (match x
              ((,define (,name ,x* ...) ,b* ...)
               (let ((env^ (cons (cons name (ident-symbol name))
                                 (append
                                  (map (lambda (x)
                                         (cons x
                                               (gensym (ident-symbol x))))
                                       x*)))))
                 `(define (,(reify name env^)
                           ,(reify x* env^) ...)
                    ,(reify b* env^) ...)))))
           ((let kernel)
            (match x
              ((,_ ((,x ,e) ...) ,b* ...)
               (let ((e (map (lambda (e)
                               (reify e env))
                             e))
                     (env (append (map (lambda (x)
                                         (cons x
                                               (gensym (ident-symbol x))))
                                       x)
                                  env)))
                 `(,a ((,(reify x env) ,e) ...)
                    ,(reify b* env) ...)))))
           ((let-region)
            (match x
              ((,_ (,r ...) ,b ...)
               (let ((env (append (map (lambda (x)
                                         (cons x
                                               (gensym (ident-symbol x))))
                                       r)
                                  env)))
               `(let-region (,(reify r env) ...)
                            ,(reify b env) ...)))))
           ((match)
            (match x
              ((,_ ,e
                   ((,tag ,x* ...) ,b ...) ...)
               (let ((e (reify e env))
                     (arms
                      (map (lambda (tag x* b)
                             (let ((env
                                    (cons (cons tag (ident-symbol tag))
                                          (append
                                           (map (lambda (x)
                                                  (cons x
                                                        (gensym
                                                         (ident-symbol x))))
                                                x*)
                                           env))))
                               (reify `((,tag ,x* ...) ,b ...) env)))
                           tag x* b)))
                 `(match ,e . ,arms)))))
           (else (cons a (reify (cdr x) env))))))
      ((ident? x)
       (let ((t (assq x env)))
         (if t
             (cdr t)
             (ident-symbol x))))
      (else x)))
         
  (define (expand-macros x)
    (let-values (((x colors) (color x '())))
      (reify (expand-module x colors) '())))

  ;; end library
  )
