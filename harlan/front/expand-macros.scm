(library
    (harlan front expand-macros)
  (export expand-macros)
  (import
   (except (chezscheme) gensym)
   (only (elegant-weapons helpers) gensym define-match)
   (elegant-weapons match)
   (elegant-weapons sets))

  ;; This is basically going to be syntax-case as given in Dybvig et
  ;; al, 1992. We'll have a notion of primitive syntax and extended
  ;; syntax. We need to be sure that there is primitive syntax for
  ;; every binding form in Harlan, or else we'll get all the capture
  ;; wrong.

  (define-record-type ident (fields name marks (mutable symbol))
    (protocol (lambda (new)
                (lambda (name marks)
                  (new name marks #f)))))

  (define-record-type mark (fields id)
    (protocol (let ((next-id 0))
                (lambda (new)
                  (lambda ()
                    (let ((id next-id))
                      (set! next-id (+ 1 next-id))
                      (new id)))))))
  
  (define-record-type wrap (fields expr marks subst))
  
  ;;(define (reify-ident x)
  ;;  (if (null? (ident-marks x))
  ;;      (ident-name x)
  ;;      (let ((sym (ident-symbol x)))
  ;;        (if sym
  ;;            sym
  ;;            (begin
  ;;              (let ((sym (gensym (ident-name x))))
  ;;                (ident-symbol-set! x sym)
  ;;                sym))))))
  (define reify-ident ident-name)

  (define (reify-idents e)
    (define (rename x) (gensym (ident-name x)))
    (let reify-idents ((e e)
                       (env '()))
      (let-syntax ((reify-primitives
                    (syntax-rules ()
                      ((_ ((name . pattern)
                           body) ...)
                       (match e
                         ((,x . pattern)
                          (guard (and (not (lookup-ident x env))
                                      (eq? 'name (ident-name x))))
                          body) ...
                         (,else (cons (reify-idents (car e) env)
                                      (reify-idents (cdr e) env))))))))
        (cond
          ((and (pair? e) (ident? (car e)))
           (reify-primitives
            ((let-region ,r* ,body)
             (let ((r*^ (map rename r*)))
               `(let-region ,r*^ ,(reify-idents
                                   body (append (map cons r* r*^) env)))))
            ((kernel ((,x ,e) ...) ,body)
             (let ((x^ (map rename x))
                   (e (map (lambda (e) (reify-idents e env)) e)))
               `(kernel ,(map list x^ e)
                  ,(reify-idents body (append (map cons x x^) env)))))
            ((match ,e
               ((,tag ,x* ...) ,b) ...)
             `(match ,(reify-idents e env)
                ,(map (lambda (tag x* body)
                        (let ((tag (reify-idents tag env))
                              (x*^ (map rename x*)))
                          `((,tag . ,x*^)
                            ,(reify-idents body
                                           (append (map cons x* x*^) env)))))
                      tag x* b)))
            ((let ((,x ,e) ...) ,body)
             (let ((x^ (map rename x))
                   (e (map (lambda (e) (reify-idents e env)) e)))
               `(let ,(map list x^ e)
                  ,(reify-idents body (append (map cons x x^) env)))))
            ((define (,fn ,args ...) ,body)
             (let ((args^ (map rename args)))
               `(define (,(ident-name fn) . ,args^)
                  ,(reify-idents body
                                 (append (map cons args args^) env)))))))
          ((pair? e)
           (cons (reify-idents (car e) env)
                 (reify-idents (cdr e) env)))
          ((ident? e)
           (let ((name (lookup-ident e env)))
             (if name
                 (cdr name)
                 (reify (reify-ident e)))))
          ((wrap? e)
           (reify-idents (expose e) env))
          (else e)))))

  (define (gen-ident x)
    (if (wrap? x)
        (gen-ident (expose x))
        (make-ident (gensym (ident-name x)) (ident-marks x))))
  
  ;;(define merge-marks disjoint-union)
  (define merge-marks union)
  
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
        ((and (memq p kw*) (ident? e) (eq? p (ident-name e)))
         (sk '()))
        ((and (symbol? p) (not (memq p kw*)))
         (if (memq e kw*)
             (error 'match-pat "misplaced aux keyword" e)
             (sk (list (cons p e)))))
        ((and (null? p) (null? e))
         (sk '()))
        (else (fk)))))

  (define (is-...? x)
    (cond
      ((symbol? x)
       (eq? x '...))
      ((ident? x)
       (eq? '... (ident-name x)))
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
      ((lookup-ident p bindings) => cdr)
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
      (else (or (eq? x ls) (ident-equal? x ls)))))

  (define (apply-macro kw* patterns e)
    (if (null? patterns)
        (error 'apply-macro "Invalid syntax")
        (match-pat kw* (caar patterns) e
                   (lambda (bindings)
                     ;;(pretty-print "ABCDEF\n")
                     (let* ((mark (make-mark))
                            (bindings (map (lambda (b)
                                             (cons (make-ident (car b)
                                                               (list mark))
                                                   (cdr b)))
                                           bindings))
                            (template (begin
                                        ;;(pretty-print (cadar patterns))
                                        (expose (make-wrap
                                                 (cadar patterns)
                                                 (list mark)
                                                 '())))))
                       ;;(pretty-print bindings)
                       ;;(pretty-print template)
                       (subst* template bindings)))
                   (lambda ()
                     (apply-macro kw* (cdr patterns) e)))))

  (define (ident-equal? a b)
    ;;(if (eq? (ident-name a) (ident-name b))
    ;;    (begin (display `(,a ,b)) (newline)))
    (and (eq? (ident-name a) (ident-name b))
         (set-equal? (ident-marks a) (ident-marks b))))

  (define (lookup-ident x env)
    (let loop ((x x)
               (env env)
               (result #f))
      (cond
        ((not (ident? x)) result)
        ((null? env) result)
        ((ident-equal? x (caar env)) (car env))
        (else (loop x (cdr env) result)))))

  (define (expand-one e env)
    (match (expose e)
      ((,m . ,args)
       (guard (lookup-ident (expose m) env))
       (let* ((mark (make-mark))
              (e (make-wrap `(,m . ,args) (list mark) '()))
              (e^ ((cdr (lookup-ident (expose m) env)) e)))
         (expand-one (expose (make-wrap
                              e^
                              (list mark)
                              '())) env)))
      ((,[(lambda (x) (expand-one x env)) -> e*] ...) e*)
      (,x x)))

  (define-match reify
    (,x (guard (symbol? x))
        (getprop x 'rename x))
    ((,[e*] ...) e*)
    (,e e))
  
  ;; This is the main expander driver. It combines parsing too.
  (define (expand-top e env)
    (match (expose e)
      (((define-macro ,name ,kw*
          ,patterns ...) . ,rest)
       (guard (symbol? name))
       (expand-top rest (cons (cons (make-ident name '())
                                    (lambda (e)
                                      (apply-macro kw* patterns e)))
                              env)))
      ((,e . ,[e*])
       (cons (expand-one (make-wrap e '() '()) env) e*))
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
               (x^ (map gen-ident x)))
           (putprop let 'rename 'let)
           (match #t
             (#t
              `(,let ((,x^ ,e) ...)
                 ,(make-wrap `(begin ,b ...) '() (map cons x x^))))))))
     (lambda () (error 'expand-let "invalid syntax" e))))

  (define (expand-kernel e)
    (match-pat
     '()
     `(_ ((x e) ...) b ...)
     e
     (lambda (env)
       (let ((x (get-... 'x env))
             (e (get-... 'e env))
             (b (get-... 'b env)))
         (let ((kernel (rename-sym 'kernel))
               (x^ (map gen-ident x)))
           (match #t
             (#t
              `(,kernel ((,x^ ,e) ...)
                 ,(make-wrap `(begin ,b ...) '() (map cons x x^))))))))
     (lambda () (error 'expand-kernel "invalid syntax" e))))

  (define (expand-define e)
    (match-pat
     '()
     `(_ (f x ...) b ...)
     e
     (lambda (env)
       (let ((x (get-... 'x env))
             (b (get-... 'b env)))
         (let ((define (gensym 'define))
               (x^ (map gen-ident x)))
           (putprop define 'rename 'define)
           (match #t
             (#t
              `(,define (,(lookup 'f env) ,x^ ...)
                 ,(make-wrap `(begin ,b ...) '() (map cons x x^))))))))
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
               (r^ (map gen-ident r)))
           (putprop let-region 'rename 'let-region)
           (match #t
             (#t
              `(,let-region (,r^ ...)
                            ,(make-wrap `(begin ,b ...)
                                        '()
                                        (map cons r r^))))))))
     (lambda () (error 'expand-let-region "invalid syntax" e))))

  (define (expand-match e)
    (match-pat
     '()
     '(_ e arms ...)
     e
     (lambda (env)
       (let ((arms (get-... 'arms env)))
         `(,(rename-sym 'match) ,(lookup 'e env)
           . ,(map expand-match-arm arms))))
     (lambda () (error 'expand-match "invalid syntax" e))))

  (define (expand-match-arm arm)
    (match-pat
     '()
     '((tag x ...) b ...)
     arm
     (lambda (env)
       (let* ((x (get-... 'x env))
              (b (get-... 'b env))
              (x^ (map gen-ident x))
              (s* (map cons x x^)))
         `((,(lookup 'tag env) . ,x^)
           . ,(map (lambda (b)
                     (make-wrap b '() s*))
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

  (define (expose e)
    (if (wrap? e)
        (let ((e (wrap-expr e))
              (m (wrap-marks e))
              (s (wrap-subst e)))
          (cond
            ((pair? e)
             (map (lambda (e)
                    (expose (make-wrap e m s)))
                  e))
            ((symbol? e)
             (expose (make-wrap (make-ident e m) m s)))
            ((ident? e)
             (let ((match (lookup-ident e s)))
               (if match
                   (expose (make-wrap (cdr match) m s))
                   (make-ident (ident-name e)
                               (merge-marks m (ident-marks e))))))
            ((wrap? e)
             (expose (make-wrap (wrap-expr e)
                                (merge-marks m (wrap-marks e))
                                (append s (wrap-subst e)))))
            (else e)))
        e))
    
  (define primitive-env
    (map (lambda (b)
           (cons (make-ident (car b) '())
                 (cdr b)))
         `((let . ,expand-let)
           (define . ,expand-define)
           (let-region . ,expand-let-region)
           (match . ,expand-match)
           (kernel . ,expand-kernel))))

  (define (expand-macros x)
    ;; Assume we got a (module decl ...) form
    `(module . ,(reify-idents
                 (expand-top (cdr x)
                             '() #;primitive-env)))))
