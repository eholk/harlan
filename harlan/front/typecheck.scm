(library
  (harlan front typecheck)
  (export typecheck)
  (import
    (rnrs)
    (elegant-weapons match)
    (elegant-weapons helpers)
    (harlan compile-opts)
    (util color))

  (define (typecheck m)
    (let-values (((m s) (infer-module m)))
      `(module . ,m)))

  (define-record-type tvar (fields name))
  (define-record-type rvar (fields name))

  ;; Walks type and region variables in a substitution
  (define (walk x s)
    (let ((x^ (assq x s)))
      ;; TODO: We will probably need to check for cycles.
      (if x^
          (let ((x (cdr x^)))
            (if (or (tvar? x) (rvar? x))
                (walk x s)
                x))
          x)))
              
  (define (walk-type t s)
    (match t
      (int   'int)
      (float 'float)
      (bool  'bool)
      (void  'void)
      ((vector ,r ,[t]) `(vector ,r ,t))
      (((,[t*] ...) -> ,[t]) `((,t* ...) -> ,t))
      (,x (guard (tvar? x))
          (let ((x^ (walk x s)))
            (if (equal? x x^)
                x
                (walk-type x^ s))))))
  
  ;; Unifies types a and b. s is an a-list containing substitutions
  ;; for both type and region variables. If the unification is
  ;; successful, this function returns a new substitution. Otherwise,
  ;; this functions returns #f.
  (define (unify-types a b s)
    (match `(,a ,b)
      ;; Obviously equal types unify.
      ((,a ,b) (guard (equal? (walk-type a s) (walk-type b s))) s)
      ((,a ,b) (guard (tvar? a)) `((,a . ,b) . ,s))
      ((,a ,b) (guard (tvar? b)) `((,b . ,a) . ,s))
      (((vector ,ra ,a) (vector ,rb ,b))
       (let ((s (unify-types a b s)))
         (and s
              (if (eq? ra rb)
                  s
                  `((,ra . ,rb) . ,s)))))
      (,else #f)))

  (define (type-error e expected found)
    (error 'typecheck
           "Could not unify types."
           e expected found))

  (define (return e t)
    (lambda (r s)
      (values e t s)))

  (define (bind m seq)
    (lambda (r s)
      (let-values (((e t s) (m r s)))
        ((seq e t) r s))))

  (define (unify a b seq)
    (lambda (r s)
      (let ((s (unify-types a b s)))
        (if s
            ((seq) r s)
            (type-error '() a b)))))

  (define (require-type e env t seq)
    (let ((tv (make-tvar 'tv)))
      (bind (infer-expr e env)
            (lambda (e t^)
              (unify t t^
                     (lambda ()
                       (seq e)))))))

  (define (unify-return-type t seq)
    (lambda (r s)
      ((unify r t seq) r s)))
          
  
  ;; you can use this with bind too!
  (define (infer-expr* e* env)
    (if (null? e*)
        (return '() '())
        (let ((e (car e*))
              (e* (cdr e*)))
          (bind
           (infer-expr* e* env)
           (lambda (e* t*)
             (bind (infer-expr e env)
                   (lambda (e t)
                     (return `(,e . ,e*)
                             `(,t . ,t*)))))))))

  (define-syntax do*
    (syntax-rules ()
      ((_ (((x ...) e) ((x* ...) e*) ...) b)
       (bind e (lambda (x ...)
                 (do* (((x* ...) e*) ...) b))))
      ((_ () b) b)))
                   
  
  (define (infer-expr e env)
    (match e
      ((int ,n)
       (return `(int ,n) 'int))
      ((num ,n)
       ;; TODO: We actually need to add a numerically-constrained type
       ;; that is grounded later.
       (return `(int ,n) 'int))
      ((bool ,b)
       (return `(bool ,b) 'bool))
      ((var ,x)
       (let ((t (lookup x env)))
         (return `(var ,t ,x) t)))
      ((return)
       (unify-return-type
        'void
        (lambda () (return `(return) (make-tvar 'bottom)))))
      ((return ,e)
       (bind (infer-expr e env)
             (lambda (e t)
               (unify-return-type
                t
                (lambda ()
                  (return `(return ,e) t))))))
      ((iota ,e)
       (require-type e env 'int
                     (lambda (e^)
                       (let ((r (make-rvar (gensym 'r))))
                         (return `(iota-r ,r ,e^)
                                 `(vec ,r int))))))
      ((if ,test ,c ,a)
       (require-type
        test env 'bool
        (lambda (test)
          (bind (infer-expr c env)
                (lambda (c t)
                  (require-type
                   a env t
                   (lambda (a)
                     (return `(if ,test ,c ,a) t))))))))
      ((let ((,x ,e) ...) ,body)
       (do* (((e t*) (infer-expr* e env))
             ((body t) (infer-expr body (append (map cons x t*) env))))
            (return `(let ((,x ,t* ,e) ...) ,body) t)))
      ))

  (define infer-body infer-expr)

  (define (make-top-level-env decls)
    (map (lambda (d)
           (match d
             ((fn ,name (,[make-tvar -> var*] ...) ,body)
              `(,name . ((,var* ...) -> ,(make-tvar name))))
             ((extern ,name . ,t)
              (cons name t))))
         decls))

  (define (infer-module m)
    (match m
      ((module . ,decls)
       (let ((env (make-top-level-env decls)))
         (infer-decls decls env)))))

  (define (infer-decls decls env)
    (match decls
      (() (values '() '()))
      ((,d . ,d*)
       (let-values (((d* s) (infer-decls d* env)))
         (let-values (((d s) (infer-decl d env s)))
           (values (cons d d*) s))))))

  (define (infer-decl d env s)
    (match d
      ((extern . ,whatever)
       (values `(extern . ,whatever) s))
      ((fn ,name (,var* ...) ,body)
       ;; find the function definition in the environment, bring the
       ;; parameters into scope.
       (match (lookup name env)
         (((,t* ...) -> ,t)
          (let-values (((b t s)
                        ((infer-body body (append (map cons var* t*) env))
                          t s)))
            (values
             `(fn ,name (,var* ...) ((,t* ...) -> ,t) ,b)
             s)))))))

  (define (lookup x e)
    (cdr (assq x e)))
)

