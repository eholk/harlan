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
  
  ;; returns (e^ t s^)
  (define (infer-expr e ret env s)
    (match e
      ((int ,n)
       (values `(int ,n) 'int s))
      ((num ,n)
       ;; TODO: We actually need to add a numerically-constrained type
       ;; that is grounded later.
       (values `(int ,n) 'int s))
      ((bool ,b)
       (values `(bool ,b) 'bool s))
      ((var ,x)
       (let ((t (lookup x env)))
         (values `(var ,t ,x) t s)))
      ((return)
       (values `(return) 'void s))
      ((return ,e)
       (let-values (((e^ t s)
                     (infer-expr e ret env s)))
         (let ((s (unify-types t ret s)))
           (if s
               (values `(return ,e^) t s)
               (type-error `(return ,e) ret t)))))
      ((iota ,e)
       (let-values (((e^ t s^) (infer-expr e ret env s)))
         (let ((s^^ (unify-types t 'int s^)))
           (if s^^
               (let ((r (make-rvar (gensym 'r))))
                 (values `(iota-r ,r ,e^)
                         `(vector ,r int)
                         s^^))
               (type-error `(iota ,e) 'int t)))))
      ((if ,test ,c ,a)
       (let-values (((test tt s)
                     (infer-expr test ret env s)))
         (let ((s (unify-types tt 'bool s)))
           (if s
               (let-values
                   (((c tc s)
                     (infer-expr c ret env s)))
                 (let-values (((a ta s)
                               (infer-expr a ret env s)))
                   (let ((s (unify-types tc ta s)))
                     (if s
                         (values `(if ,test ,c ,a)
                                 tc
                                 s)
                         (type-error `(if ,test ,c ,a)
                                     tc
                                     ta)))))
               (type-error `(if ,test ,c ,a)
                           tt
                           'bool)))))
      ((let ((,x ,e) ...) ,body)
       (let-values
           (((x e t* s)
             (let loop
                 ((x x)
                  (e e))
               (match `(,x ,e)
                 ((() ()) (values '() '() '() s))
                 (((,x . ,x*) (,e . ,e*))
                  (let-values (((x* e* t* s) (loop x* e*)))
                    (let-values
                        (((e t s) (infer-expr e ret env s)))
                      (values (cons x x*)
                              (cons e e*)
                              (cons t t*)
                              s))))))))
         (let-values (((b t s)
                       (infer-body body ret
                                   (append (map cons x t*) env)
                                   s)))
           (values `(let ((,x ,t* ,e) ...) ,b) t s))))
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
                        (infer-body body t (append (map cons var* t*) env) s)))
            (values
             `(fn ,name (,var* ...) ((,t* ...) -> ,t) ,b)
             s)))))))

  (define (lookup x e)
    (cdr (assq x e)))
)

