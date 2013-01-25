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
    (error 'typecheck "unimplemented"))

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
      ;; TODO: watch for cycles
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
  (define (infer-expr e env s)
    (match e
      ((int ,n) (guard (integer? n))
       (values `(int ,n) 'int s))
      ((bool ,b)
       (values `(bool ,b) 'bool s))
      ((iota ,e)
       (let-values (((e^ t s^) (infer-expr e env s)))
         (let ((s^^ (unify-types t 'int s^)))
           (if s^^
               (let ((r (make-rvar (gensym 'r))))
                 (values `(iota-r ,r ,e^)
                         `(vector ,r int)
                         s^^))
               (type-error `(iota ,e) 'int t)))))
      ))
       
)

