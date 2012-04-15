(library
  (harlan middle annotate-free-vars)
  (export annotate-free-vars)
  (import
   (rnrs)
   (harlan helpers)
   (elegant-weapons helpers))

(define-match annotate-free-vars
  ((module . ,[annotate-decl* -> decl*])
   `(module . ,decl*)))

(define-match annotate-decl*
  (((,tag* ,name* . ,rest*) ...)
   (map (annotate-decl name*)
        `((,tag* ,name* . ,rest*) ...))))

(define-match (annotate-decl globals)
  ((fn ,name ,args ,type ,[annotate-stmt -> stmt fv*])
   (let ((fv* (fold-right remove/var fv* (append globals args))))
     (unless (null? fv*)
       (error 'annotate-decl "unbound variables" fv*))
     `(fn ,name ,args ,type ,stmt)))
  ((extern ,name ,arg-types -> ,type)
   `(extern ,name ,arg-types -> ,type))
  ((global ,name ,type ,[expr-fv -> e _])
   `(global ,name ,type ,e)))

(define-match annotate-stmt
  ((begin ,[stmt* fv**] ...)
   (values `(begin . ,stmt*)
     (apply union/var fv**)))
  ((kernel ,type ,dims
     (((,x* ,t*) (,xs* ,ts*) ,dim*) ...)
     ,[stmt fv*])
   (let ((fv* (fold-right remove/var fv* x*)))
     (values
       `(kernel ,type ,dims (((,x* ,t*) (,xs* ,ts*) ,dim*) ...)
          (free-vars . ,(map (lambda (p) `(,(caddr p) ,(cadr p))) fv*))
          ,stmt)
       (apply union/var fv* (map expr-fv xs*)))))
  ((return) (values `(return) `()))
  ((return ,e)
   (values `(return ,e) (expr-fv e)))
  ((if ,t ,[c cfv*])
   (values `(if ,t ,c)
     (union/var (expr-fv t) cfv*)))
  ((if ,t ,[c cfv*] ,[a afv*])
   (values `(if ,t ,c ,a)
     (union/var (expr-fv t) cfv* afv*)))
  ((do ,e)
   (values `(do ,e) (expr-fv e)))
  ((let ((,x* ,t* ,e*) ...) ,[stmt fv*])
   (let ((fv* (fold-right remove/var fv* x*)))
     (values `(let ((,x* ,t* ,e*) ...) ,stmt)
       (apply union/var fv*
         (map expr-fv e*)))))
  ((while ,e ,[stmt sfv*])
   (values `(while ,e ,stmt)
     (union/var (expr-fv e) sfv*)))
  ((for (,x ,start ,end ,step) ,[stmt fv*])
   (let ((fv* (remove/var x fv*)))
     (values `(for (,x ,start ,end ,step) ,stmt)
       (union/var fv* (expr-fv start) (expr-fv end)
         (expr-fv step)))))
  ((vector-set! ,t ,e* ...)
   (values `(vector-set! ,t . ,e*)
     (apply union/var (map expr-fv e*))))
  ((set! ,x ,e)
   (values `(set! ,x ,e)
     (union/var (expr-fv x)
       (expr-fv e))))
  ((print ,e ...)
   (values `(print . ,e)
     (apply union/var (map expr-fv e))))
  ((assert ,e)
   (values `(assert ,e) (expr-fv e))))

(define-match expr-fv
  ((,t ,n) (guard (scalar-type? t)) `())
  ((var ,t ,x) `((var ,t ,x)))
  ((int->float ,[fv*]) fv*)
  ((length ,[fv*]) fv*)
  ((addressof ,[fv*]) fv*)
  ((deref ,[fv*]) fv*)
  ((c-expr ,t ,x) `())
  ((call ,[fv*] ,[fv**] ...)
   (apply union/var fv* fv**))
  ((vector ,t ,[fv**] ...)
   (apply union/var fv**))
  ((reduce ,t ,op ,[fv*]) fv*)
  ((make-vector ,t ,[fv*]) fv*)
  ((iota ,[fv*]) fv*)
  ((let ((,x* ,t* ,[fv**]) ...) ,[fv*])
   (let ((fv* (fold-right remove/var fv* x*)))
     (apply union/var fv* fv**)))
  ((if ,[tfv*] ,[cfv*] ,[afv*])
   (union/var tfv* cfv* afv*))
  ((sizeof ,t) `())
  ((,op ,[lfv*] ,[rfv*])
   (guard (or (binop? op) (relop? op)))
   (union/var lfv* rfv*))
  ((vector-ref ,t ,[vfv*] ,[ifv*])
   (union/var vfv* ifv*)))

(define (member/var x s)
  (cond
    ((null? s) #f)
    ((eq? (caddar s) x) #t)
    (else (member/var x (cdr s)))))

(define (remove/var x s)
  (cond
    ((null? s) `())
    ((eq? (caddar s) x) (cdr s))
    (else (cons (car s) (remove/var x (cdr s))))))

(define (difference/var s r)
  (cond
    ((null? s) `())
    ((member/var (car s) r)
     (difference/var (cdr s) r))
    (else (cons (car s)
            (difference/var (cdr s) r)))))

(define (set-add/var s x)
  (if (member/var (caddr x) s) s (cons x s)))

(define (union/var . s*)
  (cond
    ((null? s*) `())
    (else
      (let ((s (car s*)) (s* (cdr s*)))
        (fold-left
          (lambda (s1 s2) (fold-left set-add/var s1 s2))
          s s*)))))

;; end library
)
