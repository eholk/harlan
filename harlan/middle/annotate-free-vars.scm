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
     (if (null? fv*)
         `(fn ,name ,args ,type ,stmt)
         (error 'annotate-decl "unbound variables" fv*))))
  ((extern ,name ,arg-types -> ,type)
   `(extern ,name ,arg-types -> ,type))
  ((global ,name ,type ,[annotate-expr -> e _])
   `(global ,name ,type ,e)))

(define-match annotate-stmt
  ((begin ,[stmt* fv**] ...)
   (values `(begin . ,stmt*) (apply union/var fv**)))
  ((kernel ,dims (((,x* ,t*)
                   (,[annotate-expr -> xs* fv**] ,ts*)
                   ,dim*) ...)
     ,[stmt fv*])
   (let ((fv* (fold-right remove/var fv* x*)))
     (values
       `(kernel ,dims (((,x* ,t*) (,xs* ,ts*) ,dim*) ...)
          (free-vars . ,(map (lambda (p) `(,(caddr p) ,(cadr p))) fv*)) ,stmt)
       (apply union/var fv* fv**))))
  ((return) (values `(return) '()))
  ((return ,[annotate-expr -> e fv*])
   (values `(return ,e) fv*))
  ((if ,[annotate-expr -> test tfv*] ,[conseq cfv*])
   (values `(if ,test ,conseq) (union/var tfv* cfv*)))
  ((if ,[annotate-expr -> test tfv*] ,[conseq cfv*] ,[alt afv*])
   (values `(if ,test ,conseq ,alt)
     (union/var tfv* cfv* afv*)))
  ((do ,[annotate-expr -> e fv*])
   (values `(do ,e) fv*))
  ((let ((,x* ,t* ,[annotate-expr -> e* fv**]) ...) ,[stmt fv*])
   (let ((fv* (fold-right remove/var fv* x*)))
     (values `(let ((,x* ,t* ,e*) ...) ,stmt)
       (apply union/var fv* fv**))))
  ((while ,[annotate-expr -> e efv*] ,[stmt sfv*])
   (values `(while ,e ,stmt) (union/var efv* sfv*)))
  ((for (,x ,[annotate-expr -> start sfv*]
          ,[annotate-expr -> end efv*])
     ,[stmt bfv*])
   (let ((bfv* (remove/var x bfv*)))
     (values `(for (,x ,start ,end) ,stmt)
       (union/var bfv* sfv* efv*))))
  ((set! ,[annotate-expr -> x xfv*] ,[annotate-expr -> e efv*])
   (values `(set! ,x ,e) (union/var xfv* efv*)))
  ((print ,[annotate-expr -> e fv*] ...)
   (values `(print . ,e) (apply union/var fv*)))
  ((assert ,[annotate-expr -> e fv*])
   (values `(assert ,e) fv*)))

;; Do any of the exprs change? If not, why are we returning them
;; unchanged...
(define-match annotate-expr
  ((void) (values `(void) '()))
  ((,t ,n) (guard (scalar-type? t)) (values `(,t ,n) '()))
  ((var ,t ,x) (values `(var ,t ,x) `((var ,t ,x))))
  ((c-expr ,t ,x) (values `(c-expr ,t ,x) `()))
  ((alloc ,[region rfv*] ,[size sfv*])
   (values `(alloc ,region ,size) (union/var rfv* sfv*)))
  ((region-ref ,t ,[region rfv*] ,[ptr pfv*])
   (values `(region-ref ,t ,region ,ptr)
     (union/var rfv* pfv*)))
  ((cast ,t ,[e fv*]) (values `(cast ,t ,e) fv*))
  ((call ,[rator fv*] ,[rand* fv**] ...)
   (values
     `(call ,rator . ,rand*)
     (apply union/var fv* fv**)))
  ((let ((,x* ,t* ,[e* fv**]) ...) ,[expr fv*])
   (let ((fv* (fold-right remove/var fv* x*)))
     (values `(let ((,x* ,t* ,e*) ...) ,expr)
       (apply union/var fv* fv**))))
  ((if ,[test tfv*] ,[conseq cfv*] ,[alt afv*])
   (values `(if ,test ,conseq ,alt)
     (union/var tfv* cfv* afv*)))
  ((sizeof ,t) (values `(sizeof ,t) '()))
  ((addressof ,[e fv*]) (values `(addressof ,e) fv*))
  ((deref ,[e fv*]) (values `(deref ,e) fv*))
  ((,op ,[lhs lfv*] ,[rhs rfv*])
   (guard (or (binop? op) (relop? op)))
   (values `(,op ,lhs ,rhs) (union/var lfv* rfv*)))
  ((vector-ref ,t ,[v vfv*] ,[i ifv*])
   (values `(vector-ref ,t ,v ,i) (union/var vfv* ifv*))))

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
