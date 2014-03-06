(library
  (harlan front returnify)
  (export returnify)
  (import
   (rnrs)
   (except (elegant-weapons helpers) ident?)
   (harlan helpers))
  
(define-match returnify
  ((module ,[returnify-decl -> decl*] ...)
   `(module . ,decl*)))

(define-match returnify-decl
  ((fn ,name ,args ,[returnify-stmt -> stmt])
   `(fn ,name ,args ,stmt))
  ((define-datatype . ,whatever)
   `(define-datatype . ,whatever))
  ((extern  . ,_)
   `(extern  . ,_)))

(define-match returnify-stmt
  ((var ,x) `(return (var ,x)))
  ((set! ,e1 ,e2) `(set! ,e1 ,e2))
  ((let ((,x ,e) ...) ,[stmt])
   `(let ((,x ,e) ...) ,stmt))
  ((let-region (,r) ,[s]) `(let-region (,r) ,s))
  ((print ,e ...) (make-begin `((print . ,e) (return))))
  ((println ,e ...) (make-begin `((println . ,e) (return))))
  ((assert ,e) (make-begin `((assert ,e) (return))))
  ((while ,e ,[returnify-stmt -> s]) `(while ,e ,s))
  ((return) `(return))
  ((return ,expr) `(return ,expr))
  ((do ,expr) `(return ,expr))
  ((begin ,stmt* ... ,[returnify-stmt -> stmt])
   (make-begin `(,@stmt* ,stmt)))
  ((if ,test ,[conseq]) `(if ,test ,conseq))
  ((if ,test ,[conseq] ,[alt]) `(if ,test ,conseq ,alt)))

)
