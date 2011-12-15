(library
  (harlan front returnify)
  (export returnify)
  (import (rnrs) (elegant-weapons helpers))
  
(define-match returnify
  ((module ,[returnify-decl -> decl*] ...)
   `(module . ,decl*)))

(define-match returnify-decl
  ((fn ,name ,args ,[returnify-stmt -> stmt])
   `(fn ,name ,args ,stmt))
  ((extern ,name ,args -> ,rtype)
   `(extern ,name ,args -> ,rtype)))

(define-match returnify-stmt
  ((var ,x) `(return (var ,x)))       
  ((let ((,x ,e) ...) ,[stmt])
   `(let ((,x ,e) ...) ,stmt))
  ((print ,e) `(begin (print ,e) (return)))
  ((assert ,e) `(begin (assert ,e) (return)))
  ((return) `(return))
  ((return ,expr)
   `(return ,expr))
  ((do ,expr) `(return ,expr))
  ((begin ,stmt* ... ,[returnify-stmt -> stmt])
   `(begin ,@stmt* ,stmt))
  ((if ,test ,[conseq])
   `(if ,test ,conseq))
  ((if ,test ,[conseq] ,[alt])
   `(if ,test ,conseq ,alt)))

)