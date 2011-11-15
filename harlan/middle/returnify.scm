(library
  (harlan middle returnify)
  (export returnify)
  (import (rnrs) (elegant-weapons helpers))
  
(define-match returnify
  ((module ,[returnify-decl -> decl*] ...)
   `(module . ,decl*)))

(define-match returnify-decl
  ((fn ,name ,args ,type ,[returnify-stmt -> stmt])
   `(fn ,name ,args ,type ,stmt))
  ((extern ,name ,args -> ,rtype)
   `(extern ,name ,args -> ,rtype)))

(define-match returnify-stmt
  (,n (guard (number? n)) `(return ,n))
  ((var ,x) `(return (var ,x)))       
  ((vector . ,e*) `(return (vector . ,e*)))
  ((let ((,x ,e) ...) ,[stmt])
   `(let ((,x ,e) ...) ,stmt))
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