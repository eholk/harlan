(library
  (harlan middle returnify)
 (export returnify)
 (import
   (rnrs)
   (only (chezscheme) format)
   (elegant-weapons helpers)
   (elegant-weapons match)
   (util verify-grammar))

 (define-match returnify
   ((module ,[returnify-decl -> fn*] ...)
    `(module . ,fn*)))

 (define-match returnify-decl
   ((fn ,name ,args ,stmt* ... ,[returnify-stmt -> stmt])
    `(fn ,name ,args ,@stmt* . ,stmt))
   ((extern ,name ,args -> ,rtype)
    `(extern ,name ,args -> ,rtype)))
     
 (define-match returnify-stmt
   (,n (guard (number? n)) `((return ,n)))
   ((var ,x) `((return (var ,x))))       
   ((vector . ,e*) `((return (vector . ,e*))))
   ((print ,expr)
    `((print ,expr)))
   ((print ,e1 ,e2)
    `((print ,e1 ,e2)))
   ((return ,expr)
    `((return ,expr)))
   ((begin ,stmt* ... ,stmt)
    `(,(make-begin stmt*) . ,(returnify-stmt stmt)))
   ((if ,test ,conseq)
    `((if ,test ,conseq)))
   ((if ,test ,conseq ,alt)
    `((if ,test ,conseq ,alt))))

)