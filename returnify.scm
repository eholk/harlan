(library
 (returnify)
 (export returnify verify-returnify)
 (import
   (chezscheme)
   (util match)
   (verify-grammar))

 (generate-verify returnify
   (Module (module Decl))
   (Decl
     (fn Var (Var *) Stmt * Ret-Stmt)
     (extern Var Var -> Type))
   (Stmt
     integer
     (var Var)
     (vector Expr *)
     (print Expr)
     (print Expr Expr)
     Ret-Stmt
     Expr)
   (Ret-Stmt
     (return Expr))
   (Var symbol)
   (Type wildcard)
   (Expr wildcard))

 (define returnify
   (lambda (mod)
     (match mod
       ((module ,[returnify-decl -> fn*] ...)
        `(module ,fn* ...)))))

 (define (returnify-decl decl)
   (match decl
     ((fn ,name ,args ,stmt* ... ,stmt)
      `(fn ,name ,args ,stmt* ... ,(returnify-stmt stmt)))
     ((extern ,name ,args -> ,rtype)
      `(extern ,name ,args -> ,rtype))
     (,else
      (error 'returnify-decl "Invalid declaration" else))))
     
 (define returnify-stmt
   (lambda (stmt)     
     (match stmt
       (,n (guard (number? n)) `(return ,n))
;       (,x (guard (symbol? x)) `(return ,x))
       ((var ,x) `(return (var ,x)))       
       ((vector . ,e*) `(return (vector . ,e*)))
       ((print ,expr)
        `(print ,expr))
       ((print ,e1 ,e2)
        `(print ,e1 ,e2))
       ((return ,expr)
        `(return ,expr))
       ;; 'let' can't appear in tail position
       (,else (error 'returnify-stmt
                (format "unknown statement: ~a" else))))))

)