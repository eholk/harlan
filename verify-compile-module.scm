(library
 (verify-compile-module)
 (export verify-compile-module)
 (import (only (chezscheme) format define)
         (rnrs)
         (util match)
         (print-c))
 
 (define verify-compile-module
   (lambda (mod)
     (match mod
       ((,[verify-decl -> decl*] ...)
        decl*))))
 
 (define verify-decl
   (lambda (decl)
     (match decl
       ((func ,type ,name ,args ,[verify-stmt -> stmt*] ...)
        `(func ,type ,name ,args . ,stmt*))
       ((extern ,t ,name ,args)
        `(extern ,t ,name ,args))
       ((gpu-module ,[verify-kernel -> kernel*] ...)
        `(gpu-module ,kernel* ...))
       (,else (error 'verify-decl "Invalid declaration" else)))))

 (define verify-kernel
   ;; TODO
   (lambda (k) k))
 
 (define verify-stmt
   (lambda (stmt)
     (match stmt
       ((apply-kernel ,k ,[verify-expr -> args] ...)
        (guard (symbol? k))
        `(apply-kernel ,k ,args ...))
       ((return ,[verify-expr -> e]) `(return ,e))
       ((do ,[verify-expr -> e*] ...) `(do ,e* ...))       
       ((let ,x ,t ,[verify-expr -> e]) `(let ,x ,t ,e))
       ((while (,relop ,[verify-expr -> e1] ,[verify-expr -> e2])
             ,[verify-stmt -> s] ...)
        `(while (,relop ,e1 ,e2) ,s ...))
       ((for (,x ,[verify-expr -> start] ,[verify-expr -> end])
             ,[verify-stmt -> s] ...)
        `(for (,x ,start ,end) ,s ...))
       ((set! ,x ,[verify-expr -> e]) `(set! ,x ,e))       
       ((print ,[verify-expr -> e]) `(print ,e))
       ((print ,[verify-expr -> e1] ,[verify-expr -> e2]) `(print ,e1 ,e2))       
       ((vec_set_vec ,[verify-expr -> e1]
                     ,[verify-expr -> e2]
                     ,[verify-expr -> e3])
        `(vec_set_vec ,e1 ,e2 ,e3))
       ((block ,[stmt*] ...) `(block ,stmt* ...))
       (,else
        (error 'verify-stmt
               (format "verify-compile-module--unknown statement type: ~s"
                       stmt))))))

(define verify-expr
  (lambda (expr)
    (match expr
      (,n (guard (number? n)) n)
      (,x (guard (symbol? x)) x)
      (,s (guard (string? s)) s)
      ((nanotime) '(nanotime))
      ((,[rator] ,[rand*] ...)
       `(,rator ,rand* ...))
      (,else (error 'verify-expr
                    (format "verify-compile-module--unknown expr type: ~s"
                            expr))))))

)
