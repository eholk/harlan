(library
 (verify-compile-module)
 (export verify-compile-module)
 (import (only (chezscheme) format define)
         (rnrs)
         (match)
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
        `(func ,type ,name ,args . ,stmt*)))))

 (define verify-stmt
   (lambda (stmt)
     (match stmt
       ((kernel (((,x* ,t*) (,xs* ,ts*)) ...) ,[verify-stmt -> stmt*] ...)
        `(kernel (((,x* ,t*) (,xs* ,ts*)) ...) . ,stmt*))
       ((return ,[verify-expr -> e]) `(return ,e))
       ((do ,[verify-stmt -> s]) `(do ,s))
       ((assert ,[verify-expr -> e]) `(assert ,e))       
       ((let ,x ,t ,[verify-expr -> e]) `(let ,x ,t ,e))
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
       (,else
        (error 'verify-stmt
               (format "verify-compile-module--unknown statement type: ~s"
                       stmt))))))

(define verify-expr
  (lambda (expr)
    (match expr
      (,n (guard (number? n)) n)
      (,x (guard (symbol? x)) x)
      ((,+ ,[verify-expr -> e1] ,[verify-expr -> e2])
       (guard (binop? +))
       `(,+ ,e1 ,e2))
      ((deref ,[verify-expr -> e]) `(deref ,e))
      ((hvec_byte_size ,[verify-expr -> e])
       `(hvec_byte_size ,e))
      ((vec_ref_1d ,[verify-expr -> ve] ,[verify-expr -> ie])
       `(vec_ref_1d ,ve ,ie))
      ((vec_ref_nd ,[verify-expr -> ve] ,[verify-expr -> ie])
       `(vec_ref_nd ,ve ,ie))
      ((hvec_length ,[e] ,[sz]) `(hvec_length ,e ,sz))
      ((cast ,t ,[verify-expr -> e]) `(cast ,t ,e))
      ((addressof ,[verify-expr -> e]) `(addressof ,e))
      ((sizeof ,t) `(sizeof ,t))
      ((malloc ,[verify-expr -> s]) `(malloc ,s))
      ((hvec_ref ,[v] ,[i] ,[sz])
       `(hvec_ref ,v ,i ,sz))
      ((hmk_vec ,[verify-expr -> e1] ,[verify-expr -> e2] ,[verify-expr -> e3])
       `(hmk_vec ,e1 ,e2 ,e3))
      ((nanotime) '(nanotime))
      (,else (error 'verify-expr
                    (format "verify-compile-module--unknown expr type: ~s"
                            expr))))))

)
