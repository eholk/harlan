(library
 (returnify)
 (export returnify)
 (import (chezscheme)
         (match))

 (define returnify
   (lambda (mod)
     (match mod
       ((module ,fn* ...)
        `(module . ,(map returnify-fn fn*))))))

  (define (returnify-fn fn)
   (match fn
     ((fn ,name ,args ,stmt* ...)
      `(fn ,name ,args . ,(returnify-stmt* stmt*)))))

 (define returnify-stmt*
   (lambda (stmt*)
     (cond
       [(null? stmt*) '()]
       [(null? (cdr stmt*))
        (cons (returnify-stmt (car stmt*)) '())]
       [else
        (cons (car stmt*) (returnify-stmt* (cdr stmt*)))])))

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
       (,else (error 'returnify-stmt (format "unknown statement: ~a" else))))))

)