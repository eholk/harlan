(library
 (returnify-kernels)
 (export returnify-kernels)
 (import (rnrs)
         (match))
 
 (define gensym
   (let ((c 0))
     (lambda (x)
       (set! c (+ 1 c))
       (string->symbol
        (string-append (symbol->string x) "_" (number->string c))))))
 
 (define returnify-kernels
   (lambda (mod)
     (match mod
       ((module ,fn* ...)
        `(module . ,(map returnify-kernel-fn fn*))))))

  (define (returnify-kernel-fn fn)
   (match fn
     ((fn ,name ,args ,type ,stmt* ...)
      `(fn ,name ,args ,type . ,(returnify-kernel-stmt* stmt*)))))

 (define returnify-kernel-stmt*
   (lambda (stmt*)
     (cond
       [(null? stmt*) '()]
       [else
        (append (returnify-kernel-stmt (car stmt*))
                (returnify-kernel-stmt* (cdr stmt*)))])))

 (define returnify-kernel-stmt
   (lambda (stmt)
     (match stmt
       ((print ,expr) `((print ,expr)))
       ((print ,e1 ,e2) `((print ,e1 ,e2)))       
       ((assert ,expr) `((assert ,expr)))
       ((set! ,x ,e) `((set! ,x ,e)))
       ((vector-set! ,t ,x ,e1 ,e2) `((vector-set! ,t ,x ,e1 ,e2)))
       ((return ,expr) `((return ,expr)))
       ((for (,x ,e1 ,e2) ,body* ...)
        `((for (,x ,e1 ,e2) . ,(returnify-kernel-stmt* body*))))
       ((kernel ,t2 ,arg* . ,body*)
       ;; allowing kernels as both statements and expressions
        `((kernel ,t2 ,arg* . ,(returnify-kernel-stmt* body*))))
       ((let ,id ,type ,expr)
        (returnify-kernel-let `(let ,id ,type ,expr)))
       (,else (error 'returnify-kernel-stmt  "unknown statement:" else)))))

 (define type-dim
   (lambda (t)
     (match t
       ((vector ,[t]) (+ 1 t))
       (,x 0))))
 
 (define returnify-kernel-let
   (lambda (e)
     (match e
       ((let ,id ,t1 (kernel void ,arg* . ,body*))
        ;; TODO: we still need to traverse the body*
        `(let ,id ,t1 (kernel void ,arg* . ,body*)))
       ((let ,id ,t1 (kernel ,t2 ,arg* . ,body*))
        (if (= 1 (type-dim t1))
            (match arg*
              ((((,x* ,tx*) (,xe* ,xet*)) ...)
               (let ((g (gensym 'retval)))
                 (let ((body*
                        (let loop ((body* body*))
                          (match body*
                            ((,body) `((set! (var ,(cadr t2) ,g) ,body)))
                            ((,body . ,rest)
                             (let ((body^ (returnify-kernel-stmt body)))
                               (append body^ (loop rest))))))))
                   `((let ,id ,t1 (length ,(car xe*)))
                     (kernel ,t2 (((,g ,(cadr t2)) ((var ,t1 ,id) ,t1))
                                  . ,arg*)
                             . ,body*))))))
            (error 'returnify-kernel-let
                   "Only 1-dimensional return values are allowed.")))
       ((let ,id ,type ,expr) `((let ,id ,type ,expr)))
       (,else (error 'returnify-kernel-let  "unknown statement" else)))))

)
