(library
    (harlan middle remove-complex-kernel-args)
  (export remove-complex-kernel-args)
  (import
   (rnrs)
   (nanopass)
   (only (elegant-weapons helpers) gensym)
   (only (elegant-weapons lists) fold-right-values)
   (harlan middle languages M7))

  (define-pass remove-complex-kernel-args : M7.0 (m) -> M7.0 ()
    (definitions
      (define (complex? t)
        (nanopass-case
         (M7.0 Rho-Type) t
         ((adt ,x) #t)
         ((adt ,x ,r) #t)
         (else #f))))
    
    (SplitFreeVars
     : FreeVars (fv) -> FreeVars (x^ t^ x* t*)

     ((free-vars (,x ,[t]) ...)
      (let-values (((x* t* x^ t^)
                    (fold-right-values
                     ((x* '()) (t* '())
                      (x^ '()) (t^ '())
                      <- (x x) (t t))
                     (if (complex? t)
                         (values (cons x x*) (cons t t*) x^ t^)
                         (values x* t* (cons x x^) (cons t t^))))))
        (values `(free-vars (,x^ ,t^) ...) x^ t^ x* t*))))
      
    (KernelStmt
     : Stmt (e) -> Stmt ()
     
     ((kernel (vec ,r ,[t]) (,[e*] ...)
              ,danger
              ,[SplitFreeVars : fv x^ t^ x* t*]
              ,[stmt])
      (let ((x*^ (map gensym x*))
            (r* (map (lambda (_) r) x*)))
        (if (null? x*^)
            `(kernel (vec ,r ,t) (,e* ...) ,danger ,fv ,stmt)
            `(let ((,x*^ (box ,r* ,t*) (box ,r* ,t* (var ,t* ,x*))) ...)
               (kernel (vec ,r ,t) (,e* ...) ,danger
                       (free-vars (,x^ ,t^) ...
                                  (,x*^ ,(map (lambda (t)
                                                `(box ,r ,t))
                                              t*)) ...)
                       (let ((,x* ,t* (unbox ,t* ,r* (var (box ,r* ,t*) ,x*^)))
                             ...)
                         ,stmt))))))
     
     ((kernel ,t (,e* ...) ,danger ,fv ,stmt)
      (error 'Kernel "Unmatched kernel" (unparse-M7.0 e))))))
