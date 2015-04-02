(library
  (harlan middle make-work-size-explicit)
  (export make-work-size-explicit)
  (import
   (rnrs)
   (harlan helpers)
   (nanopass)
   (harlan middle languages M6)
   (harlan middle languages M7)
   (except (elegant-weapons helpers) ident?))

  (define-pass make-work-size-explicit
    : M6 (m) -> M7 ()

    (Expr
     : Expr (e) -> Expr ()

     ((kernel ,[t] ,r ,i (,[e*] ...) (((,x0 ,[t0]) (,[e1] ,[t1]) ,i*) ...) ,[e])
      `(kernel ,t ,r (,e* ...) (((,x0 ,t0) (,e1 ,t1) ,i*) ...) ,e))
     ((kernel ,[t] ,r ,i (((,x0 ,[t0]) (,[e1] ,[t1]) ,i*) ...) ,[e])
      (let ((e1^ (car e1))
            (t1^ (car t1))
            (x (gensym 'ktemp)))
        `(let ((,x ,t1^ ,e1^))
           (kernel ,t ,r
                   ((length (var ,t1^ ,x)))
                   (((,x0 ,t0) (,(cons `(var ,t1^ ,x) (cdr e1)) ,t1) ,i*) ...)
                   ,e)))))))
