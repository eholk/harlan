(library (harlan middle insert-transactions)
  (export insert-transactions)
  (import
   (rnrs)
   (nanopass)
   (only (harlan middle languages M7)
         M7.0.0
         M7.0.1))

  (define-pass insert-transactions : M7.0.0 (m) -> M7.0.1 ()
    (Body
     : Body (body) -> Body ()
     ((do (kernel ,[t] ,r (,[e*] ...) (((,x0 ,[t0]) (,[e1] ,[t1]) ,i*) ...)
                  ,[e]))
      `(transaction
        (,r)
        (do (kernel ,t ,r (,e* ...) (((,x0 ,t0) (,e1 ,t1) ,i*) ...) ,e)))))))
