(library
  (harlan middle make-kernel-dimensions-explicit)
  (export make-kernel-dimensions-explicit)
  (import
   (rnrs)
   (harlan helpers)
   (harlan middle languages M5)
   (harlan middle languages M6)
   (nanopass)
   (except (elegant-weapons helpers) ident?))

  (define-pass make-kernel-dimensions-explicit
    : M5 (m) -> M6 ()

    (Expr
     : Expr (e) -> Expr ()

     ((kernel ,[t] ,r (((,x* ,[t0*]) (,[e*] ,[t1*])) ...) ,[e])
      `(kernel ,t ,r 1 (((,x* ,t0*) (,e* ,t1*) ,(map (lambda (_) 0) x*)) ...)
               ,e))
     ((iota-r ,r ,[e])
      `(kernel (vec ,r int) ,r 1 (,e) ()
               (call (c-expr (fn (int) -> int) get_global_id)
                     (int 0))))))
  ;; end library
  )
