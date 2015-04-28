(library
    (harlan middle specialize-string-equality)
  (export specialize-string-equality)
  (import (rnrs)
          (nanopass)
          (harlan middle languages M9))

  (define (type-of e)
    (nanopass-case
     (M9.3 Expr) e
     ((int ,i) 'int)
     ((bool ,b) 'bool)
     ((var ,t ,x) t)
     ((call (var (fn (,t* ...) ,-> ,t) ,x) ,e* ...) t)
     ((,op ,e1 ,e2) (guard (eq? '= op)) 'bool)
     ((,op ,e1 ,e2) (type-of e1))
     ((deref ,[t])
      (nanopass-case
       (M9.3 Rho-Type) t
       ((ptr ,t^) t^)
       (else (error 'type-of "unexpected type in deref"
                    (unparse-M9.3 t)))))
     ((region-ref ,t ,e1 ,e2) t)
     ((vector-ref ,t ,e1 ,e2) t)
     (else (error 'type-of "I don't know how to find this type"
                  (unparse-M9.3 e)))))
  
  (define-pass specialize-string-equality : M9.3 (m) -> M9.3 ()
    (Expr
     : Expr (e) -> Expr ()
     [(,op ,e1 ,[e2])
      (guard (and (eq? op '=) (eq? 'str (type-of e1))))
      `(call (c-expr (fn (str str) -> bool) hstrcmp) ,(Expr e1) ,e2)])))
