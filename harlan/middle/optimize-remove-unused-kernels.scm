(library
    (harlan middle optimize-remove-unused-kernels)
  (export optimize-remove-unused-kernels)

  (import (nanopass)
          (rnrs)
          (elegant-weapons sets)
          (only (harlan middle languages M7) M7 unparse-M7))

  ;; This is actually just a remove-unused-bindings pass, but here it
  ;; happens early enough that we can cut out whole kernels.
  (define-pass optimize-remove-unused-kernels
    : M7 (m) -> M7 ()
    (definitions
      (define x* (set 'bogus))

      (define (bound-variables lbind*)
        (map (lambda (lbind)
               (nanopass-case
                (M7 LetBinding) lbind
                ((,x ,t ,e) x)))
             lbind*))
      
      ;; Takes a list of let bindings and a list of variables
      ;; referenced in the body. Returns a list that includes only
      ;; those referenced variables.
      (define (filter-bindings lbind* ref-x)
        (fold-left
         (lambda (ret-bind lbind)
           (with-output-language
            (M7 LetBinding)
            (nanopass-case
             (M7 LetBinding) lbind
             ((,x ,t ,e)
              (if (member x ref-x)
                  (cons `(,x ,t ,e) ret-bind)
                  (begin
                    ;;(display (list "removing binding" x))
                    ret-bind))))))
         '()
         lbind*)))

    (LetBinding
     : LetBinding (lbind) -> LetBinding (x*)
     ((,x ,t ,[e x*]) (values `(,x ,t ,e) x*)))

    (Expr
     : Expr (e) -> Expr (x*)
     ((var ,t ,x) (values `(var ,t ,x) (set x)))
     ((let (,[lbind x*] ...) ,[e x])
      ;;(display x)
      (let ((lbind (filter-bindings lbind x)))
        (values (if (null? lbind)
                    e
                    `(let (,lbind ...) ,e))
                (apply union (difference x (bound-variables lbind)) x*))))

     ((do ,[e x]) (values `(do ,e) x))
     ((length ,[e x])
      (values `(length ,e) x))
     ((vector ,t ,r ,[e* x*] ...)
      (values `(vector ,t ,r ,e* ...) (apply union x*)))
     ((assert ,[e x])
      (values `(assert ,e) x))
     ((set! ,[e1 x1] ,[e2 x2])
      (values `(set! ,e1 ,e2) (union x1 x2)))
     ;; one-armed if expressions are an abomination
     ((if ,[e1 x1] ,[e2 x2])
      (values `(if ,e1 ,e2) (union x1 x2)))
     ((if ,[e1 x1] ,[e2 x2] ,[e3 x3])
      (values `(if ,e1 ,e2 ,e3) (union x1 x2 x3)))
     ((kernel ,t ,r (,[e* x*] ...) (((,x0 ,t0) (,[e1* x1*] ,t1) ,i) ...)
              ,[e2 x2])
      (values `(kernel ,t ,r (,e* ...) (((,x0 ,t0) (,e1* ,t1) ,i) ...) ,e2)
              (apply union (difference x2 x0) (append x* x1*))))
     ((while ,[e x1] ,[e2 x2])
      (values `(while ,e ,e2) (union x1 x2)))
     ((for (,x ,[e1 x1] ,[e2 x2] ,[e3 x3]) ,[e x4])
      (values `(for (,x ,e1 ,e2 ,e3) ,e)
              (union x1 x2 x3 (difference x4 (set x)))))
     ((vector-ref ,t ,[e1 x1] ,[e2 x2])
      (values `(vector-ref ,t ,e1 ,e2) (union x1 x2)))
     ((unsafe-vector-ref ,t ,[e0 x0] ,[e1 x1])
      (values `(unsafe-vector-ref ,t ,e0 ,e1)
              (union x0 x1)))
     ((unsafe-vec-ptr ,t ,[e x])
      (values `(unsafe-vec-ptr ,t ,e) x))
     ((field ,[e x*] ,x) (values `(field ,e ,x) x*))
     ((call ,[e x] ,[e* x*] ...)
      (values `(call ,e ,e* ...) (apply union x x*)))
     ((print ,[e x]) (values `(print ,e) x))
     ((print ,[e1 x1] ,[e2 x2]) (values `(print ,e1 ,e2) (union x1 x2)))
     ((,op ,[e1 x1] ,[e2 x2])
      (values `(,op ,e1 ,e2) (union x1 x2)))
     ((begin ,[e x] ,[e* x*] ...)
      (values `(begin ,e ,e* ...) (apply union x x*))))
    
    (Body
     : Body (body) -> Body (x*)
     ((let (,[lbind x*] ...) ,[body x])
      ;;(display x)
      (let ((lbind (filter-bindings lbind x)))
        (values (if (null? lbind)
                    body
                    `(let (,lbind ...) ,body))
                (apply union (difference x (bound-variables lbind)) x*))))

     ((if ,[e x] ,[body1 x1] ,[body2 x2])
      (values `(if ,e ,body1 ,body2)
              (union x x1 x2)))
     ((if ,[e x] ,[body1 x1])
      (values `(if ,e ,body1)
              (union x x1)))
     ((assert ,[e x])
      (values `(assert ,e) x))
     ((set! ,[e1 x1] ,[e2 x2])
      (values `(set! ,e1 ,e2) (union x1 x2)))
     ((while ,[e x1] ,[body x2])
      (values `(while ,e ,body) (union x1 x2)))
     ((print ,[e x]) (values `(print ,e) x))
     ((print ,[e1 x1] ,[e2 x2]) (values `(print ,e1 ,e2) (union x1 x2)))
     ((return ,[e x])
      (values `(return ,e) x))
     ((begin ,[body* x*] ... ,[body x])
      (values `(begin ,body* ... ,body)
              (apply union x x*)))
     ((do ,[e x]) (values `(do ,e) x))
     ((let-region (,r ...) ,[body x*])
      (values `(let-region (,r ...) ,body) x*)))
    
    (Decl
     : Decl (decl) -> Decl ()
     ((fn ,name (,x ...) ,t ,[body _])
      `(fn ,name (,x ...) ,t ,body)))))
