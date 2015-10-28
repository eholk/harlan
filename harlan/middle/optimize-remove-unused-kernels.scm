(library
    (harlan middle optimize-remove-unused-kernels)
  (export optimize-remove-unused-kernels)

  (import (nanopass)
          (rnrs)
          (elegant-weapons sets)
          (only (harlan middle languages M7) M7 unparse-M7))

  ;; This is actually just a remove-unused-bindings pass, but here it
  ;; happens early enough that we can cut out whole kernels.
  (trace-define-pass optimize-remove-unused-kernels
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

     ((vector-ref ,t ,[e1 x1] ,[e2 x2])
      (values `(vector-ref ,t ,e1 ,e2) (union x1 x2)))
     ((field ,[e x*] ,x) (values `(field ,e ,x) x*))
     ((call ,[e x] ,[e* x*] ...)
      (values `(call ,e ,e* ...) (apply union x x*)))
     ((print ,[e x]) (values `(print ,e) x))
     ((print ,[e1 x1] ,[e2 x2]) (values `(print ,e1 ,e2) (union x1 x2)))
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
