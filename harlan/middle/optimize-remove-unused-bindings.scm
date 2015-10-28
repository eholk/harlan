(library
    (harlan middle optimize-remove-unused-bindings)
  (export optimize-remove-unused-bindings)
  (import
   (rnrs)
   (only (harlan middle languages M7)
         M7.1 unparse-M7.1)
   (elegant-weapons sets)
   (nanopass))

  (define-pass optimize-remove-unused-bindings : M7.1 (m) -> M7.1 ()
    (definitions
      (define x* (set 'bogus))

      (define (bound-variables lbind*)
        (map (lambda (lbind)
               (nanopass-case
                (M7.1 LetBinding) lbind
                ((,x ,t) x)
                ((,x ,t ,e) x)))
             lbind*))
      
      ;; Takes a list of let bindings and a list of variables
      ;; referenced in the body. Returns a list that includes only
      ;; those referenced variables.
      (define (filter-bindings lbind* ref-x)
        (fold-left
         (lambda (ret-bind lbind)
           (with-output-language
            (M7.1 LetBinding)
            (nanopass-case
             (M7.1 LetBinding) lbind
             ((,x ,t)
              (if (member x ref-x)
                  (cons `(,x ,t) ret-bind)
                  (begin
                    ;;(display (list "removing binding" x)) (newline)
                    ret-bind)))
             ((,x ,t ,e)
              (if (member x ref-x)
                  (cons `(,x ,t ,e) ret-bind)
                  (begin
                    ;;(display (list "removing binding" x))
                    ret-bind))))))
         '()
         lbind*)))

    (Decl
     : Decl (decl) -> Decl ()
     ((fn ,name (,x* ...) ,t ,[body _])
      `(fn ,name (,x* ...) ,t ,body)))
    
    (Body
     : Body (body) -> Body (x*)
     )

    (Stmt
     : Stmt (stmt) -> Stmt (x*)
     
     ((let (,[lbind x*] ...) ,[stmt x])
      ;;(display x)
      (let ((lbind (filter-bindings lbind x)))
        (values (if (null? lbind)
                    stmt
                    `(let (,lbind ...) ,stmt))
                (apply union (difference x (bound-variables lbind)) x*))))
     ((begin ,[stmt* x*] ...)
      (values `(begin ,stmt* ...) (apply union x*)))
     ((do ,[e x])
      (values `(do ,e) x))
     ((set! ,[e1 x1] ,[e2 x2])
      (values `(set! ,e1 ,e2) (union x1 x2)))
     ((kernel (,[e* x*] ...) ,fv ,[stmt x])
      (values `(kernel (,e* ...) ,fv ,stmt)
              (apply union x x*)))
     ((if ,[e x1] ,[stmt x2])
      (values `(if ,e ,stmt) (union x1 x2)))
     ((if ,[e x] ,[stmt1 x1] ,[stmt2 x2])
      (values `(if ,e ,stmt1 ,stmt2)
              (union x x1 x2)))
     ((while ,[e x1] ,[stmt x2])
      (values `(while ,e ,stmt) (union x1 x2)))
     ((for (,x ,[e1 x1] ,[e2 x2] ,[e3 x3]) ,[stmt x4])
      (values `(for (,x ,e1 ,e2 ,e3) ,stmt)
              (union x1 x2 x3 (difference x4 (set x)))))
     ((print ,[e1 x1] ,[e2 x2])
      (values `(print ,e1 ,e2) (union x1 x2)))
     ((print ,[e1 x1])
      (values `(print ,e1) x1))
     ((assert ,[e x])
      (values `(assert ,e) x))
     ((return ,[e x])
      (values `(return ,e) x)))

    (LetBinding
     : LetBinding (lbind) -> LetBinding (x*)
     ((,x ,t) (values `(,x ,t) (set)))
     ((,x ,t ,[e x*]) (values `(,x ,t ,e) x*)))
    
    (Expr
     : Expr (e) -> Expr (x*)
     ((var ,t ,x)
      (values `(var ,t ,x) (set x)))
     ((let (,[lbind x*] ...) ,[e x])
      ;;(display x)
      (let ((lbind (filter-bindings lbind x)))
        (values (if (null? lbind)
                    e
                    `(let (,lbind ...) ,e))
                (apply union (difference x (bound-variables lbind)) x*))))

     ((field ,[e x*] ,x) (values `(field ,e ,x) x*))
     ((box ,r ,t) (values `(box ,r ,t) (set)))
     ((empty-struct) (values `(empty-struct) (set)))
     ((c-expr ,t ,x) (values `(c-expr ,t ,x) (set)))
     ((int ,i) (values `(int ,i) (set)))
     ((u64 ,i) (values `(u64 ,i) (set)))
     ((float ,f) (values `(float ,f) (set)))
     ((bool ,b) (values `(bool ,b) (set)))
     ((char ,c) (values `(char ,c) (set)))
     ((str ,str-t) (values `(str ,str-t) (set)))
     ((vector ,t ,r ,[e* x*] ...)
      (values `(vector ,t ,r ,e* ...) (apply union x*)))
     ((cast ,t ,[e x])
      (values `(cast ,t ,e) x))
     ((begin ,[e x] ,[e* x*] ...)
      (values `(begin ,e ,e* ...) (apply union x x*)))
     ((call ,[e x] ,[e* x*] ...)
      (values `(call ,e ,e* ...)
              (apply union x x*)))
     ((deref ,[e x])
      (values `(deref ,e) x))
     ((addressof ,[e x])
      (values `(addressof ,e) x))
     ((vector-ref ,t ,[e0 x0] ,[e1 x1])
      (values `(vector-ref ,t ,e0 ,e1)
              (union x0 x1)))
     ((unsafe-vec-ptr ,t ,[e x])
      (values `(unsafe-vec-ptr ,t ,e) x))
     ((alloc ,[e1 x1] ,[e2 x2])
      (values `(alloc ,e1 ,e2) (union x1 x2)))
     ((if ,[e0 x0] ,[e1 x1] ,[e2 x2])
      (values `(if ,e0 ,e1 ,e2) (union x0 x1 x2)))
     ((region-ref ,t ,[e1 x1] ,[e2 x2])
      (values `(region-ref ,t ,e1 ,e2) (union x1 x2)))
     ((,op ,[e1 x1] ,[e2 x2])
      (values `(,op ,e1 ,e2) (union x1 x2))))

    #;(FakeExpr
     : Expr (e) -> Expr ()
     (else (error 'FakeExpr "invalid transformer called")))))
     
