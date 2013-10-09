(library
    (harlan middle remove-lambdas)
  (export remove-lambdas)
  (import
   (rnrs)
   (only (chezscheme) pretty-print trace-define)
   (nanopass)
   (except (elegant-weapons match) ->)
   (harlan compile-opts)
   (harlan helpers)
   (harlan middle languages)
   (elegant-weapons sets)
   (only (elegant-weapons helpers) gensym andmap map-values))
  
  (define-pass uncover-lambdas : M0 (m) -> M1 ()
    (definitions
      (define closure-defs '())

      (define (remove-vars x* fv*)
        (let loop ((fv* fv*))
          (pair-case
           fv*
           ((a . d)
            (pair-case
             a
             ((x . t)
              (if (member x x*)
                  (loop d)
                  (cons a (loop d))))
             (_ => '())))
           (_ => '()))))

      (define (remove-duplicate-vars fv)
        (match fv
          (() '())
          (((,x ,t) . ,rest)
           `((,x ,t) . ,(remove-duplicate-vars (remove-vars (list x) rest))))))
      
      (define (free-vars e)
        (nanopass-case
         (M0 Expr) e
         ((int ,i) '())
         ((float ,f) '())
         ((var ,t ,x) (list (list x t)))
         ((lambda ,t ((,x* ,t*) ...) ,[e])
          (remove-vars x* e))
         ((let ((,x* ,t* ,[e*]) ...) ,[e])
          (apply union (remove-vars x* e) e*))
         ((if ,[e0] ,[e1] ,[e2])
          (union e0 e1 e2))
         ((vector-ref ,t ,[e0] ,[e1])
          (union e0 e1))
         ((call (var ,t ,x) ,[e*] ...)
          (apply union e*))
         ((invoke ,[e] ,[e*] ...)
          (apply union e e*))
         ((match ,t ,[e]
                 ((,x0 ,x* ...) ,[e*]) ...)
          (apply union e
                 (map (lambda (x e)
                        (remove-vars x e))
                      x* e*)))
         ((,op ,[e1] ,[e2])
          (union e1 e2))
         (else (error 'free-vars "Unexpected expression" e)))))

    (Module : Module (m) -> Module ())

    (Rho-Type : Rho-Type (t) -> Rho-Type ())
    
    (Expr : Expr (expr) -> Expr ()
          ((lambda ,[t] ((,x* ,t*) ...) ,e)
           (let* ((tag (gensym 'lambda))
                  (fv (remove-vars x* (remove-duplicate-vars (free-vars e))))
                  (fvx (map car fv))
                  (fvt (map (lambda (t) (Rho-Type (cadr t))) fv))
                  (e (Expr e)))
             (set! closure-defs
                   (cons (with-output-language
                          (M1 ClosureTag)
                          `(,tag ,t (,x* ...) ,e (,fvx ,fvt) ...))
                         closure-defs))
             `(make-closure ,t ,tag ,(map (lambda (x t)
                                            `(var ,t ,x))
                                          fvx fvt) ...))))
    ;; We need a body too, which defines ADTs and dispatch functions
    ;; for all the closures.
    (with-output-language
     (M1 Closures)
     (let ((m (Module m)))
       `(closures (,closure-defs ...) ,m))))

  (define-syntax nanopass-case2
    (syntax-rules (else)
      ((_ (lang term) (e1 e2)
          ((p1 p2) b) ...
          (else e))
       (nanopass-case
        (lang term) e1
        (p1 (nanopass-case
             (lang term) e2
             (p2 b)
             (else e))) ...
             (else e)))
      ((_ (lang term) (e1 e2)
          ((p1 p2) b) ...)
       (nanopass-case2
        (lang term) (e1 e2)
        ((p1 p2) b) ...
        (else (if #f 5))))))

  (define (map^2 f ls)
    (map (lambda (x) (map f x)) ls))
  
  (define-pass sort-closures : M1 (m) -> M2 ()
    (definitions
      ;; Returns whether types are equivalent up to renaming of region
      ;; variables.
      (define (type-compat? a b)
        (let loop ((a a)
                   (b b)
                   (env '()))
          (nanopass-case2
           (M1 Rho-Type) (a b)
           (((closure ,r1 (,t1* ...) ,-> ,t1)
             (closure ,r2 (,t2* ...) ,-> ,t2))
            ;; TODO: This needs to consider region variables and
            ;; renaming.
            (and (eq? (length t1*) (length t2*))
                 (andmap type-compat? (cons t1 t1*) (cons t2 t2*))))
           (((ptr ,t1) (ptr ,t2))
            (loop t1 t2 env))
           (((adt ,x1 ,r1) (adt ,x2 ,r2))
            (eq? x1 x2))
           ((,bt1 ,bt2) (equal? bt1 bt2))
           (else (begin
                   ;;(pretty-print "Failed Match!\n")
                   ;;(pretty-print a)
                   ;;(pretty-print b)
                   #f)))))

      (define (select-closure-type t closures)
        (match closures
          (() (values '() '()))
          ((,c . ,c*)
           (let-values (((a b) (select-closure-type t c*)))
             (nanopass-case
              (M1 ClosureTag) c
              ((,x ,t^ (,x0 ...) ,e (,x* ,t*) ...)
               (if (type-compat? t t^)
                   (values (cons c a)
                           b)
                   (values a
                           (cons c b)))))))))

      (define (sort-closures closures)
        (match closures
          (() '())
          ((,c . ,c*)
           (nanopass-case
            (M1 ClosureTag) c
            ((,x ,t (,x0 ...) ,e (,x* ,t*) ...)
             (let-values (((this rest)
                           (select-closure-type t c*)))
               `((,t ,c . ,this) . ,(sort-closures rest)))))))))

    (ClosureTag
     : ClosureTag (c) -> ClosureTag ()
     ((,x ,t (,x0 ...) ,[e] (,x* ,[t*]) ...)
      `(,x (,x0 ...) ,e (,x* ,t*) ...)))

    (Rho-Type : Rho-Type (t) -> Rho-Type ())
    
    (Closures
     : Closures (c) -> Closures ()
     ((closures (,ctag ...) ,[m])
      (match (sort-closures ctag)
        (((,t . ,c) ...)
         (let ((c-types (map (lambda (_) (gensym 'lambda-type)) t))
               (dispatch (map (lambda (_) (gensym 'dispatch)) t))
               (c (map^2 ClosureTag c)))
           (with-output-language
            (M2 Closures)
            `(closures
              ((,c-types ,dispatch ,(map Rho-Type t) ,c ...) ...)
              ,m))))))))

  (define-syntax pair-case
    (syntax-rules (=>)
      ((_ p
          ((a . d) b1)
          (x => b2))
       (let ((t p))
         (if (pair? t)
             (let ((a (car t))
                   (d (cdr t)))
               b1)
             (let ((x t))
               b2))))))
         
  
  (define-pass remove-closures : M2 (m) -> M3 ()
    (definitions
      ;; Returns whether types are equivalent up to renaming of region
      ;; variables.
      ;;
      ;; This is the sort of thing where nanopass polymorphism would
      ;; help.
      (define (type-compat? a b)
        (let loop ((a a)
                   (b b)
                   (env '()))
          (nanopass-case2
           (M2 Rho-Type) (a b)
           (((closure ,r1 (,t1* ...) ,-> ,t1)
             (closure ,r2 (,t2* ...) ,-> ,t2))
            ;; TODO: This needs to consider region variables and
            ;; renaming.
            (and (eq? (length t1*) (length t2*))
                 (andmap type-compat? (cons t1 t1*) (cons t2 t2*))))
           (((ptr ,t1) (ptr ,t2))
            (loop t1 t2 env))
           (((adt ,x1 ,r1) (adt ,x2 ,r2))
            (eq? x1 x2))
           (((adt ,x1) (adt ,x2))
            (eq? x1 x2))
           ((,bt1 ,bt2) (equal? bt1 bt2))
           (else (begin
                   ;;(pretty-print "Failed Match!")
                   ;;(pretty-print (unparse-M2 a))
                   ;;(pretty-print (unparse-M2 b))
                   #f)))))

      (define (find-env t env)
        (pair-case
         env
         ((c . rest)
          (match c
            ((,x0 ,x1 ,t^)
             (if (type-compat? t t^)
                 `(,x0 ,x1 ,t^)
                 (find-env t rest)))
            (,_ (error 'find-env "Wat?" _))))
         (_ => #f)))
      
      (define (find-typename t env)
        (match (find-env t env)
          ((,x0 ,x1 ,t)
           x0)
          (,_ (begin
                (pretty-print (unparse-M2 t))
                (pretty-print env)
                (error 'find-typename "Wat?" _)))))

      (define (find-dispatch t env)
        (match (find-env t env)
          ((,x0 ,x1 ,t)
           x1)
          (,_ (error 'find-dispatch "Wat?" _)))))

    (Rho-Type
     : Rho-Type (t env) -> Rho-Type ()
     ((closure ,r
               (,[Rho-Type : t* env -> t*] ...) ,-> ,[Rho-Type : t^ env -> t^])
      (let ((adt-name (find-typename t env)))
        `(adt ,adt-name ,r))))
    
    (AdtDeclPattern : AdtDeclPattern (pt env) -> AdtDeclPattern ())

    (ClosureCase
     : ClosureTag (t env) -> AdtDeclPattern ()
     ((,x (,x0 ...) ,e (,x* ,[Rho-Type : t* env -> t*]) ...)
      `(,x ,t* ...)))

    (ClosureMatch
     : ClosureTag (t formals ftypes env) -> MatchArm ()
     ((,x (,x0 ...) ,[Expr : e env -> e] (,x1 ,t1) ...)
      `((,x ,x1 ...)
        (let ((,x0 ,ftypes (var ,ftypes ,formals)) ...)
          ,e))))

    (MakeEnv
     : ClosureGroup (cgroup) -> * ()
     ((,x0 ,x1 ,t ,ctag ...)
      `(,x0 ,x1 ,t)))
    
    (ClosureGroup
     : ClosureGroup (cgroup env) -> * (typedef dispatch)
     ((,x0 ,x1 ,t ,ctag ...)
      (nanopass-case
        (M2 Rho-Type) t
        ((closure ,r (,t* ...) ,-> ,t)
         (values
           (with-output-language
             (M3 Decl)
             `(define-datatype (,x0 ,r) ,(map (lambda (t)
                                                (ClosureCase t env))
                                           ctag) ...))
           (with-output-language
             (M3 Decl)
             (let* ((formals (map (lambda _ (gensym 'formal)) t*))
                     (t* (map (lambda (t) (Rho-Type t env)) t*))
                     (t (Rho-Type t env))
                     (x (gensym 'closure))
                     (ctype (with-output-language
                              (M3 Rho-Type)
                              `(adt ,x0 ,r)))
                     (arms (map (lambda (t)
                                  (ClosureMatch t formals t* env))
                             ctag)))
               `(fn ,x1 (,(cons x formals) ...)
                  (fn (,(cons ctype t*) ...) -> ,t)
                  (return (match ,t (var ,ctype ,x)
                            ,arms ...))))))))
        ))

    (Closures
     : Closures (x) -> Module ()
     ((closures (,cgroup ...) ,m)
      (let ((env (map MakeEnv cgroup)))
        (let-values (([types dispatches]
                      (if (null? cgroup)
                          (values '() '())
                          (map-values (lambda (g)
                                        (ClosureGroup g env))
                                      cgroup ))))
          (Module m env types dispatches)))))

    (Body : Body (b env) -> Body ())
    (LetBinding : LetBinding (lbind env) -> LetBinding ())
    
    (MatchArm : MatchArm (arm env) -> MatchArm ())
    
    (Decl : Decl (d env) -> Decl ())
    
    (Expr
     : Expr (e env) -> Expr ()
     ((make-closure ,t ,x ,[Expr : e* env -> e*] ...)
      (let ((adt-name (find-typename t env)))
        (nanopass-case
         (M2 Rho-Type) t
         ((closure ,r (,t* ...) ,-> ,t^)
          `(call (var (fn (_) -> (adt ,adt-name ,r)) ,x) ,e* ...)))))
     ((invoke ,e ,[Expr : e* env -> e*] ...)
      (nanopass-case
       (M2 Expr) e
       ((var (closure ,r (,t* ...) ,-> ,t^) ,x)
        (let ((e (Expr e env))
              (t (with-output-language
                  (M2 Rho-Type)
                  `(closure ,r (,t* ...) ,-> ,t^))))
          (let ((dispatch (find-dispatch t env))
                (t (Rho-Type t env))
                (t^ (Rho-Type t^ env))
                (t* (map (lambda (t) (Rho-Type t env)) t*)))
            `(call
              (var (fn (,(cons t t*) ...) -> ,t^)
                   ,dispatch)
              ,(cons e e*) ...))))
       ((make-closure (closure ,r (,t* ...) ,-> ,t^) ,x ,[e**] ...)
        (let ((e (Expr e env))
              (t (with-output-language
                  (M2 Rho-Type)
                  `(closure ,r (,t* ...) ,-> ,t^))))
          (let ((dispatch (find-dispatch t env))
                (t (Rho-Type t env))
                (t^ (Rho-Type t^ env))
                (t* (map (lambda (t) (Rho-Type t env)) t*)))
            `(call
              (var (fn (,(cons t t*) ...) -> ,t^)
                   ,dispatch)
              ,(cons e e*) ...))))
       ((call (var (fn (,t*^ ...) ,->^ (closure ,r (,t* ...) ,-> ,t^)) ,x)
              ,e** ...)
        (let ((e (Expr e env))
              (t (with-output-language
                  (M2 Rho-Type)
                  `(closure ,r (,t* ...) ,-> ,t^))))
          (let ((dispatch (find-dispatch t env))
                (t (Rho-Type t env))
                (t^ (Rho-Type t^ env))
                (t* (map (lambda (t) (Rho-Type t env)) t*)))
            `(call
              (var (fn (,(cons t t*) ...) -> ,t^)
                   ,dispatch)
              ,(cons e e*) ...))))
       (else (error 'Expr "unknown invoke target" (unparse-M2 e))))))
    
    (Module
     : Module (m env types dispatches) -> Module ()
     ((module ,[decl env -> decl] ...)
      `(module ,(append decl types dispatches) ...)))
        
     )

  (define (remove-lambdas module)
    (>::> module
          uncover-lambdas
          sort-closures
          remove-closures))
  )

