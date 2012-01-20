(library
    (harlan middle optimize-lift-lets)
  (export optimize-lift-lets verify-optimize-lift-lets)
  (import
   (rnrs)
   (elegant-weapons helpers)
   (elegant-weapons sets)
   (harlan verification-passes))

  ;; This is an optimization pass, so it doesn't change the grammar.
  (define verify-optimize-lift-lets verify-remove-nested-kernels)
  
  ;; This optimization does something like loop invariant code
  ;; motion. The main idea is to let bindings up as high as possible
  ;; in the program. This may not be a good idea in general, but we'll
  ;; find out.
  ;;
  ;; This pass ignores code inside of kernels. That might need to be
  ;; treating separately. Kernels shouldn't be doing memory allocation
  ;; anyway.
  ;;
  ;; The approach pass uses is to walk all the code. We return two
  ;; values: a rewritten AST and a list of bindings that need to be
  ;; placed. When we pass a binding form that binds a variable one of
  ;; the liftable bindings depends on, we go ahead and place this let
  ;; immediately inside this form. We then continue to pass the rest
  ;; of the bindings up the tree.

  (define-match optimize-lift-lets
    ((module ,[Decl -> decl*] ...)
     `(module ,decl* ...)))

  (define (make-let bindings body)
    (if (null? bindings)
        body
        `(let ,bindings ,body)))
  
  (define-match Decl
    ((extern . ,_)
     `(extern . ,_))
    ((fn ,name ,args ,type ,[Stmt -> body bindings])
     `(fn ,name ,args ,type
          ,(make-let bindings body))))

  ;; Takes a list of variables and bindings. Returns a list of
  ;; bindings that can and cannot be lifted over the definition of the
  ;; variables in vars.
  (define-match (split-bindings vars)
    (() (values '() '()))
    (((,x ,e) . ,[liftable pinned])
     ;; TODO: get a more correct test for whether this can be lifted
     (if (let ((fv (free-vars e)))
           (and (not (member x fv))
                (null? (intersection vars fv))
                (pure? e)))
         (values (cons `(,x ,e) liftable) pinned)
         (values liftable (cons `(,x ,e) pinned)))))
      
  (define-match Stmt
    ((let ((,x* ,e*) ...)
       ,[body bindings])
     (let-values (((liftable pinned) ((split-bindings x*) bindings)))
       (values (make-let pinned body) (append (map list x* e*) liftable))))
    ((begin ,[stmt* bindings*] ... ,[Expr -> e bindings])
     (values
      `(begin ,@(map make-let bindings* stmt*) ,(make-let bindings e))
      '()))
    ((for (,x ,[Expr -> start start-bindings] ,[Expr -> end end-bindings])
          ,[body bindings])
     (let-values (((liftable pinned)
                   ((split-bindings (list x)) bindings)))
       (values
        `(for (,x ,start
                  ,end)
              ,(make-let pinned body))
        (append start-bindings end-bindings liftable))))
    ((while ,[Expr -> test test-bindings]
       ,[body bindings])
     (values `(while ,test ,(make-let bindings body))
             test-bindings))
    ((if ,[Expr -> e bindings] ,c)
     (values `(if ,e ,c) bindings))
    ((if ,[Expr -> e bindings] ,c ,a)
     (values `(if ,e ,c ,a) bindings))
    ((set! ,[Expr -> x x-bindings] ,[Expr -> e e-bindings])
     (values `(set! ,x ,e)
             (append x-bindings e-bindings)))
    ((assert ,[Expr -> e bindings])
     (values `(assert ,e) bindings))
    ((print ,e) (values `(print ,e) '()))
    ((return) (values `(return) '()))
    ((return ,e) (values `(return ,e) '()))
    ((do ,[Expr -> e bindings])
     (values `(do ,e) bindings))
    ((vector-set! ,t ,x ,i ,v)
     (values `(vector-set! ,t ,x ,i ,v) '())))

  (define-match Expr
    (,e (values e '())))

  (define-match free-vars
    ((,t ,x) (guard (scalar-type? t)) '())
    ((var ,t ,x) (list x))
    ((,op ,[e1] ,[e2])
     (guard (or (binop? op) (relop? op)))
     (union e1 e2))
    ((iota ,[e]) e)
    ((call ,[fn] ,[args] ...)
     (apply union fn args))
    ((int->float ,[e]) e)
    ((length ,[e]) e)
    ((make-vector ,t ,[e]) e)
    ((vector ,t ,[e*] ...)
     (apply union e*))
    ((vector-ref ,t ,[x] ,[i])
     (union x i))
    ((kernel ,t ,dims (((,x* ,t*) (,[xs*] ,ts*) ,d) ...) ,[e])
     (apply union (cons (difference e x*) xs*)))
    ((reduce ,t ,op ,[e]) e)
    ((let ((,x* ,[e*]) ...) ,[e])
     (apply union (cons (difference e x*) e*)))
    ((if ,[t] ,[c] ,[a])
     (union t c a))
    ((begin ,[free-vars-Stmt -> s*] ... ,[e])
     (union e (apply union s*))))

  (define-match free-vars-Stmt
    ((set! ,[free-vars -> x] ,[free-vars -> v])
     (union x v))
    ((if ,[free-vars -> test] ,[conseq])
     (union test conseq))
    ((if ,[free-vars -> test] ,[conseq] ,[altern])
     (union test conseq altern))
    ((while ,[free-vars -> test] ,[body])
     (union test body))
    ((let ((,x* ,[free-vars -> e*]) ...) ,[e])
     (apply union (cons (difference e x*) e*)))
    ((begin ,[s*] ...)
     (apply union s*)))

  (define-match pure?
    ((int ,n) #t)
    ((float ,x) #t)
    ((var ,t ,x) #t)
    ((int->float ,[e]) e)
    ((,op ,[lhs] ,[rhs])
     (guard (binop? op))
     (and lhs rhs))
    ((vector ,t ,e* ...) #f)
    ((make-vector ,t ,e) #f)
    ((vector-ref ,t ,[x] ,[i])
     (and x i))
    ((iota ,[e]) e))
  
  ;; end library
  )