(library (harlan middle optimize-lift-lets)
  (export optimize-lift-lets verify-optimize-lift-lets)
  (import
   (rnrs)
   (harlan helpers)
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
    (((,x ,t ,e) . ,[liftable pinned])
     ;; TODO: get a more correct test for whether this can be lifted
     (if (let ((fv (free-vars-Expr e)))
           (and (not (member x fv))
                (null? (intersection vars fv))
                (pure? e)))
         (values (cons `(,x ,t ,e) liftable) pinned)
         (values liftable (cons `(,x ,t ,e) pinned)))))
      
  (define-match Stmt
    ((let ((,x* ,t* ,e*) ...)
       ,[body bindings])
     (let-values (((liftable pinned) ((split-bindings x*) bindings)))
       (values (make-let pinned body) (append (map list x* t* e*) liftable))))
    ((begin ,[stmt* bindings*] ... ,[Expr -> e bindings])
     (values
      `(begin ,@(map make-let bindings* stmt*) ,(make-let bindings e))
      '()))
    ((for (,x ,[Expr -> start start-bindings]
              ,[Expr -> end end-bindings]
              ,[Expr -> step step-bindings])
          ,[body bindings])
     (let-values (((liftable pinned)
                   ((split-bindings (list x)) bindings)))
       (values
        `(for (,x ,start ,end ,step)
              ,(make-let pinned body))
        (append start-bindings end-bindings step-bindings liftable))))
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
    ((print . ,e*) (values `(print . ,e*) '()))
    ((return) (values `(return) '()))
    ((return ,e) (values `(return ,e) '()))
    ((do ,[Expr -> e bindings])
     (values `(do ,e) bindings)))

  (define-match Expr
    (,e (values e '())))

  (define-match free-vars-Expr
    ((,t ,x) (guard (scalar-type? t)) '())
    ((var ,t ,x) (list x))
    ((,op ,[e1] ,[e2])
     (guard (or (binop? op) (relop? op)))
     (union e1 e2))
    ((call ,[fn] ,[args] ...)
     (apply union fn args))
    ((int->float ,[e]) e)
    ((length ,[e]) e)
    ((make-vector ,t ,[e]) e)
    ((vector-ref ,t ,[x] ,[i])
     (union x i))
    ((kernel ,t ,dims (((,x* ,t*) (,[xs*] ,ts*) ,d) ...) ,[e])
     (apply union (difference e x*) xs*))
    ((let ((,x* ,t* ,[e*]) ...) ,[e])
     (apply union (difference e x*) e*))
    ((if ,[t] ,[c] ,[a])
     (union t c a))
    ((c-expr ,t ,v) `(c-expr ,t ,v))
    ((begin ,[free-vars-Stmt -> s*] ... ,[e])
     (apply union e s*)))

  (define-match free-vars-Stmt
    ((print ,[free-vars-Expr -> fv*]) fv*)
    ((assert ,[free-vars-Expr -> fv*]) fv*)
    ((return) `())
    ((return ,[free-vars-Expr -> fv*]) fv*)
    ((for (,x ,[free-vars-Expr -> sfv*] ,[free-vars-Expr -> efv*]
              ,[free-vars-Expr -> stepfv*]) ,[fv*])
     (union sfv* efv* stepfv* (difference fv* `(,x))))
    ((set! ,[free-vars-Expr -> x] ,[free-vars-Expr -> v])
     (union x v))
    ((if ,[free-vars-Expr -> test] ,[conseq])
     (union test conseq))
    ((if ,[free-vars-Expr -> test] ,[conseq] ,[altern])
     (union test conseq altern))
    ((while ,[free-vars-Expr -> test] ,[body])
     (union test body))
    ((let ((,x* ,t* ,[free-vars-Expr -> e*]) ...) ,[e])
     (apply union (difference e x*) e*))
    ((begin ,[s*] ...)
     (apply union s*)))

  (define-match pure?
    ((int ,n) #t)
    ((float ,x) #t)
    ((bool ,b) #t)
    ((var ,t ,x) #t)
    ((int->float ,[e]) e)
    ((,op ,[lhs] ,[rhs])
     (guard (or (binop? op) (relop? op)))
     (and lhs rhs))
    ((vector ,t . ,e*) #f)
    ((make-vector ,t ,e) #f)
    ((length ,[e]) e)
    ;; Don't lift function calls.
    ((call ,fn ,arg* ...) #f)
    ((vector-ref ,t ,[x] ,[i])
     (and x i))
    ((iota ,[e]) e)
    ;; TODO: Kernels might actually be pure in some cases.
    ((kernel . ,_) #f))
  
  ;; end library
  )