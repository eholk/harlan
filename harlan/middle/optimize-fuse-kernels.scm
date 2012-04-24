(library
  (harlan middle optimize-fuse-kernels)
  (export optimize-fuse-kernels
    verify-optimize-fuse-kernels
    make-2d-kernel
    Expr
    Stmt)
  (import
   (rnrs)
   (harlan helpers)
   (only (harlan verification-passes)
         verify-make-kernel-dimensions-explicit)
   (harlan compile-opts)
   (elegant-weapons helpers)
   (only (chezscheme) trace-define))

  (define verify-optimize-fuse-kernels
    verify-make-kernel-dimensions-explicit)

  (define-match optimize-fuse-kernels
    ((module ,[Decl -> decl*] ...)
     `(module ,decl* ...)))
    
  (define-match Decl
    ((fn ,name ,args ,t ,[Stmt -> stmt])
     `(fn ,name ,args ,t ,stmt))
    ((extern ,name ,args -> ,rtype)
     `(extern ,name ,args -> ,rtype)))

  (define-match Stmt
    ((let ((,x* ,t* ,[Expr -> e*]) ...) ,[body])
     `(let ((,x* ,t* ,e*) ...) ,body))
    ((set! ,[Expr -> lhs] ,[Expr -> rhs])
     `(set! ,lhs ,rhs))
    ((if ,[Expr -> test] ,[conseq] ,[altern])
     `(if ,test ,conseq ,altern))
    ((if ,[Expr -> test] ,[conseq])
     `(if ,test ,conseq))
    ((while ,[Expr -> test] ,[body])
     `(while ,test ,body))
    ((for (,x ,[Expr -> start]
              ,[Expr -> stop]
              ,[Expr -> step])
          ,[body])
     `(for (,x ,start ,stop ,step) ,body))
    ((begin ,[stmt*] ...)
     `(begin ,stmt* ...))
    ((print ,[Expr -> e] ...)
     `(print . ,e))
    ((assert ,[Expr -> e])
     `(assert ,e))
    ((return) `(return))
    ((return ,[Expr -> e])
     `(return ,e))
    ((error ,x) `(error ,x))
    ((do ,[Expr -> e])
     `(do ,e)))

  (define-match Expr
    ((,t ,v) (guard (scalar-type? t)) `(,t ,v))
    ((var ,t ,x) `(var ,t ,x))
    ((int->float ,[e]) `(int->float ,e))
    ((make-vector ,t ,[e])
     `(make-vector ,t ,e))
    ((c-expr ,t ,v)
     `(c-expr ,t ,v))
    ((vector-ref ,t ,[v] ,[i])
     `(vector-ref ,t ,v ,i))
    ((length ,[e])
     `(length ,e))
    ((call ,[f] ,[args] ...)
     `(call ,f ,args ...))
    ((if ,[test] ,[conseq] ,[altern])
     `(if ,test ,conseq ,altern))
    ((if ,[test] ,[conseq])
     `(if ,test ,conseq))
    ((kernel ,t ,dims ,iters ,[body])
     (make-2d-kernel t dims iters body))
    ((let ((,x* ,t* ,[e*]) ...) ,[e])
     `(let ((,x* ,t* ,e*) ...) ,e))
    ((begin ,[Stmt -> s*] ... ,[e])
     `(begin ,s* ... ,e))
    ((,op ,[lhs] ,[rhs])
     (guard (or (relop? op) (binop? op)))
     `(,op ,lhs ,rhs)))

  (define (inline-kernel t dims iters body)
    (match iters
      ((((,x ,xt)
         ((kernel ,t^ ,dims^ ,iters^ ,body^)
          ,et)
         ,i)
        . ,rest)
       ;; Super kernel!
       (begin
         (if (verbose)
             (begin
               (display "Harlan Compiler Message:")
               (display "optimize-fuse-kernels is inlining a kernel\n")))
         (Expr
          `(kernel ,t ,dims
                   (,@rest . ,iters^)
                   (let ((,x ,xt ,body^))
                     ,body)))))
      (,else `(kernel ,t ,dims ,iters ,body))))

  ;; This seems wrong.  But here's the example:
  ;; (do (kernel [vec (vec int)]
  ;;       [(length (var (vec (vec int)) x_1))]
  ;;       [((row_2 (vec int))
  ;;         ((var (vec (vec int)) x_1) (vec (vec int))) 0)]
  ;;       [kernel (vec int) ((length (var (vec int) row_2)))
  ;;               (((i_3 int) ((var (vec int) row_2) (vec int))  0))
  ;;               (+ (var int i_3) (int 1))]))

  (define (make-2d-kernel t dims iters body)
    (let ((arg-vars (map caar iters)))
      (match body
        ((kernel ,t^ ,dims^ ,iters^ ,body^)
         (guard (and ((not-in arg-vars) dims^)
                     ((not-in arg-vars) iters^)))
         (Expr
          `(kernel
               ,t
             (,@dims ,@dims^)
             (,@iters
              ,@(map incr-dimension iters^))
             ,(incr-dim-expr body^))))
        (,else
         (inline-kernel t dims iters body)))))

  (define-match (not-in argv)
    ((var ,t ,x) (not (memq x argv)))
    ((,x* ...)
     (andmap (not-in argv) x*))
    (,x #t))

  (define (make-env iters)
    (match iters
      (() `())
      ((((,x ,xt) (,xs ,ts) ,d) . ,[env])
       (cons `(,x . ,xs) env))))

  (define (incr-dimension iter)
    (match iter
      ((,arg ,exp ,dim)
       `(,arg ,exp ,(+ dim 1)))))

  (define-match incr-dim-stmt
    ((let ((,x* ,t* ,[incr-dim-expr -> e*]) ...) ,[body])
     `(let ((,x* ,t* ,e*) ...) ,body))
    ((set! ,[incr-dim-expr -> lhs] ,[incr-dim-expr -> rhs])
     `(set! ,lhs ,rhs))
    ((if ,[incr-dim-expr -> test] ,[conseq] ,[altern])
     `(if ,test ,conseq ,altern))
    ((if ,[incr-dim-expr -> test] ,[conseq])
     `(if ,test ,conseq))
    ((while ,[incr-dim-expr -> test] ,[body])
     `(while ,test ,body))
    ((for (,x ,[incr-dim-expr -> start]
              ,[incr-dim-expr -> stop]
              ,[incr-dim-expr -> step])
          ,[body])
     `(for (,x ,start ,stop ,step) ,body))
    ((begin ,[stmt*] ...)
     `(begin ,stmt* ...))
    ((print ,[incr-dim-expr -> e] ...)
     `(print . ,e))
    ((assert ,[incr-dim-expr -> e])
     `(assert ,e))
    ((return) `(return))
    ((return ,[incr-dim-expr -> e])
     `(return ,e))
    ((error ,x) `(error ,x))
    ((do ,[incr-dim-expr -> e])
     `(do ,e)))

  (define-match incr-dim-expr
    ((,t ,v) (guard (scalar-type? t)) `(,t ,v))
    ((var ,t ,x) `(var ,t ,x))
    ((int->float ,[e]) `(int->float ,e))
    ((make-vector ,t ,[e])
     `(make-vector ,t ,e))
    ((c-expr ,t ,v)
     `(c-expr ,t ,v))
    ((vector-ref ,t ,[v] ,[i])
     `(vector-ref ,t ,v ,i))
    ((length ,[e])
     `(length ,e))
    ((call (c-expr ,t get_global_id) (int ,n))
     `(call (c-expr ,t get_global_id) (int ,(+ n 1))))
    ((call ,[f] ,[args] ...)
     `(call ,f ,args ...))
    ((if ,[test] ,[conseq] ,[altern])
     `(if ,test ,conseq ,altern))
    ((kernel ,t (,[dims] ...)
             (((,x ,xt) (,[e] ,et) ,d) ...)
             ,[body])
     `(kernel ,t ,dims
              (((,x ,xt) (,e ,et) ,d) ...)
              ,body))
    ((let ((,x* ,t* ,[e*]) ...) ,[e])
     `(let ((,x* ,t* ,e*) ...) ,e))
    ((begin ,[incr-dim-stmt -> s*] ... ,[e])
     `(begin ,s* ... ,e))
    ((,op ,[lhs] ,[rhs])
     (guard (or (relop? op) (binop? op)))
     `(,op ,lhs ,rhs)))


  ;; end library
  )
