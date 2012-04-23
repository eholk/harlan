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
   (elegant-weapons helpers))

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
     (inline-kernel t dims iters body))
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
         (assert (equal? dims dims^))
         (if (verbose)
             (begin
               (display "Harlan Compiler Message:")
               (display "optimize-fuse-kernels is inlining a kernel\n")))
         `(kernel ,t ,dims
                  (,@rest . ,iters^)
                  (let ((,x ,xt ,body^))
                    ,body))))
      (,else (make-2d-kernel t dims iters body))))

  ;; This seems wrong.  But here's the example:
  ;; (do (kernel [vec (vec int)]
  ;;       [(length (var (vec (vec int)) x_1))]
  ;;       [((row_2 (vec int))
  ;;         ((var (vec (vec int)) x_1) (vec (vec int))) 0)]
  ;;       [kernel (vec int) ((length (var (vec int) row_2)))
  ;;               (((i_3 int) ((var (vec int) row_2) (vec int))  0))
  ;;               (+ (var int i_3) (int 1))]))
  ;; All references to row_2 would be get_global_id(0),
  ;; and all references to i_3 would be get_global_id(1).
  ;; The matrix x_1 would be the only remaining argument.

  (define (make-2d-kernel t dims iters body)
    (match iters
      ((((,x ,xt) (,e ,et) ,i))
       (match body
         ((kernel ,t^ ,dims^
                  (((,bx ,bxt) ((var ,t^ ,ev) ,bet) ,d))
                  ,body^)
          (guard (eq? ev x))
          (match dims
            (((length (var (vec ,yt) ,y)))
             `(kernel ,t
                (,@dims (length (vector-ref ,yt (var (vec ,t) ,y) (int 0))))
                (((,x ,xt) (,e ,et) ,i)
                 ((,bx ,bxt) ((var ,t ,ev) ,bet) ,(+ d 1)))
                ,body^))))
         (,else `(kernel ,t ,dims ,iters ,body))))
      (,else `(kernel ,t ,dims ,iters ,body))))

  ;; end library
  )
