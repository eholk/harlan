(library
  (harlan middle make-work-size-explicit)
  (export make-work-size-explicit)
  (import
   (rnrs)
   (harlan helpers)
   (except (elegant-weapons helpers) ident?))

  (define-match make-work-size-explicit
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
    ((let-region (,r ...) ,[body]) `(let-region (,r ...) ,body))
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
     `(begin . ,stmt*))
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
    ((make-vector ,t ,r ,[e])
     `(make-vector ,t ,r ,e))
    ((vector ,t ,r ,[e] ...)
     `(vector ,t ,r . ,e))
    ((vector-ref ,t ,[v] ,[i])
     `(vector-ref ,t ,v ,i))
    ((length ,[e])
     `(length ,e))
    ((call ,[f] ,[args] ...)
     `(call ,f . ,args))
    ((if ,[t] ,[c] ,[a]) `(if ,t ,c ,a))
    ((if ,[t] ,[c]) `(if ,t ,c))
    ((kernel ,kt ,r ,dim ,ws ,args ,body)
     `(kernel ,kt ,r ,ws ,args ,body))
    ((kernel
         (vec ,inner-type) ,r
         ,dim
         (((,x ,t) (,[xs] ,ts) ,d)
          ((,x* ,t*) (,[xs*] ,ts*) ,d*) ...) ,[body])
     ((arg-length
       ts
       (lambda (len xs^)
         (assert (= (length len) dim))
         `(kernel (vec ,inner-type) ,r
            ,len
            (((,x ,t) (,xs^ ,ts) ,d)
             ((,x* ,t*) (,xs* ,ts*) ,d*) ...) ,body)))
      xs))
    ((let ((,x* ,t* ,[e*]) ...) ,[e])
     `(let ((,x* ,t* ,e*) ...) ,e))
    ((begin ,[Stmt -> s*] ... ,[e])
     `(begin ,s* ... ,e))
    ((,op ,[lhs] ,[rhs])
     (guard (or (relop? op) (binop? op)))
     `(,op ,lhs ,rhs)))

  (define (arg-length ts finish)
    (lambda (s)
      (match s
        ((make-vector ,t ,r (int ,n))
         (finish `((int ,n)) `(make-vector ,t ,r (int ,n))))
        ((kernel ,t ,r (,dim ...) ,arg ,body)
         (finish dim `(kernel ,t ,r ,dim ,arg ,body)))
        ((var ,t ,x)
         (finish `((length (var ,t ,x))) `(var ,t ,x)))
        (,else
         (let ((xs^ (gensym 'xs)))
           `(let ((,xs^ ,ts ,else))
              ,(finish `((length (var ,ts ,xs^)))
                       `(var ,ts ,xs^))))))))

  ;; end library
  )
