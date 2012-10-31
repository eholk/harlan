(library
  (harlan middle remove-danger)
  (export remove-danger)
  (import
   (rnrs)
   (harlan helpers)
   (elegant-weapons helpers))

  (define-match remove-danger
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
    ((let-region (,r) ,[body]) `(let-region (,r) ,body))
    ((set! ,[Expr -> lhs] ,[Expr -> rhs])
     `(set! ,lhs ,rhs))
    ((if ,[Expr -> test] ,[conseq] ,[altern])
     `(if ,test ,conseq ,altern))
    ((if ,[Expr -> test] ,[conseq])
     `(if ,test ,conseq))
    ((while ,[Expr -> test] ,[body])
     `(while ,test ,body))
    ((for (,x ,[Expr -> start] ,[Expr -> stop] ,[Expr -> step]) ,[body])
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
    ((do ,[Expr -> e])
     `(do ,e)))

  (define-match Expr
    ((,t ,v) (guard (scalar-type? t)) `(,t ,v))
    ((var ,t ,x) `(var ,t ,x))
    ((int->float ,[e]) `(int->float ,e))
    ((make-vector ,t ,[e])
     `(make-vector ,t ,e))
    ((iota ,[e]) `(iota ,e))
    ((vector-ref ,t ,[v] ,[i])
     (let ((v-var (gensym 'vec))
           (i-var (gensym 'refindex)))
       `(let ((,v-var (vec ,t) ,v)
              (,i-var int ,i))
          (begin
            (if (>= (var int ,i-var) (length (var (vec ,t) ,v-var)))
                (error ,(gensym 'vector-length-error)))
            (vector-ref ,t (var (vec ,t) ,v-var) (var int ,i-var))))))
    ((length ,[e])
     `(length ,e))
    ((vector ,t ,[e*] ...)
     `(vector ,t . ,e*))
    ((vector-r ,t ,r ,[e] ...)
     `(vector-r ,t ,r . ,e))
    ((call ,[f] ,[args] ...)
     `(call ,f . ,args))
    ((if ,[test] ,[conseq] ,[altern])
     `(if ,test ,conseq ,altern))
    ((kernel
      (vec ,inner-type)
      (,[dim] ...)
      (((,x* ,t*) (,[xs*] ,ts*) ,d*) ...)
      ;; TODO: put the cata form for body back, once we figure out
      ;; how to do kernel error reporting.
      ,body)
     ;; TODO
     `(kernel
       (vec ,inner-type)
       ,dim
       (((,x* ,t*) (,xs* ,ts*) ,d*) ...) ,body))
    ((let ((,x* ,t* ,[e*]) ...) ,[e])
     `(let ((,x* ,t* ,e*) ...) ,e))
    ((begin ,[Stmt -> s*] ... ,[e])
     `(begin ,s* ... ,e))
    ((,op ,[lhs] ,[rhs])
     (guard (or (relop? op) (binop? op)))
     `(,op ,lhs ,rhs)))

  ;; end library
  )