(library
  (harlan middle make-kernel-dimensions-explicit)
  (export make-kernel-dimensions-explicit)
  (import
   (rnrs)
   (harlan helpers)
   (elegant-weapons helpers))

  (define-match make-kernel-dimensions-explicit
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
    ((kernel
       (vec ,inner-type)
       (((,x ,t) (,[xs] ,ts))
        ((,x* ,t*) (,[xs*] ,ts*)) ...) ,[body])
     ((arg-length
       ts
       (lambda (len xs^)
         `(kernel (vec ,inner-type)
                  ,len
                  (((,x ,t) (,xs^ ,ts) 0)
                   ((,x* ,t*) (,xs* ,ts*) 0) ...) ,body)))
      xs))
    ((iota ,e)
     `(kernel (vec int) (,e) ()
        (call
         (c-expr ((int) -> int) get_global_id)
         (int 0))))
    ((let ((,x* ,t* ,[e*]) ...) ,[e])
     `(let ((,x* ,t* ,e*) ...) ,e))
    ((begin ,[Stmt -> s*] ... ,[e])
     `(begin ,s* ... ,e))
    ((,op ,[lhs] ,[rhs])
     (guard (or (relop? op) (binop? op)))
     `(,op ,lhs ,rhs)))

  ;; This might be missing some opportunities.
  (define (arg-length ts finish)
    (lambda (s)
      (match s
        ((make-vector ,t (int ,n))
         (finish `((int ,n)) `(make-vector ,t (int ,n))))
        ((kernel ,t (,dim ...) ,arg ,body)
         (finish dim `(kernel ,t ,dim ,arg ,body)))
        ((var ,t ,x)
         (finish `((length (var ,t ,x))) `(var ,t ,x)))
        (,else
         (let ((xs^ (gensym 'xs)))
           `(let ((,xs^ ,ts ,else))
              ,(finish `((length (var ,ts ,xs^)))
                       `(var ,ts ,xs^))))))))

  ;; end library
  )
