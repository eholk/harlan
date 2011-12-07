(library
    (harlan middle make-kernel-dimensions-explicit)
  (export make-kernel-dimensions-explicit)
  (import
   (rnrs)
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
    ((let ((,x* ,[Expr -> e*]) ...) ,[body])
     `(let ((,x* ,e*) ...) ,body))
    ((begin ,[stmt*] ...)
     `(begin ,stmt* ...))
    ((return ,[Expr -> e])
     `(return ,e))
    ((do ,[Expr -> e])
     `(do ,e)))

  (define-match Expr
    ((int ,n) `(int ,n))
    ((var ,t ,x) `(var ,t ,x))
    ((vector ,t ,[e*] ...)
     `(vector ,t ,e* ...))
    ((kernel (vec ,inner-type ,n) (((,x ,t) (,[xs] ,ts)) ...) ,[body])
     `(kernel (vec ,inner-type ,n) (,n) (((,x ,t) (,xs ,ts) 0) ...) ,body))
    ((,op ,[lhs] ,[rhs])
     (guard (binop? op))
     `(,op ,lhs ,rhs)))

  ;; end library
  )
