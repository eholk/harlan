(library
  (harlan front flatten-lets)
  (export flatten-lets)
  (import
    (rnrs)
    (elegant-weapons helpers)
    (elegant-weapons match))

;; parse-harlan takes a syntax tree that a user might actually want
;; to write and converts it into something that's more easily
;; analyzed by the type inferencer and the rest of the compiler.
;; This subsumes the functionality of the previous
;; simplify-literals mini-pass.

;; unnests lets, checks that all variables are in scope, and
;; renames variables to unique identifiers
  
(define-match flatten-lets
  ((module ,[Decl -> decl*] ...)
   `(module . ,decl*)))

(define-match Decl
  ((fn ,name ,args ,type ,[Stmt -> stmt*] ...)
   `(fn ,name ,args ,type ,(make-begin (apply append stmt*))))
  (,else else))

(define-match Stmt
  ((let ((,x* ,[Expr -> e* t*]) ...) ,[stmt*])
   `(,@(map (lambda (x t e) `(let ,x ,t . ,e)) x* t* e*)
     ,(make-begin stmt*)))
  ((if ,[Expr -> test tt] ,[conseq])
   `((if ,@test ,@conseq)))
  ((if ,[Expr -> test tt] ,[conseq] ,[alt])
   `((if ,@test ,@conseq ,@alt)))
  ((begin ,[stmt*] ...)
   `(,(make-begin (apply append stmt*))))
  ((print ,[Expr -> expr _])
   `((print . ,expr)))
  ((assert ,[Expr -> expr _])
   `((assert . ,expr)))
  ((return ,[Expr -> expr _])
   `((return . ,expr)))
  ((for (,x ,start ,end) ,[stmt])
   `((for (,x ,start ,end) ,(make-begin stmt))))
  ((while ,test ,[stmt])
   `((while ,test ,(make-begin stmt))))
  ((do ,[Expr -> expr _])
   `((do . ,expr)))
  ((set! ,[Expr -> e1 t1] ,[Expr -> e2 t2])
   `((set! ,@e1 ,@e2)))
  ((vector-set! ,t ,[Expr -> e1 t1]
     ,[Expr -> e2 t2] ,[Expr -> e3 t3])
   `((vector-set! ,t ,@e1 ,@e2 ,@e3))))

(define-match Expr
  ((int ,n) (values `((int ,n)) 'int))
  ((u64 ,n) (values `((u64 ,n)) 'u64))
  ((float ,n) (values `((float ,n)) 'float))
  ((str ,s) (values `((str ,s)) 'str))
  ((var ,type ,x) (values `((var ,type ,x)) type))
  ((if ,[test tt] ,[conseq tc] ,[alt ta])
   (values `((if ,(make-begin test)
                 ,(make-begin conseq)
                 ,(make-begin alt)))
     tc))
  ((begin ,[Stmt -> stmt*] ... ,[expr t])
   (values `(,(make-begin `(,@(apply append stmt*) . ,expr))) t))
  ((let ((,x* ,[e* t*]) ...) ,[Stmt -> stmt*] ... ,[expr t])
   (values
     `(,@(map (lambda (x t e) `(let ,x ,t . ,e)) x* t* e*)
       ,(make-begin `(,@(apply append stmt*) . ,expr)))
     t))
  ((vector ,type ,[e t] ...)
   (values `((vector ,type . ,(apply append e))) type))
  ((vector-ref ,type ,[e1 t1] ,[e2 t2])
   (values `((vector-ref ,type ,@e1 ,@e2)) type))
  ((kernel ,type (((,x ,tx) (,[e* te^] ,te)) ...)
     ,[Stmt -> stmt*] ... ,[expr t^])
   (values
     `((kernel ,type (((,x ,tx) (,@e* ,te)) ...) 
         ,(make-begin `(,@(apply append stmt*) . ,expr))))
     type))
  ((reduce ,type ,op ,[expr t])
   (values `((reduce ,type ,op ,@expr)) type))
  ((iota (int ,n))
   (values `((iota (int ,n))) `(vector int ,n)))
  ((length ,n)
   (values `((length ,n)) 'int))
  ((int->float ,[expr t])
   (values `((int->float ,@expr)) 'float))
  ((make-vector ,type (int ,n))
   (values `((make-vector ,type (int ,n))) `(vector ,type ,n)))
  ((,op ,[e1 t1] ,[e2 t2]) (guard (binop? op))
   (values `((,op ,@e1 ,@e2)) t1))
  ((,op ,[e1 t1] ,[e2 t2]) (guard (relop? op))
   (values `((,op ,@e1 ,@e2)) 'bool))
  ((call ,type ,var ,[expr* t*] ...)
   (values `((call ,type ,var . ,(apply append expr*))) type)))

;; end library
)
