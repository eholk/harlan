(library
  (harlan middle flatten-lets)
  (export flatten-lets)
  (import (rnrs) (elegant-weapons helpers))

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
  ((fn ,name ,args ,type ,[Stmt -> stmt])
   `(fn ,name ,args ,type ,(make-begin `(,stmt))))
  (,else else))

(define-match Stmt
  ((let ((,x* ,[Expr -> e* t*]) ...) ,[stmt])
   `(begin
      ,@(map (lambda (x t e) `(let ,x ,t ,e)) x* t* e*)
      ,stmt))
  ((if ,[Expr -> test tt] ,[conseq])
   `(if ,test ,conseq))
  ((if ,[Expr -> test tt] ,[conseq] ,[alt])
   `(if ,test ,conseq ,alt))
  ((begin ,[stmt*] ...)
   (make-begin stmt*))
  ((print ,[Expr -> expr _])
   `(print ,expr))
  ((assert ,[Expr -> expr _])
   `(assert ,expr))
  ((return ,[Expr -> expr _])
   `(return ,expr))
  ((for (,x ,start ,end) ,[stmt])
   `(for (,x ,start ,end) ,stmt))
  ((while ,test ,[stmt])
   `(while ,test ,stmt))
  ((kernel ,dims (((,x ,tx) (,[Expr -> e* te^] ,te) ,dim) ...)
     (free-vars . ,fv*) ,[stmt])
   ;; this is an amazing hack because we don't
   ;; do any sort of typechecking later
   (let ((fvt* (match stmt
                 ((var ,t ,x) (guard (memq x fv*))
                  `((,x . ,t)))
                 ((,[x] ...)
                  (apply append x))
                 (,x '()))))
     `(kernel ,dims (((,x ,tx) (,e* ,te) ,dim) ...)
        (free-vars ,@(map (lambda (fv)
                            `(,fv ,(cdr (assq fv fvt*))))
                       fv*))
        ,stmt)))
  ((do ,[Expr -> expr _])
   `(do ,expr))
  ((set! ,[Expr -> e1 t1] ,[Expr -> e2 t2])
   `(set! ,e1 ,e2)))

(define-match Expr
  ((void) (values `(void) 'void))
  ((int ,n) (values `(int ,n) 'int))
  ((u64 ,n) (values `(u64 ,n) 'u64))
  ((float ,n) (values `(float ,n) 'float))
  ((str ,s) (values `(str ,s) 'str))
  ((var ,type ,x) (values `(var ,type ,x) type))
  ((c-expr ,type ,x) (values `(c-expr ,type ,x) type))
  ((if ,[test tt] ,[conseq tc] ,[alt ta])
   (values `(if ,test ,conseq ,alt) tc))
  ((cast ,t ,[expr t^])
   (values `(cast ,t ,expr) t))
  ((sizeof ,t)
   (values `(sizeof ,t) 'int))
  ((addressof ,[expr t])
   (values `(addressof ,expr) `(ptr int)))
  ((let ((,x* ,[e* t*]) ...) ,[expr t])
   (values
     `(begin
        ,@(map (lambda (x t e) `(let ,x ,t ,e)) x* t* e*)
        ,expr)
     t))
  ((vector-ref ,type ,[e1 t1] ,[e2 t2])
   (values `(vector-ref ,type ,e1 ,e2) type))
  ((length ,n)
   (values `(length ,n) 'int))
  ((,op ,[e1 t1] ,[e2 t2]) (guard (binop? op))
   (values `(,op ,e1 ,e2) t1))
  ((,op ,[e1 t1] ,[e2 t2]) (guard (relop? op))
   (values `(,op ,e1 ,e2) 'bool))
  ((call ,[expr t] ,[expr* t*] ...)
   (match t
     ((,argtypes -> ,rtype)
      (values `(call ,expr . ,expr*) rtype)))))

;; end library

)