(library
  (harlan middle remove-nested-kernels)
  (export remove-nested-kernels)
  (import (rnrs) (elegant-weapons helpers)
    (harlan helpers))

;; This pass takes a nest of kernels and turns all but the innermost
;; one into for loops. This isn't the best way to do this, but it's
;; the easiest way to support nested kernels and will give us
;; something to build off of.

(define-match remove-nested-kernels
  ((module ,[Decl -> decl*] ...)
   `(module . ,decl*)))

(define-match Decl
  ((fn ,name ,args ,type ,[Stmt -> stmt _])
   `(fn ,name ,args ,type ,stmt))
  ((extern . ,rest)
   `(extern . ,rest)))

(define (any? ls)
  (if (null? ls)
      #f
      (or (car ls) (any? (cdr ls)))))

(define-match (Let finish k?)
  (() (values finish k?))
  (((,x ,xt
     (kernel (vec ,n ,t) ,dims (((,x* ,t*) (,xs* ,ts*) ,d*) ...)
       ,[Expr -> e kernel?]))
    . ,[rest _])
   (values
     (if kernel?
         (let ((i (gensym 'i)))
           (assert (= (length dims) 1))
           `(let ((,x ,xt (make-vector ,t (int ,n))))
              (begin
                (for (,i (int 0) ,(car dims))
                  (let (,@(map (lambda (x t xs)
                                 `(,x ,t (vector-ref ,t ,xs (var int ,i))))
                            x* t* xs*))
                    ,((set-kernel-return
                        (lambda (e) `(vector-set!
                                  ,t (var (vec ,n ,t) ,x) (var int ,i) ,e)))
                      e)))
                ,rest)))
         `(let ((,x ,xt
                  (kernel (vec ,n ,t) ,dims
                    (((,x* ,t*) (,xs* ,ts*) ,d*) ...)
                    ,e)))
            ,rest))
     #t))
  (((,x ,t ,e) . ,[rest k?])
   (values `(let ((,x ,t ,e)) ,rest) k?)))

(define-match Stmt
  ((let ((,x ,t ,e) ...) ,[stmt k?])
   ((Let stmt k?) `((,x ,t ,e) ...)))
  ((begin ,[stmt* has-kernel*] ...)
   (values (make-begin stmt*) (any? has-kernel*)))
  ((for (,i ,start ,end) ,[stmt has-kernel])
   (values `(for (,i ,start ,end) ,stmt) has-kernel))
  ((while ,t ,[stmt has-kernel])
   (values `(while ,t ,stmt) has-kernel))
  ((if ,test ,[conseq chas-kernel])
   (values `(if ,test ,conseq) chas-kernel))
  ((if ,test ,[conseq chas-kernel] ,[alt ahas-kernel])
   (values `(if ,test ,conseq ,alt)
     (or chas-kernel ahas-kernel)))
  ((set! ,lhs ,rhs)
   (values `(set! ,lhs ,rhs) #f))
  ((vector-set! ,t ,v ,i ,e)
   (values `(vector-set! ,t ,v ,i ,e) #f))
  ((do ,e) (values `(do ,e) #f))
  ((print . ,e*) (values `(print . ,e*) #f))
  ((assert ,e) (values `(assert ,e) #f))
  ((return) (values `(return) #f))
  ((return ,e) (values `(return ,e) #f)))

(define-match Expr
  ((let ((,x ,t ,e) ...) ,[expr k?])
   ((Let expr k?) `((,x ,t ,e) ...)))
  ((begin ,[Stmt -> stmt* kernel*] ... ,[e has-kernel])
   (values `(begin ,@stmt* ,e)
     (or has-kernel (any? kernel*))))
  (,else (values else #f)))

(define-match (set-kernel-return finish)
  ((begin ,stmt* ... ,[(set-kernel-return finish) -> expr])
   `(begin ,@stmt* ,expr))
  ((let ,b ,[(set-kernel-return finish) -> expr])
   `(let ,b ,expr))
  (,else (finish else)))

;;end library
)
