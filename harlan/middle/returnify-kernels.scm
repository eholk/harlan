(library
  (harlan middle returnify-kernels)
  (export returnify-kernels)
  (import (rnrs) (elegant-weapons helpers)
    (harlan helpers))
  
(define-match returnify-kernels
  ((module ,[returnify-kernel-decl -> fn*] ...)
   `(module . ,fn*)))

(define-match returnify-kernel-decl
  ((fn ,name ,args ,type ,[returnify-kernel-stmt -> stmt])
   `(fn ,name ,args ,type ,stmt))
  ((extern ,name ,args -> ,type)
   `(extern ,name ,args -> ,type)))

(define-match returnify-kernel-stmt
  ((print . ,expr*) `(print . ,expr*))
  ((assert ,expr) `(assert ,expr))
  ((set! ,x ,e) `(set! ,x ,e))
  ((error ,x) `(error ,x))
  ((begin ,[stmt*] ...)
   `(begin . ,stmt*))
  ((if ,test ,[conseq])
   `(if ,test ,conseq))
  ((if ,test ,[conseq] ,[alt])
   `(if ,test ,conseq ,alt))
  ((vector-set! ,t ,x ,e1 ,e2) `(vector-set! ,t ,x ,e1 ,e2))
  ((return) `(return))
  ((return ,expr) `(return ,expr))
  ((while ,expr ,[body])
   `(while ,expr ,body))
  ((for ,b ,[body])
   `(for ,b ,body))
  ((let ((,id ,t ,e) ...) ,[stmt])
   ((returnify-kernel-let stmt) `((,id ,t ,e) ...)))
  ((do ,expr) `(do ,expr)))

(define-match returnify-kernel-expr
  ((begin ,[returnify-kernel-stmt -> stmt*] ,[expr])
   `(begin ,@stmt* ,expr))
  ((let ((,id ,t ,e) ...) ,[expr])
   ((returnify-kernel-let expr) `((,id ,t ,e) ...)))
  (,else else))

(define-match (returnify-kernel-let finish)
  (() finish)
  (((,id ,xt (kernel void ,arg* ,body))
    . ,[(returnify-kernel-let finish) -> rest])
   ;; TODO: we still need to traverse the body
   `(let ((,id ,xt (kernel void ,arg* ,body))) ,rest))
  (((,id ,xt (kernel (vec ,t) ,dims ,arg* ,body))
    . ,[(returnify-kernel-let finish) -> rest])
   (match arg*
     ((((,x* ,tx*) (,xe* ,xet*) ,dim) ...)
      (let ((retvar (gensym 'retval)))
        `(let ((,id ,xt (make-vector ,t ,(car dims))))
           (begin
             (kernel (vec ,t) ,dims
               (((,retvar ,t)
                 ((var (vec ,t) ,id) (vec ,t)) 0)
                . ,arg*)
               ,((set-retval t retvar) body))
             ,rest))))))
  (((,id ,t ,expr) . ,[(returnify-kernel-let finish) -> rest])
   `(let ((,id ,t ,expr)) ,rest)))

(define-match (set-retval t retvar)
  ((begin ,stmt* ... ,[(set-retval t retvar) -> expr])
   `(begin ,@stmt* ,expr))
  ((let ,b ,[(set-retval t retvar) -> expr])
   `(let ,b ,expr))
  (,else `(set! (var ,t ,retvar) ,else)))

;; end library
)

