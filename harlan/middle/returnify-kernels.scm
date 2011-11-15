(library
  (harlan middle returnify-kernels)
  (export returnify-kernels)
  (import (rnrs) (elegant-weapons helpers))
  
(define-match returnify-kernels
  ((module ,[returnify-kernel-decl -> fn*] ...)
   `(module . ,fn*)))

(define-match returnify-kernel-decl
  ((fn ,name ,args ,type ,[returnify-kernel-stmt -> stmt])
   `(fn ,name ,args ,type ,stmt))
  ((extern ,name ,args -> ,type)
   `(extern ,name ,args -> ,type)))

(define-match returnify-kernel-stmt
  ((print ,expr) `(print ,expr))
  ((assert ,expr) `(assert ,expr))
  ((set! ,x ,e) `(set! ,x ,e))
  ((begin ,[stmt*] ...)
   `(begin . ,stmt*))
  ((if ,test ,[conseq])
   `(if ,test ,conseq))
  ((if ,test ,[conseq] ,[alt])
   `(if ,test ,conseq ,alt))
  ((vector-set! ,t ,x ,e1 ,e2) `(vector-set! ,t ,x ,e1 ,e2))
  ((return ,expr) `(return ,expr))
  ((while ,expr ,[body])
   `(while ,expr ,body))
  ((for (,x ,e1 ,e2) ,[body])
   `(for (,x ,e1 ,e2) ,body))
  ((let ((,id ,e) ...) ,[stmt])
   ((returnify-kernel-let stmt) `((,id ,e) ...)))
  ((do ,expr) `(do ,expr)))

(define-match returnify-kernel-expr
  ((begin ,[returnify-kernel-stmt -> stmt*] ,[expr])
   `(begin ,@stmt* ,expr))
  ((let ((,id ,e) ...) ,[expr])
   ((returnify-kernel-let expr) `((,id ,e) ...)))
  (,else else))

(define-match type-dim
  ((vector ,[t] ,n) (+ 1 t))
  (,x 0))

(define-match (returnify-kernel-let finish)
  (() finish)
  (((,id (kernel void ,arg* ,body))
    . ,[(returnify-kernel-let finish) -> rest])
   ;; TODO: we still need to traverse the body
   `(let ((,id (kernel void ,arg* ,body))) ,rest))
  (((,id (kernel (vector ,t ,n) ,arg* ,body))
    . ,[(returnify-kernel-let finish) -> rest])
   (match arg*
     ((((,x* ,tx*) (,xe* ,xet*)) ...)
      (let ((retvar (gensym 'retval)))
        `(let ((,id (make-vector ,t (int ,n))))
           (begin
             (kernel (vector ,t ,n)
               (((,retvar ,t)
                 ((var (vector ,t ,n) ,id) (vector ,t ,n)))
                . ,arg*)
               ,((set-retval t retvar) body))
             ,rest))))))
  (((,id ,expr) . ,[(returnify-kernel-let finish) -> rest])
   `(let ((,id ,expr)) ,rest)))

(define-match (set-retval t retvar)
  ((begin ,stmt* ... ,[(set-retval t retvar) -> expr])
   `(begin ,@stmt* ,expr))
  ((let ,b ,[(set-retval t retvar) -> expr])
   `(let ,b ,expr))
  (,else `(set! (var ,t ,retvar) ,else)))

;; end library
)

