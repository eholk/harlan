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
  ((print ,[returnify-kernel-expr -> expr*] ...)
   `(print . ,expr*))
  ((assert ,[returnify-kernel-expr -> expr])
   `(assert ,expr))
  ((set! ,[returnify-kernel-expr -> x]
         ,[returnify-kernel-expr -> e])
   `(set! ,x ,e))
  ((error ,x) `(error ,x))
  ((begin ,[stmt*] ...)
   `(begin . ,stmt*))
  ((if ,[returnify-kernel-expr -> test] ,[conseq])
   `(if ,test ,conseq))
  ((if ,[returnify-kernel-expr -> test] ,[conseq] ,[alt])
   `(if ,test ,conseq ,alt))
  ((return) `(return))
  ((return ,[returnify-kernel-expr -> expr])
   `(return ,expr))
  ((while ,[returnify-kernel-expr -> expr] ,[body])
   `(while ,expr ,body))
  ((for (,i ,[returnify-kernel-expr -> start]
            ,[returnify-kernel-expr -> stop]
            ,[returnify-kernel-expr -> step])
        ,[body])
   `(for (,i ,start ,stop ,step) ,body))
  ((let ((,x ,t ,[returnify-kernel-expr -> e]) ...) ,[stmt])
   `(let ((,x ,t ,e) ...) ,stmt))
  ((let-region (,r) ,[body]) `(let-region (,r) ,body))
  ((do ,[returnify-kernel-expr -> expr]) `(do ,expr)))

(define-match returnify-kernel-expr
  ((begin ,[returnify-kernel-stmt -> stmt*] ... ,[expr])
   `(begin ,@stmt* ,expr))
  ((let ((,x ,t ,[e]) ...) ,[expr])
   `(let ((,x ,t ,e) ...) ,expr))
  ((,t ,x) (guard (scalar-type? t)) `(,t ,x))
  ((var ,t ,x) `(var ,t ,x))
  ((if ,[t] ,[c] ,[a])
   `(if ,t ,c ,a))
  ((vector-ref ,t ,[v] ,[i])
   `(vector-ref ,t ,v ,i))
  ((length ,[e]) `(length ,e))
  ((int->float ,[e]) `(int->float ,e))
  ((make-vector ,t ,[e]) `(make-vector ,t ,e))
  ((vector ,t ,[e*] ...) `(vector ,t . ,e*))
  ((vector-r ,t ,r ,[e] ...)
   `(vector-r ,t ,r . ,e))
  ((,op ,[lhs] ,[rhs])
   (guard (or (binop? op) (relop? op)))
   `(,op ,lhs ,rhs))
  ((c-expr ,t ,x) `(c-expr ,t ,x))
  ((call ,[fn] ,[arg*] ...) `(call ,fn . ,arg*))
  ((kernel . ,body*)
   (returnify-kernel `(kernel . ,body*))))

(define-match returnify-kernel
  ((kernel (vec ,t)
           ,dims
           (((,x* ,tx*) (,[returnify-kernel-expr -> xe*] ,xet*) ,dim) ...)
           ,body)
   (let ((retvars (map (lambda (_) (gensym 'retval)) dims))
         (i (gensym 'i))
         (vv (gensym 'vv))
         (id (gensym 'kern)))
     `(let ((,id (vec ,t) (make-vector ,t ,(car dims))))
        (begin
          ,@(if (null? (cdr dims))
                `()
                `((for (,i (int 0) ,(car dims) (int 1))
                       (let ((,vv ,t (make-vector ,(cadr t) ,(cadr dims))))
                         (set! (vector-ref ,t (var (vec ,t) ,id) (var int ,i))
                               (var ,t ,vv))))))
          (kernel
           (vec ,t)
           ,dims
           ,(insert-retvars retvars (cons id retvars) 0 t `(((,x* ,tx*) (,xe* ,xet*) ,dim) ...))
           ,((set-retval (shave-type (length dims) `(vec ,t))
                         (car (reverse retvars)))
             body))
          (var (vec ,t) ,id)))))
  )

;; This is stupid
(define (shave-type dim t)
  (if (zero? dim) t (shave-type (- dim 1) (cadr t))))

;; This is the stupidest procedure I've ever written
(define (insert-retvars retvars sources dim t arg*)
  (match arg*
    (() (guard (null? retvars)) `())
    (()
     (cons
      `((,(car retvars) ,t)
        ((var (vec ,t) ,(car sources))
         (vec ,t))
        ,dim)
      (if (null? (cdr retvars))
          `()
          (insert-retvars (cdr retvars)
                          (cdr sources)
                          (+ dim 1)
                          (cadr t)
                          arg*))))
    ((((,x ,tx) (,xs ,ts) ,d) . ,rest)
     (if (<= dim d)
         (cons
          `((,(car retvars) ,t)
            ((var (vec ,t) ,(car sources))
             (vec ,t))
            ,dim)
          (if (null? (cdr retvars))
              arg*
              (insert-retvars (cdr retvars)
                              (cdr sources)
                              (+ dim 1)
                              (cadr t)
                              arg*)))
         (cons (car arg*)
               (insert-retvars retvars
                               sources
                               dim
                               t
                               (cdr arg*)))))))

(define-match (set-retval t retvar)
  ((begin ,stmt* ... ,[(set-retval t retvar) -> expr])
   `(begin ,@stmt* ,expr))
  ((let ,b ,[(set-retval t retvar) -> expr])
   `(let ,b ,expr))
  (,else `(set! (var ,t ,retvar) ,else)))

;; end library
)

