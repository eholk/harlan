(library
  (harlan middle make-vector-refs-explicit)
  (export make-vector-refs-explicit)
  (import (rnrs) (elegant-weapons helpers))

(define-match make-vector-refs-explicit
  ((module ,[explicify-decl -> fn*] ...)
   `(module . ,fn*)))

(define-match explicify-decl
  ((fn ,name ,args ,t ,[explicify-stmt -> stmt])
   `(fn ,name ,args ,t ,stmt))
  (,else else))

(define-match explicify-stmt
  ((let ((,x ,t ,[explicify-expr -> e]) ...) ,[stmt])
   `(let ((,x ,t ,e) ...) ,stmt))
  ((begin ,[stmt*] ...)
   (make-begin stmt*))
  ((kernel ,t ,dims (((,x* ,t*)
                      (,[explicify-expr -> xs*] ,ts*)
                      , d*) ...)
     ,[stmt])
   ;; This map is important because kernel arguments can depend on
   ;; each other; don't take it out as appealing as that may be!
   (let ((xs* (map (replace-vec-refs-expr x*) xs*)))
     `(kernel ,t ,dims
              ,(generate-kernel x* t* xs* d* stmt))))
  ((error ,x) `(error ,x))
  ((print ,[explicify-expr -> expr] ...)
   `(print . ,expr))
  ((assert ,[explicify-expr -> expr])
   `(assert ,expr))
  ((set! ,[explicify-expr -> x]
         ,[explicify-expr -> i])
   `(set! ,x ,i))
  ((if ,[explicify-expr -> test] ,[conseq])
   `(if ,test ,conseq))
  ((if ,[explicify-expr -> test] ,[conseq] ,[alt])
   `(if ,test ,conseq ,alt))
  ((while ,[explicify-expr -> expr] ,[body])
   `(while ,expr ,body))
  ((for (,x ,[explicify-expr -> start]
            ,[explicify-expr -> end]
            ,[explicify-expr -> step])
        ,[body])
   `(for (,x ,start ,end ,step) ,body))
  ((return) `(return))
  ((return ,[explicify-expr -> expr]) `(return ,expr))
  ((do ,[explicify-expr -> expr]) `(do ,expr)))

(define-match explicify-expr
  ((begin ,[explicify-stmt -> stmt*] ... ,[expr])
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
  ((,op ,[lhs] ,[rhs])
   (guard (or (binop? op) (relop? op)))
   `(,op ,lhs ,rhs))
  ((c-expr ,t ,x) `(c-expr ,t ,x))
  ((call ,[fn] ,[arg*] ...) `(call ,fn . ,arg*)))

;; This enforces the let* semantics of kernel arguments.
(define (generate-kernel-args x t xs d prev)
  `(let ((,x
          (ptr ,t)
          (addressof
           (vector-ref ,t ,xs
            (call
             (c-expr ((int) -> int) get_global_id)
             (int ,d))))))
     ,prev))

(define generate-kernel
  (lambda (x* t* xs* d* stmt)
    (fold-right
     generate-kernel-args
     ((replace-vec-refs-stmt x*) stmt)
     x* t* xs* d*)))

(define-match (replace-vec-refs-stmt x*)
  ((let ((,x ,t ,[(replace-vec-refs-expr x*) -> e]) ...)
     ,[(replace-vec-refs-stmt x*) -> stmt])
   `(let ((,x ,t ,e) ...) ,stmt))
  ((begin ,[(replace-vec-refs-stmt x*) -> stmt*] ...)
   (make-begin stmt*))
  ((print ,[(replace-vec-refs-expr x*) -> expr])
   `(print ,expr))      
  ((assert ,[(replace-vec-refs-expr x*) -> expr])
   `(assert ,expr))
  ((set! ,[(replace-vec-refs-expr x*) -> x]
     ,[(replace-vec-refs-expr x*) -> expr])
   `(set! ,x ,expr))
  ((kernel ,t (,[(replace-vec-refs-expr x*) -> dims] ...) ,[body])
   `(kernel ,t ,dims ,body))
  ((if ,[(replace-vec-refs-expr x*) -> test]
       ,[(replace-vec-refs-stmt x*) -> conseq])
   `(if ,test ,conseq))
  ((if ,[(replace-vec-refs-expr x*) -> test]
       ,[(replace-vec-refs-stmt x*) -> conseq]
       ,[(replace-vec-refs-stmt x*) -> alt])
   `(if ,test ,conseq ,alt))
  ((while ,[(replace-vec-refs-expr x*) -> expr]
     ,[(replace-vec-refs-stmt x*) -> body])
   `(while ,expr ,body))
  ((for (,x ,[(replace-vec-refs-expr x*) -> start]
            ,[(replace-vec-refs-expr x*) -> end]
            ,[(replace-vec-refs-expr x*) -> step])
     ,[(replace-vec-refs-stmt x*) -> body])
   `(for (,x ,start ,end ,step) ,body))
  ((return) `(return))
  ((return ,[(replace-vec-refs-expr x*) -> expr])
   `(return ,expr))
  ((do ,[(replace-vec-refs-expr x*) -> expr]) `(do ,expr)))

(define-match (replace-vec-refs-expr x*)
  ((char ,c) `(char ,c))
  ((int ,i) `(int ,i))
  ((u64 ,u) `(u64 ,u))
  ((float ,f) `(float ,f))
  ((bool ,b) `(bool ,b))
  ((int->float ,[(replace-vec-refs-expr x*) -> e])
   `(int->float ,e))
  ((deref ,[e]) `(deref ,e))
  ((addressof ,[e]) `(addressof ,e))
  ((str ,s) `(str ,s))
  ((var ,t ,x)
   (if (memq x x*) `(deref (var ,t ,x)) `(var ,t ,x)))
  ((let ((,x ,t ,[(replace-vec-refs-expr x*) -> e]) ...)
     ,[expr])
   `(let ((,x ,t ,e) ...) ,expr))
  ((begin ,[(replace-vec-refs-stmt x*) -> stmt*] ...
          ,[expr])
   (make-begin `(,@stmt* ,expr)))
  ((make-vector ,t ,[(replace-vec-refs-expr x*) -> triv*] ...)
   `(make-vector ,t . ,triv*))
  ((vector ,t ,[triv*] ...)
   `(vector ,t . ,triv*))
  ((if ,[(replace-vec-refs-expr x*) -> t]
       ,[(replace-vec-refs-expr x*) -> c]
       ,[(replace-vec-refs-expr x*) -> a])
   `(if ,t ,c ,a))
  ((call ,[(replace-vec-refs-expr x*) -> v]
         ,[(replace-vec-refs-expr x*) -> arg*] ...)
   `(call ,v . ,arg*))
  ((vector-ref ,t ,[(replace-vec-refs-expr x*) -> v]
               ,[(replace-vec-refs-expr x*) -> i])
   `(vector-ref ,t ,v ,i))
  ((length ,[(replace-vec-refs-expr x*) -> e])
   `(length ,e))
  ((c-expr ,t ,v) `(c-expr ,t ,v))
  ((,op ,[(replace-vec-refs-expr x*) -> e1]
        ,[(replace-vec-refs-expr x*) -> e2])
   (guard (or (binop? op) (relop? op)))
   `(,op ,e1 ,e2)))

)
