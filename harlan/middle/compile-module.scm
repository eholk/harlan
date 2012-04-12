(library
  (harlan middle compile-module)
  (export compile-module)
  (import (rnrs) (elegant-weapons helpers)
    (harlan helpers))

(define-match compile-kernel^
  ((kernel ,name ,args ,[compile-stmt -> stmt*] ...)
   `(kernel ,name ,args . ,stmt*)))

(define-match compile-decl
  [(fn ,name ,args (,arg-types -> ,ret-type)
     ,[compile-stmt -> stmt*] ...)
   `(func ,ret-type ,name ,(map list args arg-types) . ,stmt*)]
  [(extern ,name ,arg-types -> ,rtype)
   `(extern ,rtype ,name ,arg-types)]
  [(global ,type ,name ,[compile-expr -> e])
   `(global ,type ,name ,e)]
  [(gpu-module ,[compile-kernel^ -> kernel*] ...)
   `(gpu-module . ,kernel*)])

(define-match compile-stmt
  ((begin ,[stmt*] ...)
   `(begin . ,stmt*))
  [(let ,x ,t ,[compile-expr -> e])
   `(let ,x ,t ,e)]
  ((if ,[compile-expr -> test] ,[conseq])
   `(if ,test ,conseq))
  ((if ,[compile-expr -> test] ,[conseq] ,[alt])
   `(if ,test ,conseq ,alt))
  [(print ,[compile-expr -> expr]) `(print ,expr)]
  ((return) `(return))
  [(return ,[compile-expr -> expr]) `(return ,expr)]
  [(assert ,[compile-expr -> expr]) `(do (assert ,expr))]
  [(set! ,x ,e)
   (compile-set! x e)]
  [(vector-set! ,v ,i ,[compile-expr -> expr])
   `(vector-set! ,v ,i ,expr)]
  [(while ,[compile-expr -> expr] ,[stmt])
   `(while ,expr ,stmt)]
  [(for (,i ,[compile-expr -> start] ,[compile-expr -> end])
     ,[stmt*] ...)
   `(for (,i ,start ,end) . ,stmt*)]
  [(do ,[compile-expr -> e]) `(do ,e)])

(define (compile-set! x e)
  `(set! ,(compile-expr x) ,(compile-expr e)))

(define-match scalar-type
  ;; TODO:
  ;; converting the types should happen after this, so hopefully
  ;; we won't need pointers.
  ((ptr ,[t]) t)
  ((vec ,n ,[t]) t)
  (bool 'bool)
  (char 'char)
  (int 'int)
  (float 'float)
  )

(define-match byte-size
  (int `(sizeof int))
  (bool `(sizeof bool))
  (char `(sizeof char))
  (float `(sizeof float))
  ((vec ,n ,[t]) `(* (int ,n) ,t)))

(define-match compile-expr
  [(,t ,n) (guard (scalar-type? t)) `(,t ,n)]
  [(var ,t ,x) `(var ,x)]
  [(c-expr ,t ,x) `(c-expr ,t ,x)]
  [(vector-ref ,t ,[v] ,[i]) `(vector-ref ,v ,i)]
  ((if ,[test] ,[conseq] ,[alt])
   `(if ,test ,conseq ,alt))
  [(field (var ,t ,obj) ,x) `(field ,obj ,x)]
  ((field (var ,t ,obj) ,x ,t) `(field ,obj ,x ,t))
  [(sizeof ,t) `(sizeof ,t)]
  [(deref ,[e]) `(deref ,e)]
  [(addressof ,[e]) `(addressof ,e)]
  [(cast ,t ,[e]) `(cast ,t ,e)]
  [(,op ,[e1] ,[e2]) (guard (or (binop? op) (relop? op)))
   `(,op ,e1 ,e2)]
  [(time) '(nanotime)]
  [(call ,[f] ,[a*] ...) `(call ,f ,a* ...)])

(define-match compile-module
  [(module ,[compile-decl -> decl*] ...)
   `((include "harlan.hpp") . ,decl*)])

;; end library
)