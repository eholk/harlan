(library
  (harlan middle compile-module)
  (export compile-module)
  (import (rnrs) (elegant-weapons helpers))

;; This compile kernel is used in the compile-module pass.
(define-match compile-kernel^
  ((kernel ,name ,args ,[compile-stmt -> stmt*] ...)
   `(kernel ,name ,args . ,stmt*)))

(define-match compile-decl
  [(fn ,name ,args (,arg-types -> ,ret-type)
     ,[compile-stmt -> stmt*] ...)
   `(func ,ret-type ,name ,(map list args arg-types) . ,stmt*)]
  [(extern ,name ,arg-types -> ,rtype)
   `(extern ,rtype ,name ,arg-types)]
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
  [(let-gpu ,x ,t)
   `(let ,x (cl::buffer ,(scalar-type t))
         (call
           (field g_ctx createBuffer ,(scalar-type t))
           ,(byte-size t)
           (c-expr void CL_MEM_READ_WRITE)))]
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
  [(do ,[compile-expr -> e]) `(do ,e)]
  [(map-gpu ((,x* ,e*) ...) ,[stmt*] ...)
   `(begin
      ,@(map
          (lambda (x e)
            `(let ,x (cl::buffer_map ,(scalar-type (type-of e)))
                  (call
                    (field g_queue mapBuffer
                      ,(scalar-type (type-of e)))
                    ,(compile-expr e))))
          x* e*)
      ,stmt* ...)])

(define (compile-set! x e)
  (let ((lhs-t (type-of x)))
    (match lhs-t
      ((vec ,t ,n)
       `(do (call
              (c-expr (() -> void) memcpy)
              ,(compile-expr x)
              ,(compile-expr e)
              ,(byte-size lhs-t))))
      (,scalar
        (guard (symbol? scalar))
        `(set! ,(compile-expr x) ,(compile-expr e)))
      (,e (error 'compile-set! "Unknown target type" e)))))

(define-match scalar-type
  ;; TODO:
  ;; converting the types should happen after this, so hopefully
  ;; we won't need pointers.
  ((ptr ,[t]) t)
  ((vec ,[t] ,n) t)
  (int 'int))

(define-match byte-size
  (int `(sizeof int))
  ((vec ,[t] ,n) `(* (int ,n) ,t)))

(define-match compile-expr
  [(int ,n) `(int ,n)]
  [(u64 ,n) `(u64 ,n)]
  [(float ,f) `(float ,f)]
  [(var ,t ,x) `(var ,x)]
  [(str ,s) `(str ,s)]
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