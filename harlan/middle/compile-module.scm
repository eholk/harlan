(library
  (harlan middle compile-module)
  (export compile-module)
  (import
    (rnrs)
    (only (chezscheme) format)
    (util helpers)
    (harlan back print-c)
    (harlan front parser)
    (util match))

;; This compile kernel is used in the compile-module pass.
(define-match compile-kernel^
  ((kernel ,name ,args ,[compile-stmt -> stmt*] ...)
   `(kernel ,name ,args . ,stmt*)))

(define-match compile-decl
  [(fn ,name ,args (,arg-types -> ,ret-type)
     ,[compile-stmt -> stmt*] ...)
   `(func ,ret-type ,name ,(map cons arg-types args) . ,stmt*)]
  [(extern ,name ,arg-types -> ,rtype)
   `(extern ,rtype ,name ,arg-types)]
  [(gpu-module ,[compile-kernel^ -> kernel*] ...)
   `(gpu-module . ,kernel*)])

(define-match compile-stmt
  [(let ,x ,t ,[compile-expr -> e])
   `(let ,x ,t ,e)]
  [(let-gpu ,x ,t)
   `(let ,x (cl::buffer ,(scalar-type t))
         ((field g_ctx createBuffer ,(scalar-type t))
          ,(byte-size t)
          CL_MEM_READ_WRITE))]
  [(print ,[compile-expr -> expr]) `(print ,expr)]
  [(return ,[compile-expr -> expr]) `(return ,expr)]
  [(assert ,[compile-expr -> expr]) `(do (assert ,expr))]
  [(set! ,x ,e)
   (compile-set! x e)]
  [(vector-set! ,v ,i ,[compile-expr -> expr])
   `(vector-set! ,v ,i ,expr)]
  [(while (,relop ,[compile-expr -> e1] ,[compile-expr -> e2])
     ,[stmt*] ...)
   `(while (,relop ,e1 ,e2) . ,stmt*)]
  [(for (,i ,[compile-expr -> start] ,[compile-expr -> end])
     ,[stmt*] ...)
   `(for (,i ,start ,end) . ,stmt*)]
  [(do ,[compile-expr -> e] ...) `(do ,e ...)]
  [(map-gpu ((,x* ,e*) ...) ,[stmt*] ...)
   `(block
      ,@(map
          (lambda (x e)
            `(let ,x (cl::buffer_map ,(scalar-type (type-of e)))
                  ((field g_queue mapBuffer
                     ,(scalar-type (type-of e)))
                   ,(compile-expr e))))
          x* e*)
      ,stmt* ...)]
  [(block ,[stmt*] ...)
   `(block . ,stmt*)])

(define (compile-set! x e)
  (let ((lhs-t (type-of x)))
    (match lhs-t
      ((vector ,t ,n)
       `(do (memcpy ,(compile-expr x) ,(compile-expr e)
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
  ((vector ,[t] ,n) t)
  (int 'int))

(define-match byte-size
  (int `(sizeof int))
  ((vector ,[t] ,n) `(* ,n ,t)))

(define-match compile-expr
  [(int ,n) (guard (number? n)) n]
  [(u64 ,n) (guard (number? n)) n]
  [(float ,f) f]
  [(var ,t ,x) (guard (symbol? x)) x]
  [(str ,s) (guard (string? s)) s]
  [(vector-ref ,t ,[v] ,[i]) `(vector-ref ,v ,i)]
  [(field ,[obj] ,x)
   (guard (ident? x))
   `(field ,obj ,x)]
  ((field ,[obj] ,x ,t)
   (guard (ident? x))
   `(field ,obj ,x ,t))
  [(sizeof ,t) `(sizeof ,t)]
  [(deref ,[e]) `(deref ,e)]
  [(addressof ,[e]) `(addressof ,e)]
  [(cast ,t ,[e]) `(cast ,t ,e)]
  [(,op ,[e1] ,[e2]) (guard (binop? op)) `(,op ,e1 ,e2)]
  [(time) '(nanotime)]
  [(call ,t ,f ,[a*] ...)
   (guard (symbol? f))
   `(,f ,a* ...)]
  [(call ,t ,[f] ,[a*] ...) `(,f ,a* ...)])

(define-match compile-module
  [(module ,[compile-decl -> decl*] ...) decl*])


;; end library
)