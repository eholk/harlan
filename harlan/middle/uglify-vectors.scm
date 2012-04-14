(library
  (harlan middle uglify-vectors)
  (export uglify-vectors)
  (import (rnrs) (elegant-weapons helpers)
    (harlan helpers))

  (define length-offset '(sizeof int))
  
(define-match uglify-vectors
  ((module ,[uglify-decl -> fn*] ...)
   `(module
      (global g_region (ptr region)
        (call
         (c-expr ((int) -> (ptr region))
           create_region)
         ;; 16MB
         (int 16777216))) . ,fn*)))

(define-match uglify-decl
  ((fn ,name ,args ,t ,[uglify-stmt -> stmt])
   `(fn ,name ,args ,t ,stmt))
  ((extern ,name ,args -> ,t)
   `(extern ,name ,args -> ,t)))

(define (uglify-let-vec t n)
  (let ((t (if (scalar-type? t) t `region_ptr)))
    `(alloc
       (var (ptr region) g_region)
       (+ (* (sizeof ,t) ,n)
         ;; sizeof int for the length field.
         ,length-offset))))

(define (vector-length-field e)
  `(deref (region-ref (ptr int) (var (ptr region) g_region) ,e)))
  
(define-match (uglify-let finish)
  (() finish)
  (((,x ,xt (make-vector ,t ,[uglify-expr -> n])) .
    ,[(uglify-let finish) -> rest])
   (let* ((length (gensym (symbol-append x '_length)))
          (vv (uglify-let-vec t `(var int ,length))))
     `(let ((,length int ,n))
        (let ((,x ,xt ,vv))
          ,(make-begin
            `((set! ,(vector-length-field `(var ,xt ,x)) (var int ,length))
              ,rest))))))
  (((,x ,t ,[uglify-expr -> e])
    . ,[(uglify-let finish) -> rest])
   `(let ((,x ,t ,e)) ,rest)))

(define-match uglify-stmt
  ((let ((,x ,xt ,e) ...) ,[stmt])
   ((uglify-let stmt) `((,x ,xt ,e) ...)))
  ((begin ,[uglify-stmt -> stmt*] ...)
   (make-begin stmt*))
  ((if ,[uglify-expr -> test] ,[conseq])
   `(if ,test ,conseq))
  ((if ,[uglify-expr -> test] ,[conseq] ,[alt])
   `(if ,test ,conseq ,alt))
  ((while ,[uglify-expr -> e] ,[uglify-stmt -> stmt])
   `(while ,e ,stmt))
  ((for (,i ,[uglify-expr -> start] ,[uglify-expr -> end])
     ,[uglify-stmt -> stmt])
   `(for (,i ,start ,end) ,stmt))
  ((set! ,[uglify-expr -> lhs] ,[uglify-expr -> rhs])
   `(set! ,lhs ,rhs))
  ((return) `(return))
  ((return ,[uglify-expr -> e])
   `(return ,e))
  ((assert ,[uglify-expr -> e])
   `(assert ,e))
  ((vector-set! ,t ,[uglify-expr -> x] ,[uglify-expr -> i]
     ,[uglify-expr -> v])
   (uglify-vector-set! t x i v))
  ((print ,[uglify-expr -> e] ...)
   `(print . ,e))
  ((kernel ,t (,[uglify-expr -> dims] ...) ,iters ,[stmt])
   `(kernel ,dims ,iters ,stmt))
  ((do ,[uglify-expr -> e])
   `(do ,e)))

(define uglify-vector-set!
  (lambda (t x i v)
    `(set! ,(uglify-vector-ref t x i) ,v)))

(define-match expr-type
  ((var ,t ,x) t)
  ((vector-ref ,t ,v ,i) t))

(define-match uglify-expr
  ((,t ,n) (guard (scalar-type? t)) `(,t ,n))
  ((var ,tx ,x) `(var ,tx ,x))
  ((int->float ,[e]) `(cast float ,e))
  ((call ,[name] ,[args] ...)
   `(call ,name . ,args))
  ((c-expr ,t ,name)
   `(c-expr ,t ,name))
  ((if ,[test] ,[conseq] ,[alt])
   `(if ,test ,conseq ,alt))
  ((,op ,[lhs] ,[rhs])
   (guard (or (binop? op) (relop? op)))
   `(,op ,lhs ,rhs))
  ((vector-ref ,t ,[e] ,[i])
   (uglify-vector-ref t e i))
  ((length ,[e])
   (vector-length-field e))
  ((addressof ,[expr])
   `(addressof ,expr))
  ((deref ,[expr])
   `(deref ,expr)))

(define uglify-vector-ref
  (lambda (t e i)
    `(vector-ref
      ,t
      (region-ref
        (ptr ,t)
        (var (ptr region) g_region)
        (+ ,e ,length-offset))
      ,i)))

)
