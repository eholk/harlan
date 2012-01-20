(library
  (harlan middle uglify-vectors)
  (export uglify-vectors)
  (import (rnrs) (elegant-weapons helpers))

;; Uglify vectors takes our nice surface-level vector syntax and
;; converts it into an abomination of C function calls to the generic
;; vector representation library thing.
;;
;; It runs after lower-vectors but before compile-module.

(define-match uglify-vectors
  ((module ,[uglify-decl -> fn*] ...)
   `(module . ,fn*)))

(define-match uglify-decl
  ((fn ,name ,args ,t ,[uglify-stmt -> stmt])
   `(fn ,name ,args ,t ,stmt))
  ((extern ,name ,args -> ,t)
   `(extern ,name ,args -> ,t)))

(define-match extract-expr-type
  ((int ,n) 'int)
  ((var ,t ,x) t))

(define uglify-let-vec
  (lambda (t e n)
    (match e
      ((int ,y)
       (let-values (((dim t^ sz)
                     (decode-vector-type `(vec ,t ,n))))
         `(cast (vec ,t ,n)
            (call (c-expr ((int) -> (ptr void)) GC_MALLOC) ,sz))))
      ((var int ,y)
       (let-values (((dim t sz)
                     (decode-vector-type `(vec ,t ,y))))
         `(cast (vec ,t ,n)
            (call (ptr void) (c-expr GC_MALLOC) ,sz))))
      ((var ,tv ,y)
       ;; TODO: this probably needs a copy instead.
       `(var ,tv ,y))
      ;; Otherwise, just hope it works! We should use more type
      ;; information here.
      (,else else))))

(define-match (uglify-let finish)
  (() finish)
  (((,x (make-vector ,t (int ,n))) .
    ,[(uglify-let finish) -> rest])
   (let ((vv (uglify-let-vec t `(int ,n) n)))
     `(let ((,x ,vv)) ,rest)))
  (((,x ,[uglify-expr -> e]) . ,[(uglify-let finish) -> rest])
   `(let ((,x ,e)) ,rest)))

(define-match uglify-stmt
  ((let ((,x ,e) ...) ,[stmt])
   ((uglify-let stmt) `((,x ,e) ...)))
  ((begin ,[uglify-stmt -> stmt*] ...)
   (make-begin stmt*))
  ((if ,[uglify-expr -> test] ,conseq)
   `(if ,test ,conseq))
  ((if ,[uglify-expr -> test] ,conseq ,alt)
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
  ((print ,[uglify-expr -> e])
   `(print ,e))
  ((kernel ,t ,dims ,iters ,[stmt])
   `(kernel ,dims ,iters ,stmt))
  ((do ,[uglify-expr -> e])
   `(do ,e)))

(define uglify-vector-set!
  (lambda (t x i v)
    (match t
      ((vec ,t ,n)
       (let-values (((dim t sz)
                     (decode-vector-type `(vec ,t ,n))))
         `(do (call
                (c-expr (() -> void) memcpy)
                ,(uglify-vector-ref `(vec ,t ,n) x i)
                ,v
                ,sz))))
      (,scalar (guard (symbol? scalar))
        `(set! ,(uglify-vector-ref scalar x i) ,v))
      (,else (error 'uglify-vector-set!
               "unsupported vector type" else)))))

(define-match expr-type
  ((var ,t ,x) t)
  ((vector-ref ,t ,v ,i) t))

(define-match uglify-expr
  ((,t ,n) (guard (scalar-type? t)) `(,t ,n))
  ((var ,tx ,x) `(var ,tx ,x))
  ((int->float ,[e]) `(cast float ,e))
  ((call ,[name] ,[args] ...)
   `(call ,name . ,args))
  ((if ,[test] ,[conseq] ,[alt])
   `(if ,test ,conseq ,alt))
  ((,op ,[lhs] ,[rhs])
   (guard (or (binop? op) (relop? op)))
   `(,op ,lhs ,rhs))
  ((vector-ref ,t ,[e] ,[i])
   (uglify-vector-ref t e i))
  ((length ,e)
   (match (expr-type e)
     ((vec ,t ,n)
      `(int ,n))
     (,else (error 'uglify-expr "Took length of non-vector"
              else (expr-type e))))))

(define uglify-vector-ref
  (lambda (t e i)
    (match t
      ((vec ,t ,n)
       `(addressof (vector-ref ,t ,e (* ,i (int ,n)))))
      (,scalar
        (guard (symbol? scalar))
        `(vector-ref ,t ,e ,i))
      (,else (error 'uglify-vector-ref "unsupported type" else)))))
)
