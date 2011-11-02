(library
  (harlan middle uglify-vectors)
  (export
    uglify-vectors
    uglify-vector-ref)
  (import
    (only (chezscheme) format)
    (rnrs)
    (util match)
    (harlan back print-c)
    (util verify-grammar)
    (util helpers))

;; Uglify vectors takes our nice surface-level vector syntax and
;; converts it into an abomination of C function calls to the generic
;; vector representation library thing.
;;
;; It runs after lower-vectors but before compile-module.

(define-match uglify-vectors
  ((module ,[uglify-decl -> fn*] ...)
   `(module . ,fn*)))

(define-match uglify-decl
  ((fn ,name ,args ,t ,[uglify-stmt -> stmt*] ...)
   `(fn ,name ,args ,t . ,(apply append stmt*)))
  ((extern ,name ,args -> ,t)
   `(extern ,name ,args -> ,t)))

(define-match extract-expr-type
  ((int ,n) 'int)
  ((var ,t ,x) t)
  ;; This next case is sort of a hack
  ;; But it works without said hack?
  ;;(,n (guard (integer? n)) 'int)
  )

(define uglify-let-vec
  (lambda (t e n)
    (match e
      ((int ,y)
       (let-values (((dim t sz)
                     (decode-vector-type `(vector ,t ,n))))
         `(cast (vector ,t ,n) (call (ptr void) GC_MALLOC ,sz))))
      ((var int ,y)
       (let-values (((dim t sz) (decode-vector-type `(vector ,t ,y))))
         `(cast (vector ,t ,n) (call (ptr void) GC_MALLOC ,sz))))
      ((var ,tv ,y)
       ;; TODO: this probably needs a copy instead.
       `(var ,tv ,y))
      ;; Otherwise, just hope it works! We should use more type
      ;; information here.
      (,else else))))

(define-match uglify-stmt
  ((begin ,[uglify-stmt -> stmt*] ...)
   `((begin ,(apply append stmt*))))
  ((let ,x (vector ,t ,n) ,[uglify-expr -> init])
   (let ((vv (uglify-let-vec t init n)))
     `((let ,x (vector ,t ,n) ,vv))))
  ((let ,x ,t ,[uglify-expr -> e])
   `((let ,x ,t ,e)))
  ((if ,[uglify-expr -> test] ,conseq)
   `((if ,test ,conseq)))
  ((if ,[uglify-expr -> test] ,conseq ,alt)
   `((if ,test ,conseq ,alt)))
  ((while ,[uglify-expr -> e] ,[uglify-stmt -> stmt*] ...)
   `((while ,e ,(apply append stmt*) ...)))
  ((for (,i ,[uglify-expr -> start] ,[uglify-expr -> end])
     ,[uglify-stmt -> stmt*] ...)
   `((for (,i ,start ,end) . ,(apply append stmt*))))
  ((set! ,[uglify-expr -> lhs] ,[uglify-expr -> rhs])
   `((set! ,lhs ,rhs)))
  ((return ,[uglify-expr -> e])
   `((return ,e)))
  ((assert ,[uglify-expr -> e])
   `((assert ,e)))
  ((vector-set! ,t ,[uglify-expr -> x] ,[uglify-expr -> i]
     ,[uglify-expr -> v])
   (uglify-vector-set! t x i v))
  ((print ,[uglify-expr -> e])
   `((print ,e)))
  ((kernel ,t ,iters ,[stmt*] ...)
   `((kernel ,iters ,(apply append stmt*) ...)))
  ((do ,[uglify-expr -> e])
   `((do ,e))))

(define uglify-vector-set!
  (lambda (t x i v)
    (match t
      ((vector ,t ,n)
       (let-values (((dim t sz)
                     (decode-vector-type `(vector ,t ,n))))
         `((do (call void memcpy 
                 ,(uglify-vector-ref `(vector ,t ,n) x i)
                 ,v
                 ,sz)))))
      (,scalar (guard (symbol? scalar))
        `((set! ,(uglify-vector-ref scalar x i) ,v)))
      (,else (error 'uglify-vector-set!
               "unsupported vector type" else)))))

(define-match expr-type
  ((var ,t ,x) t)
  ((vector-ref ,t ,v ,i) t))

(define-match uglify-expr
  ((int ,n) `(int ,n))
  ((u64 ,n) `(u64 ,n))
  ((float ,f) `(float ,f))
  ((str ,s) `(str ,s))
  ((var ,tx ,x) `(var ,tx ,x))
  ((int->float ,[e]) `(cast float ,e))
  ((call ,t ,name ,[args] ...)
   `(call ,t ,name . ,args))
  ((if ,[test] ,[conseq] ,[alt])
   `(if ,test ,conseq ,alt))
  ((,op ,[lhs] ,[rhs]) (guard (or (binop? op) (relop? op)))
   `(,op ,lhs ,rhs))
  ((vector-ref ,t ,[e] ,[i])
   (uglify-vector-ref t e i))
  ((length ,e)
   (match (expr-type e)
     ((vector ,t ,n)
      `(int ,n))
     (,else (error 'uglify-expr "Took length of non-vector"
              else (expr-type e))))))

(define uglify-vector-ref
  (lambda (t e i)
    (match t
      ((vector ,t ,n)
       `(addressof (vector-ref ,t ,e (* ,i (int ,n)))))
      (,scalar
        (guard (symbol? scalar))
        `(vector-ref ,t ,e ,i))
      (,else (error 'uglify-vector-ref "unsupported type" else)))))
)
