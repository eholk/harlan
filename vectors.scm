(library
  (vectors)
  (export
    uglify-vectors
    vector-bytesize
    uglify-vector-ref)
  (import
    (only (chezscheme) format)
    (rnrs)
    (util match)
    (print-c)
    (verify-grammar)
    (util helpers))

;; Uglify vectors takes our nice surface-level vector syntax and
;; converts it into an abomination of C function calls to the generic
;; vector representation library thing.
;;
;; It runs after lower-vectors but before compile-module.
(define (uglify-vectors mod)
  (match mod
    ((module ,[uglify-decl -> fn*] ...)
     `(module ,fn* ...))))

(define uglify-decl
  (lambda (fn)
    (match fn
      ((fn ,name ,args ,t ,[uglify-stmt -> stmt*] ...)
       `(fn ,name ,args ,t ,(apply append stmt*) ...))
      ((extern ,name ,args -> ,t)
       `(extern ,name ,args -> ,t))
      (,else (error 'uglify-decl "Invalid declaration" else)))))

(define decode-vector-type
  (lambda (t)
    (match t
      ((vector ,[dim t sz] ,len)
       (values (+ 1 dim) t `(* (int ,len) ,sz)))
      (,t (values 0 t `(sizeof ,t))))))

(define vector-bytesize
  (lambda (t)
    (let-values (((dim t sz) (decode-vector-type t)))
      sz)))

(define extract-expr-type
  (lambda (e)
    (match e
      ((int ,n) 'int)
      ((var ,t ,x) t)
      ;; This next case is sort of a hack
      (,n (guard (integer? n)) 'int)
      (,else (error 'extract-expr-type
               "cannot find type" else)))))

(define uglify-let-vec
  (lambda (t e n)
    (match e
      ((int ,y)
       (let-values (((dim t sz) (decode-vector-type `(vector ,t ,n))))
         `(cast (vector ,t ,n) (call (ptr void) GC_MALLOC ,sz))))
      ((var int ,y)
       (let-values (((dim t sz) (decode-vector-type `(vector ,t ,y))))
         `(cast (vector ,t ,n) (call (ptr void) GC_MALLOC ,sz))))
      ((var ,tv ,y)
       ;; TODO: this probably needs a copy instead.
       `(var ,tv ,y))
      ;; Otherwise, just hope it works! We should use more type
      ;; information here.
      (,else else)
      (,else (error 'uglify-let-vec
               "invalid vector initalizer" else)))))

(define uglify-stmt
  (lambda (stmt)
    (match stmt
      ((let ,x (vector ,t ,n) ,[uglify-expr -> init])
       (let ((vv (uglify-let-vec t init n)))
         `((let ,x (vector ,t ,n) ,vv))))
      ((let ,x ,t ,[uglify-expr -> e])
       `((let ,x ,t ,e)))
      ((for (,i ,[uglify-expr -> start] ,[uglify-expr -> end])
         ,[uglify-stmt -> stmt*] ...)
       `((for (,i ,start ,end) ,(apply append stmt*) ...)))
      ;; TODO: is set! really just a binop at this point?
      ((set! ,[uglify-expr -> lhs] ,[uglify-expr -> rhs])
       `((set! ,lhs ,rhs)))
      ((return ,[uglify-expr -> e])
       `((return ,e)))
      ((assert ,[uglify-expr -> e])
       `((assert ,e)))
      ;; TODO: vector-set! needs a type annotation. For now we'll
      ;; cheat and only allow integer literals
; WEB: not sure what to do here when adding type annotations to variables
      ((vector-set! ,t ,[uglify-expr -> x] ,[uglify-expr -> i]
         ,[uglify-expr -> v])
       (uglify-vector-set! t x i v))
      ;; TODO: Hmm... we need more type information to correctly
      ;; generate code here.
      ((print ,[uglify-expr -> e])
       `((print ,e)))
      ((print ,[uglify-expr -> e1] ,[uglify-expr -> e2])
       `((print ,e1 ,e2)))       
      ((kernel ,t ,iters ,[stmt*] ...)
       ;; We erase kernel types here... It might be too soon.
       `((kernel ,iters ,(apply append stmt*) ...)))
      (,else
        (error 'uglify-stmt "unsupported stmt" else)))))

(define uglify-vector-set!
  (lambda (t x i v)
    (let ((i (if (integer? i) `(int ,i) i)))
      (match t
        ((vector ,t ,n)
         (let-values (((dim t sz) (decode-vector-type `(vector ,t ,n))))
           `((do (call void memcpy 
                   ,(uglify-vector-ref `(vector ,t ,n) x i)
                   ,v
                   ,sz)))))
        (,scalar
          (guard (symbol? scalar))
          `((set! ,(uglify-vector-ref scalar x i) ,v)))
        (,else (error 'uglify-vector-set!
                 "unsupported vector type" else))))))

(define expr-type
  (lambda (e)
    (match e
      ((var ,t ,x) t)
      ((vector-ref ,t ,v ,i) t)
      (,else (error 'expr-type
               "Unknown expression (this is probably a compiler bug)"
               else)))))

(define uglify-expr
  (lambda (e)
    (match e
      ((time) '(time))
      ((int ,n) `(int ,n))
      ((u64 ,n) `(u64 ,n))
      ((str ,s) `(str ,s))
      ((var ,tx ,x) `(var ,tx ,x))
      ((call ,t ,name ,[args] ...)
       `(call ,t ,name ,args ...))
      ((,op ,[lhs] ,[rhs]) (guard (binop? op))
       `(,op ,lhs ,rhs))
      ((vector-ref ,t ,[e] ,[i])
       (uglify-vector-ref t e i))
      ((length ,e)
       (match (expr-type e)
         ((vector ,t ,n)
          `(int ,n))
         (,else (error 'uglify-expr "Took length of non-vector"
                  else (expr-type e)))))
      (,else
        (error 'uglify-expr (format "unsupported expr: ~s" else))))))

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
