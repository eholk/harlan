(library
 (util helpers)
 (export
   gensym
   iota
   define-match
   andmap
   type-of
   decode-vector-type
   vector-bytesize
   binop?
   relop?
   ident?
   reduceop?
   float?
   scalar-type?
   c-type?
   join
   make-begin)
 (import
   (rnrs)
   (util match)
   (only (chezscheme) with-implicit))

(define make-begin
  (lambda (expr*)
    (match
      (match `(begin ,@expr*)
        [(begin ,[expr*] ...) (apply append expr*)]
        [,expr (list expr)])
      [(,x) x]
      [(,x ,x* ...) `(begin ,x ,x* ...)])))

(define join
   (lambda (sep strings)
     (match strings
       (() "")
       ((,a) a)
       ((,a ,b) (string-append a sep b))
       ((,a ,b* ...)
        (string-append a sep (join sep b*))))))
 
 ;; This abstracts away most of the boilerplate for writing
 ;; match-based transformations. A little unsafe.
 ;; Stipulation: make sure the match macro is in the environment
 ;; everywhere this macro is used.
 (define-syntax (define-match x)
   (syntax-case x ()
     ((k (name args ...) clauses ...)
      (with-implicit (k match)
        #'(define (name args ...)
            (lambda (arg)
              (match arg
                clauses ...
                (,else (error 'name "Unrecognized item" else)))))))
     ((k name clauses ...)
      (with-implicit (k match)
        #'(define name
            (lambda (arg)
              (match arg
                clauses ...
                (,else (error 'name "Unrecognized item" else)))))))))
 
 (define gensym
   (let ((c 0))
     (lambda (x)
       (unless (symbol? x) (error 'gensym "invalid symbol" x))
       (set! c (+ 1 c))
       (string->symbol
         (string-append (symbol->string x) "_" (number->string c))))))

 (define iota
   (lambda (n)
     (let loop ([i 0])
       (cond
         [(= i n) '()]
         [else (cons i (loop (+ i 1)))]))))

 (define andmap
   (lambda (p ls)
     (if (null? ls)
         #t
         (and (p (car ls)) (andmap p (cdr ls))))))

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

(define-match type-of
  ((deref ,[e]) e)
  ((vector-ref ,t ,v ,i) t)
  ((var ,t ,x) t))

 (define binop?
   (lambda (op)
     (case op
       ((bitwise-or + * - mod /) #t)
       (else #f))))
 
 (define relop?
   (lambda (op)
     (case op
       ((< <= = > >=) #t)
       (else #f))))

(define (reserved-word? x)
  (memq x
    '(kernel for while print vector vector-ref reduce
       assert vector-set! set! iota make-vector length)))

 (define (ident? x)
   (and (symbol? x)
        (not (reserved-word? x))))

 (define (reduceop? op)
   (memq op '(+ *)))

 (define (float? n)
   (and (number? n) (inexact? n)))
 
 (define (scalar-type? t)
   (case t
     ;; TODO: strings aren't quite scalars
     ((int u64 void str float cl::queue cl::kernel cl::program) #t)
     (else #f)))

(define (c-type? t)
  (case t
    ((int uint64_t void float cl::queue cl::kernel cl::program char) #t)
    (else #f)))

)