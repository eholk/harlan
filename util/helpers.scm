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
   scalar-type?)
 (import (rnrs)
         (util match))

 ;; This abstracts away most of the boilerplate for writing
 ;; match-based transformations.
 ;;
 ;; It has a major but though. Clauses like ((,foo ...) `(,foo ...))
 ;; generates things like ((,foo ...)) instead of ((,@foo)) like it
 ;; should. Do any macro wizards have any ideas?
 ;;
 ;; Fortunately, you can work around this by using ,@ or .,
 (define-syntax define-match
    (syntax-rules (unquote)
      ((_ (name args ...) clause ...)
       (define name
         (lambda (arg args ...)
           (match arg
             clause ...
             (,else (error 'name "Unrecognized item" else))))))))
 
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

(define-match (type-of)
  ((deref ,[e]) e)
  ((vector-ref ,t ,v ,i) t)
  ((var ,t ,x) t))

 (define binop?
   (lambda (op)
     (case op
       ((< = bitwise-or + * - mod /) #t)
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
     ((int u64 str void str) #t)
     (else #f)))

 )