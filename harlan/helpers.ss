(library
  (harlan helpers)
  (export
    decode-vector-type
    vector-bytesize
    type-of
    ident?
    reduceop?
    harlan-type?
    harlan-c-type?
    harlan-cl-type?)
  (import
    (rnrs)
    (elegant-weapons helpers)
    (elegant-weapons match))

  (define decode-vector-type
    (lambda (t)
      (match t
        ((vec ,len ,[dim t sz])
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

  (define (reserved-word? x)
    (memq x
      '(kernel for while print vector vector-ref reduce let
         assert vector-set! set! iota make-vector length)))

  (define (ident? x)
    (and (symbol? x)
         (not (reserved-word? x))))

  (define (reduceop? op)
    (memq op '(+ *)))

  (define (harlan-type? t)
    (or (scalar-type? t)
        (case t
          ((ofstream region_ptr region) #t)
          (else #f))))

  (define (harlan-c-type? t)
    (or (c-type? t)
        (case t
          ((region_ptr region) #t)
          (else #f))))

  (define (harlan-cl-type? t)
    (or (cl-type? t)
        (case t
          ((cl::queue cl::kernel cl::program) #t)
          (else #f))))

)
