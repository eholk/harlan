(library
  (harlan helpers)
  (export
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

  (define-match type-of
    ((deref ,[e]) e)
    ((vector-ref ,t ,v ,i) t)
    ((var ,t ,x) t))

  (define (reserved-word? x)
    (memq x
     '(kernel for while print vector vector-ref reduce
       let assert set! iota make-vector length)))
  
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
