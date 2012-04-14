(library
  (harlan backend print-c)
  (export
    harlan-print-c
    harlan-format-c
    harlan-format-decl
    harlan-format-stmt
    harlan-format-expr
    harlan-format-ident
    harlan-format-type
    harlan-format-arg
    harlan-format-binop
    harlan-format-relop)
  (import
    (rnrs)
    (only (chezscheme) trace-define)
    (elegant-weapons print-c)
    (elegant-weapons match)
    (elegant-weapons compat)
    (elegant-weapons helpers))

  (define (harlan-expr expr ft)
    (match expr
      ((alloc
         ,[harlan-format-expr -> region]
         ,[harlan-format-expr -> size])
       (string-append "alloc_in_region(" region ", " size ")"))
      ((region-ref
         ,[harlan-format-type -> type]
         ,[harlan-format-expr -> region]
         ,[harlan-format-expr -> ptr])
       (string-append
         "((" type ")(get_region_ptr(" region ", " ptr "))"))
      (,else (ft else))))

  (define-syntax format-as-harlan
    (syntax-rules ()
      ((_ expr ...)
       (call-with-fns ((expr-fns harlan-expr))
         expr ...))))

  (define-syntax define-with-harlan-fns
    (syntax-rules ()
      ((_ (name base-fn) ...)
       (begin
         (define (name x)
           (format-as-harlan (base-fn x)))
         ...))))

  (define-with-harlan-fns
    (harlan-print-c      print-c)
    (harlan-format-c     format-c)
    (harlan-format-decl  format-decl)
    (harlan-format-stmt  format-stmt)
    (harlan-format-expr  format-expr)
    (harlan-format-ident format-ident)
    (harlan-format-type  format-type)
    (harlan-format-arg   format-arg)
    (harlan-format-binop format-binop)
    (harlan-format-relop format-relop))

)

