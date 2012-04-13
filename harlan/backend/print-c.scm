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
    (elegant-weapons print-c)
    (elegant-weapons match)
    (elegant-weapons compat)
    (elegant-weapons helpers))

  (define-syntax format-as-harlan
    (syntax-rules ()
      ((_ expr ...)
       (call-with-fns () expr ...))))

  (define-syntax define-as-harlan
    (syntax-rules ()
      ((_ (define name body* ...) ...)
       (begin
         (define name (format-as-harlan body* ...))
         ...))))

  (define-as-harlan
    (define harlan-print-c print-c)
    (define harlan-format-c format-c)
    (define harlan-format-decl format-decl)
    (define harlan-format-stmt format-stmt)
    (define harlan-format-expr format-expr)
    (define harlan-format-ident format-ident)
    (define harlan-format-type format-type)
    (define harlan-format-arg format-arg)
    (define harlan-format-binop format-binop)
    (define harlan-format-relop format-relop))

)

