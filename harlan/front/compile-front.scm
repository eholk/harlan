(library
  (harlan front compile-front)
  (export compile-harlan-frontend)
  (import
    (rnrs)
    (harlan front parser)
    (harlan front typecheck)
    (harlan front nest-lets)
    (harlan front returnify)
    (harlan compile-opts)
    (only (harlan verification-passes)
      verify-harlan
      verify-nest-lets
      verify-returnify
      verify-parse-harlan
      verify-typecheck))
  
  (define compile-harlan-frontend
    (passes
      verify-harlan
      nest-lets
      verify-nest-lets
      parse-harlan
      verify-parse-harlan
      returnify
      verify-returnify
      typecheck
      verify-typecheck))
  
  ;; end library
  )
