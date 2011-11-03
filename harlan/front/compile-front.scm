(library
  (harlan front compile-front)
  (export compile-harlan-frontend)
  (import
    (rnrs)
    (harlan front parser)
    (harlan front flatten-lets)
    (harlan front typecheck)
    (harlan front nest-lets)
    (harlan compile-opts)
    (only (harlan verification-passes)
      verify-harlan
      verify-nest-lets
      verify-parse-harlan
      verify-flatten-lets
      verify-typecheck))
  
  (define compile-harlan-frontend
    (passes
      verify-harlan
      nest-lets
      verify-nest-lets
      parse-harlan
      verify-parse-harlan
      typecheck
      verify-typecheck
      flatten-lets
      verify-flatten-lets))
  
  ;; end library
  )
