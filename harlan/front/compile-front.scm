(library
  (harlan front compile-front)
  (export compile-harlan-frontend)
  (import
    (rnrs)
    (harlan front parser)
    (harlan front returnify)
    (harlan front typecheck)
    (harlan front expand-primitives)
    (harlan front remove-danger)
    (harlan compile-opts)
    (only (harlan verification-passes)
      verify-harlan
      verify-returnify
      verify-parse-harlan
      verify-typecheck
      verify-expand-primitives
      verify-remove-danger))
  
  (define compile-harlan-frontend
    (passes
     (parse-harlan verify-parse-harlan)
     (returnify verify-returnify)
     (typecheck verify-typecheck)
     (expand-primitives verify-expand-primitives)
     (remove-danger verify-remove-danger)))
  
  ;; end library
  )
