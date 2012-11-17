(library
  (harlan front compile-front)
  (export compile-harlan-frontend)
  (import
    (rnrs)
    (harlan front expand-macros)
    (harlan front parser)
    (harlan front returnify)
    (harlan front typecheck)
    (harlan front expand-primitives)
    (harlan compile-opts)
    (only (harlan verification-passes)
      verify-harlan
      verify-returnify
      verify-parse-harlan
      verify-typecheck
      verify-expand-primitives))
  
  (define compile-harlan-frontend
    (passes
     (expand-macros)
     (parse-harlan verify-parse-harlan)
     (returnify verify-returnify)
     (typecheck verify-typecheck)
     (expand-primitives verify-expand-primitives)))
  
  ;; end library
  )
