(library
  (harlan front compile-front)
  (export compile-harlan-frontend)
  (import
    (rnrs)
    (harlan front parser)
    (harlan front typecheck)
    (harlan compile-opts)
    (only (harlan verification-passes)
      verify-harlan
      verify-parse-harlan
      verify-typecheck))

(define compile-harlan-frontend
  (passes
    verify-harlan
    parse-harlan
    verify-parse-harlan
    typecheck
    verify-typecheck))

;; end library
)
