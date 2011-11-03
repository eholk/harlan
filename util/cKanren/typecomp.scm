(library
  (util cKanren typecomp)
  (export usetypecomp prefo =/=)
  (import
    (rnrs)
    (util cKanren ck)
    (util cKanren mk)
    (util cKanren pref)
    (util cKanren neq))

(define process-prefixcomp
  (lambda (p c)
    (cond
      ((null? p) identitym)
      (else
        (let ((x (lhs (car p))) (v (rhs (car p))))
          (lambdam@ (a : s d c)
            (cond
              ((and (not (var? v)) (get-dom x d))
               => (lambda (dom)
                    (and (memq v dom)
                         ((process-prefixcomp (cdr p) c) a))))
              (else
                ((composem
                   (run-constraints (if (var? v) `(,x ,v) `(,x)) c)
                   (process-prefixcomp (cdr p) c))
                 a)))))))))

(define enforce-constraintscomp enforce-constraintspref)
(define reify-constraintscomp reify-constraintsneq)

(define (usetypecomp)
  (process-prefix process-prefixcomp)
  (enforce-constraints enforce-constraintscomp)
  (reify-constraints reify-constraintscomp))

)
