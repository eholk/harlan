;; (eval
;;   '(my-nested-macro 5 6)
;;   (environment '(rnrs) '(harlan-forms) '(harlan front macros)))
;; => (return (+ 5 6))

(library (harlan-forms)
  (export return +)
  (import (prefix (rnrs) rnrs:))

(rnrs:define-syntax define-harlan-forms
  (rnrs:syntax-rules ()
    ((_ (expr))
     (rnrs:define-syntax (expr x)
       (rnrs:syntax-case x ()
         ((_ . n) (rnrs:syntax (rnrs:quote (expr . n)))))))
    ((_ (expr rest rnrs:...))
     (rnrs:begin
       (rnrs:define-syntax expr
         (rnrs:syntax-rules ()
           ((_ n (rnrs:... rnrs:...))
            (rnrs:quasiquote
              (expr (rnrs:unquote n) (rnrs:... rnrs:...))))))
       (define-harlan-forms (rest rnrs:...))))))

(define-harlan-forms (return +))

)

(library (harlan front macros)
  (export my-macro my-nested-macro)
  (import
    (rnrs)
    (only (chezscheme) expand eval)
    (prefix (harlan-forms) harlan:))

(define-syntax my-macro
  (syntax-rules ()
    ((_ n ...) (harlan:return n ...))))

(define-syntax my-nested-macro
  (syntax-rules ()
    ((_ m n) (my-macro (harlan:+ m n)))))

)


