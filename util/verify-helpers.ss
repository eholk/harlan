(library
  (util verify-helpers)
  (export verify-name pred-name nonterminal? terminal?)
  (import (rnrs) (only (chezscheme) trace-define))

;; nonterminal ->  verify-nonterminal
;; terminal    ->  terminal?
(define (create-name completion)
  (lambda (syn)
    (syntax-case syn ()
      (_ (datum->syntax syn
           (string->symbol
             (completion (symbol->string (syntax->datum syn)))))))))
(define pred-name
  (create-name (lambda (str) (string-append str "?"))))
(define verify-name
  (create-name (lambda (str) (string-append "verify-" str))))

;; terminals have a lowercase first char,
;; nonterminals are capitalized
(define (form-pred proc)
  (lambda (syn)
    (syntax-case syn ()
      (_ (let ((sym (syntax->datum syn)))
           (and (symbol? sym)
                (proc (string-ref (symbol->string sym) 0))))))))
(define nonterminal? (form-pred char-upper-case?))
(define terminal? (form-pred char-lower-case?))
  
)