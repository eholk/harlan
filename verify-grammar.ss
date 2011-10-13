;; verify-grammar.ss
;; see eof for an example

(library
  (verify-grammar)
  (export generate-verify)
  (import (chezscheme))
  
  ;; expands to ALL the procedures
  (define-syntax (generate-verify x)

    ;; nonterminal ->  verify-nonterminal
    ;; terminal    ->  terminal?
    (define (create-name completion)
      (lambda (syn)
        (datum->syntax syn
          (string->symbol
            (completion (symbol->string (syntax->datum syn)))))))
    (define pred-name
      (create-name (lambda (str) (string-append str "?"))))
    (define verify-name
      (create-name (lambda (str) (string-append "verify-" str))))
    
    ;; terminals have a lowercase first char, nonterminals are capitalized
    (define (form-pred proc)
      (lambda (syn)
        (let ((sym (syntax->datum syn)))
          (and (symbol? sym)
               (proc (string-ref (symbol->string sym) 0))))))
    (define nonterminal? (form-pred char-upper-case?))
    (define terminal? (form-pred char-lower-case?))
    
    ;; outputs a boolean expression that patternmatches inp
    ;; at this point, all lower-case symbols are matched exactly
    (define (pattern-match pattern inp)
      (syntax-case pattern ()
        ((a . d)
         #`(and (pair? #,inp)
                #,(pattern-match #'a #`(car #,inp))
                #,(pattern-match #'d #`(cdr #,inp))))
        (_ (if (nonterminal? pattern)
               #`(#,(verify-name pattern) #,inp)
               #`(eq? #,inp '#,pattern)))))
    
    ;; either returns (terminal? inp) or a boolean expression to match pairs
    (define (create-clause pattern inp)
      (if (terminal? pattern)
          #`(#,(pred-name pattern) #,inp)
          (pattern-match pattern inp)))
    
    ;; just so you know, these error messages are meaningful
    (define (meaningful-error pass left right* inp)
      #`(apply errorf
          '#,pass
          (apply string-append
            "Output expr ~s does not conform to grammar.\n"
            "Was expecting one of the following for ~s:"
            '#,(map (lambda (_) " ~s") right*))
          #,inp #,left '#,right*))
    
    ;; outputs the body of a pass verify-nonterm
    ;; catches errors, but throws one if there are no options that match
    (define (create-body pass left right* inp)
      (with-syntax (((clauses ...)
                     (map (lambda (rout) (create-clause rout inp)) right*)))
        #`(or (guard (x ((error? x) #f)) clauses) ...
              #,(meaningful-error pass left right* inp))))
    
    ;; actual macro
    (syntax-case x ()
      ((_ pass (left right* ...) (left* right** ...) ...)
       (with-syntax
           (((name start nonterminals ...)
             (map verify-name #'(pass left left* ...)))
            ((body body* ...)
             (map
               (lambda (l r) (create-body #'pass #`'#,l r l))
               #'(left left* ...)
               #'((right* ...) (right** ...) ...))))
         #'(define name
             (lambda (prg)
               (define start (lambda (left) body))
               (define nonterminals (lambda (left*) body*))
               ...
               (begin (start prg) prg)))))))

)

#!eof
;; an example

;; lambda-calc
;; Term -> (lambda (Var) Term) | (Term Term) | Var
;; Var -> symbol

;; lambda-calc
;; Term -> (lambda (Var) Term)
;;       | (Term Term)
;;       | Var
;; Var -> symbol

(generate-verify lambda-calc
  (Term
    (lambda (Var) Term)
    (Term Term)
    Var)
  (Var symbol))

