;; shoo nonsense
(print-gensym #f)

;; Nonterminal to verify-Nonterminal and terminal to terminal?
(define (create-name completion)
  (lambda (syn)
    (datum->syntax syn
      (string->symbol
        (completion (symbol->string (syntax->datum syn)))))))
(define create-pred-name
  (create-name (lambda (str) (string-append str "?"))))
(define create-verify-name
  (create-name (lambda (str) (string-append "verify-" str))))

;; terminals have a lowercase first char, nonterminals are capitalized
(define (form-pred proc syn)
  (let ((sym (syntax->datum syn)))
    (and (symbol? sym)
         (proc (string-ref (symbol->string sym) 0)))))
(define (nonterminal? syn) (form-pred char-upper-case? syn))
(define (terminal? syn) (form-pred char-lower-case? syn))

;; outputs a boolean expression that patternmatches inp
;; at this point, all lower-case symbols are matched exactly
(define (create-pattern-match rout inp)
  (syntax-case rout ()
    ((a . d)
     #`(and (pair? #,inp)
            #,(create-pattern-match #'d #`(cdr #,inp))
            #,(create-pattern-match #'a #`(car #,inp))))
    (_ (nonterminal? rout)
      #`(#,(create-name rout) #,inp))
    (_ #`(eq? #,inp '#,rout))))

;; either returns (terminal? inp) or a boolean expression
(define (create-clause rout inp)
  (if (terminal? rout)
      #`(#,(create-pred-name rout) #,inp)
      (create-pattern-match rout inp)))

;; to be clear, these error messages are meaningful
(define (meaningful-error pass nonterm opts inp)
  #`(apply errorf
      '#,pass
      (apply string-append
        "Output expr ~s does not conform to grammar.\n"
        "Was expecting one of the following for ~s:"
        '#,(map (lambda (s) " ~s") opts))
      #,inp #,nonterm '#,opts))

;; outputs the body of a pass verify-nonterm
;; catches errors, but throws one if there are no options that match
(define (create-body pass nonterm opts inp)
  (with-syntax (((clauses ...)
                 (map (lambda (rout) (create-clause rout inp)) opts)))
    #`(or (guard (x ((error? x) #f)) clauses) ...
          #,(meaningful-error pass nonterm opts inp))))

;; expands to ALL the procedures
(define-syntax (generate-verify x)
  (syntax-case x ()
    ((_ pass (r0in r0out ...) (rin* rout* ...) ...)
     (with-syntax
         (((name start nonterminals ...)
           (map create-verify-name #'(pass r0in rin* ...)))
          ((body body* ...)
           (map
             (lambda (i o) (create-body #'pass #`'#,i o i))
             #'(r0in rin* ...)
             #'((r0out ...) (rout* ...) ...))))
       #'(define name
           (lambda (prg)
             (define start (lambda (r0in) body))
             (define nonterminals (lambda (rin*) body*))
             ...
             (begin (start prg) prg)))))))

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

