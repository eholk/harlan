;; verify-grammar.ss
;; see eof for an example

(library
  (verify-grammar)
  (export generate-verify wildcard?)
  (import
    (rnrs)
    (only (chezscheme) errorf pretty-print with-output-to-string))

  (define wildcard? (lambda (x) #t))
  
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
    
    (define (dotdotdot repeated inp)
      (with-syntax (((loopvar) (generate-temporaries '(inp))))
        #`(let loop ((loopvar #,inp))
            (or (null? loopvar)
                (and #,(pattern-match repeated #`(car loopvar))
                     (loop (cdr loopvar)))))))

    ;; outputs a boolean expression that pattern matches inp
    ;; at this point, all lower-case symbols are matched exactly
    (define (pattern-match pattern inp)
      (syntax-case pattern (* +)
        (() #`(null? #,inp))
        ((a *) (dotdotdot #'a inp))
        ((a * d ...)
         (with-syntax (((rd ...) (reverse #'(d ...)))
                       ((id) (generate-temporaries '(id))))
           #`(and (pair? #,inp)
                  (let ((id (reverse #,inp)))
                    #,(pattern-match #'(rd ... a *) #'id)))))
        ((a + d ...) (pattern-match #'(a a * d ...) inp))
        (((aa . ad) . d)
         #`(and (pair? #,inp)
                (and #,(pattern-match #'d #`(cdr #,inp))
                     #,(pattern-match #'(aa . ad) #`(car #,inp)))))
        ((a . d)
         #`(and (pair? #,inp)
                #,(if (nonterminal? #'a)
                      #`(and #,(pattern-match #'d #`(cdr #,inp))
                             #,(pattern-match #'a #`(car #,inp)))
                      #`(and (eq? (car #,inp) 'a)
                             #,(pattern-match #'d #`(cdr #,inp))))))
        (_ #`(#,(verify-name pattern) #,inp))))
    
    ;; either returns (terminal? inp) or a boolean expression to match pairs
    (define (create-clause pattern inp)
      (if (terminal? pattern)
          #`(#,(pred-name pattern) #,inp)
          (pattern-match pattern inp)))
    
    ;; just so you know, these error messages are meaningful
    (define (meaningful-error pass left right* inp)
      #`(errorf
          '#,pass
          "\nFollowing ~s does not conform to grammar.\n~a\n"
          #,left
          (with-output-to-string (lambda () (pretty-print #,inp)))))
    
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
               (and (start prg) prg)))))))

)

#|

Here's an example:

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

> (import (verify-grammar))
> (generate-verify lambda-calc
    (Term
      (lambda (Var) Term)
      (Term Term)
      Var)
    (Var symbol))
> (verify-lambda-calc '(lambda (x) x))
(lambda (x) x)
> (verify-lambda-calc '(lambda (x) x y))

Exception in lambda-calc: Output expr (lambda (x) x y) does not conform to grammar.
Was expecting one of the following for Term: (lambda (Var) Term) (Term Term) Var
Type (debug) to enter the debugger.

|#
