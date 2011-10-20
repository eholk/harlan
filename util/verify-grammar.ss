;; verify-grammar.ss
;; see eof for an example

(library
  (util verify-grammar)
  (export grammar-transforms generate-verify wildcard?)
  (import
    (rnrs)
    (only (chezscheme)
      errorf
      make-parameter
      pretty-print
      trace-define
      trace-define-syntax
      with-output-to-string))

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
    
    ;; terminals have a lowercase first char,
    ;; nonterminals are capitalized
    (define (form-pred proc)
      (lambda (syn)
        (let ((sym (syntax->datum syn)))
          (and (symbol? sym)
               (proc (string-ref (symbol->string sym) 0))))))
    (define nonterminal? (form-pred char-upper-case?))
    (define terminal? (form-pred char-lower-case?))
    
    (define (*form repeated inp)
      (with-syntax (((loopvar) (generate-temporaries '(inp))))
        #`(let loop ((loopvar #,inp))
            (or (null? loopvar)
                (and #,(pattern-match repeated #`(car loopvar))
                     (loop (cdr loopvar)))))))

    ;; outputs a boolean expression that pattern matches inp
    ;; at this point, all lower-case symbols are matched exactly
    (define (pattern-match pattern inp)
      (syntax-case pattern (* +)
        ((a *) (*form #'a inp))
        ((a * d ...)
         (with-syntax (((rd ...) (reverse #'(d ...)))
                       ((tmp) (generate-temporaries '(tmp))))
           #`(and (pair? #,inp)
                  (let ((tmp (reverse #,inp)))
                    #,(pattern-match #'(rd ... a *) #'tmp)))))
        ((a + d ...) (pattern-match #'(a a * d ...) inp))
        ((a . d)
         (with-syntax (((tmp) (generate-temporaries '(tmp))))
           #`(and (pair? #,inp)
                  (let ((tmp (car #,inp)))
                    #,(pattern-match #'a #'tmp))
                  (let ((tmp (cdr #,inp)))
                    #,(pattern-match #'d #'tmp)))))
        (_ (nonterminal? pattern)
          #`((#,(verify-name pattern)) #,inp))
        (_ #`(eq? '#,pattern #,inp))))
    
    ;; either returns (terminal? inp)
    ;; or a boolean expression to match pairs
    (define (create-clause pattern inp)
      (cond
        ((terminal? pattern)
         #`(#,(pred-name pattern) #,inp))
        (else (pattern-match pattern inp))))
    
    ;; just so you know, these error messages are meaningful
    (define (meaningful-error left inp)
      (syntax-case x ()
        ((_ pass . etc)
         #`(errorf
             '#,(verify-name #'pass)
             "\nFollowing ~s does not conform to grammar.\n~a\n"
             #,left (with-output-to-string
                      (lambda () (pretty-print #,inp)))))))
    
    ;; outputs the body of a pass verify-nonterm
    ;; catches errors, throws one if no options match
    (define (create-body left right* inp)
      (with-syntax (((clauses ...)
                     (map
                       (lambda (rout) (create-clause rout inp))
                       right*)))
        #`(or (guard (x ((error? x) #f)) clauses) ...
              #,(meaningful-error left inp))))
    
    ;; actual macro
    (syntax-case x ()
      ((_ pass (left right* ...) (left* right** ...) ...)
       (with-syntax
           (((name start nonterminals ...)
             (map verify-name #'(pass left left* ...)))
            ((body body* ...)
             (map
               (lambda (l r) (create-body #`'#,l r l))
               #'(left left* ...)
               #'((right* ...) (right** ...) ...))))
         #'(define name
             (lambda (prg)
               (letrec
                   ((start (lambda (left) body))
                    (nonterminals (lambda (left*) body*))
                    ...)
                 (and (start prg) prg))))))))
  
  (define-syntax (grammar-transforms x)
    (define (lookup-nts inp parent)
      (syntax-case parent ()
        (() (errorf 'lookup-nts "You fucked up ~s" inp))
        (((nt . t*) . rest)
         (if (eq? (syntax->datum #'nt) (syntax->datum inp))
             #'(nt . t*)
             (lookup-nts inp #'rest)))))
    (define (add-nts parent child)
      (syntax-case child ()
        (() #'())
        ((nt . rest)
         #`(#,(lookup-nts #'nt parent) . #,(add-nts parent #'rest)))))
    (syntax-case x (%inherits)
      ((_ pass) #'(generate-verify . pass))
      ((_ (pass1 (nt* t** ...) ...) (pass2 (%inherits nts ...) clauses ...) . rest)
       (with-syntax (((inherited ...)
                      (add-nts #'((nt* t** ...) ...) #'(nts ...))))
         #`(begin
             (generate-verify pass1 (nt* t** ...) ...)
             (grammar-transforms
               (pass2 inherited ... clauses ...) . rest))))
      ((_ pass . rest)
       #`(begin (generate-verify . pass) (grammar-transforms . rest)))))
  
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

|#