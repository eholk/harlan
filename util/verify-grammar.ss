;; verify-grammar.ss
;; see eof for an example

(library
  (util verify-grammar)
  (export grammar-transforms generate-verify wildcard?)
  (import
    (rnrs)
    (only (chezscheme)
      errorf
      pretty-print
      with-output-to-string))

  (define wildcard? (lambda (x) #t))

  (define-syntax (grammar-transforms x)
    
    (define (create-name completion)
      (lambda (syn)
        (datum->syntax syn
          (string->symbol
            (completion (symbol->string (syntax->datum syn)))))))
    
    (define verify-name
      (create-name (lambda (str) (string-append "verify-" str))))
    
    (define remove-duplicates
      (lambda (l)
        (cond
          ((null? l) '())
          ((member (car l) (cdr l)) (remove-duplicates (cdr l)))
          (else (cons (car l) (remove-duplicates (cdr l)))))))
    
    (syntax-case x ()
      ((_ (generate-verify pass (Nonterm Terms ...) ...) ...)
       (with-syntax (((unique-nonterms ...)
                      (remove-duplicates #'(Nonterm ... ...))))
         (with-syntax (((verify-names ...)
                        (map verify-name #'(unique-nonterms ...))))
           #'(begin (define verify-names (make-paramter 'dummy)) ...
                    (generate-verify pass (Nonterm Terms ...) ...) ...))))))
  
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
    (define verify-name/pass
      (syntax-case x ()
        ((_ pass . etc)
         (let ((pass^ (symbol->string (syntax->datum #'pass))))
           (create-name (lambda (str) (string-append "verify-" pass^ "-" str)))))))
    (define extends-name ;; (Term (%extends lift-vectors Term))
      (lambda (syn)
        (syntax-case x ()
          ((_ pass . etc)
           (let ((p (syntax->datum syn)))
             (let ((old-pass (symbol->string (cadr p)))
                   (old-nonterm (symbol->string (caddr p))))
               (datum->syntax #'pass
                 (string->symbol
                   (string-append
                     "verify-" old-pass "-" old-nonterm)))))))))
    
    ;; terminals have a lowercase first char,
    ;; nonterminals are capitalized
    (define (form-pred proc)
      (lambda (syn)
        (let ((sym (syntax->datum syn)))
          (and (symbol? sym)
               (proc (string-ref (symbol->string sym) 0))))))
    (define nonterminal? (form-pred char-upper-case?))
    (define terminal? (form-pred char-lower-case?))
    (define extends?
      (lambda (syn)
        (let ((p (syntax->datum syn)))
          (and (pair? p)
               (eq? (car p) '%extends)))))
    
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
          #`(#,(verify-name pattern) #,inp))
        (_ #`(eq? '#,pattern #,inp))))
    
    ;; either returns (terminal? inp)
    ;; or a boolean expression to match pairs
    (define (create-clause pattern inp)
      (cond
        ((terminal? pattern)
         #`(#,(pred-name pattern) #,inp))
        ((extends? pattern)
         #`(#,(extends-name pattern) #,inp))
        (else (pattern-match pattern inp))))
    
    ;; just so you know, these error messages are meaningful
    (define (meaningful-error left inp)
      (syntax-case x ()
        ((_ pass . etc)
         #`(errorf
             '#,(datum->syntax #'pass
                  (string->symbol
                    (string-append "verify-"
                      (symbol->string (syntax->datum #'pass)))))
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
            ((start/pass nonterminals/pass ...)
             (map verify-name/pass #'(left left* ...)))
            ((body body* ...)
             (map
               (lambda (l r) (create-body #`'#,l r l))
               #'(left left* ...)
               #'((right* ...) (right** ...) ...))))
         #'(begin
             (define start/pass (lambda (left) body))
             (define nonterminals/pass (lambda (left*) body*))
             ...
             (define name
               (lambda (prg)
                 (start start/pass)
                 (nonterminals nonterminals/pass)
                 ...
                 (and (start prg) prg))))))))

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