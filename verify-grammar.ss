(print-gensym #f)

(define create-name
  (lambda (syn)
    (datum->syntax syn
      (string->symbol
        (let ((str (symbol->string (syntax->datum syn))))
          (if (char-lower-case? (string-ref str 0))
              (string-append str "?")
              (string-append "verify-" str)))))))

(define (nonterminal? syn)
  (let ((sym (syntax->datum syn)))
    (and (symbol? sym)
         (char-upper-case?
           (string-ref(symbol->string sym) 0)))))

(define (terminal? syn)
  (let ((sym (syntax->datum syn)))
    (and (symbol? sym)
         (char-lower-case?
           (string-ref(symbol->string sym) 0)))))

(define (create-pattern-match rout nonterm)
  (letrec
      ((loop
         (lambda (rout nonterm k)
           (syntax-case rout ()
             ((a . d)
              #`(and (pair? #,nonterm)
                     #,(loop #'d #`(cdr #,nonterm)
                         #`(lambda (v) (and v #,(loop #'a #`(car #,nonterm) k))))))
             (_ (nonterminal? rout)
               #`(#,k (#,(create-name rout) #,nonterm)))
             (_ #`(#,k (eq? #,nonterm '#,rout)))))))
    (loop rout nonterm #'(lambda (v) v))))

(define (create-clause nonterm rout)
  #`(#,(if (terminal? rout)
           #`(#,(create-name rout) #,nonterm)
           (create-pattern-match rout nonterm))
     (void)))

(define (create-body pass-name nonterm l)
  (with-syntax (((clauses ...)
                 (map (lambda (rout) (create-clause nonterm rout)) l)))
    #`(cond
        clauses ...
        (else (errorf '#,pass-name "Output does not conform to grammar.")))))

(define-syntax (generate-verify x)
  (define (create-verify-name syn)
    (datum->syntax syn
      (string->symbol
        (string-append "verify-"
          (symbol->string (syntax->datum syn))))))
  (syntax-case x ()
    ((_ pass (r0in r0out ...) (rin* rout* ...) ...)
     (with-syntax
         (((name start nonterminals ...)
           (map create-verify-name #'(pass r0in rin* ...)))
          ((body body* ...)
           (map
             (lambda (i o) (create-body #'pass i o))
             #'(r0in rin* ...)
             #'((r0out ...) (rout* ...) ...))))
       #'(define name
           (lambda (prg)
             (define start (lambda (r0in) body))
             (define nonterminals (lambda (rin*) body*))
             ...
             (begin (start prg) prg)))))))


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

(generate-verify test-cps
  (Term
    (lambda (Var) Int)
    (lambda (Int) Int Int))
  (Var symbol)
  (Int integer))
