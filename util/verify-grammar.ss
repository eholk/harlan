;; verify-grammar.ss
;; see eof for an example

(library
  (util verify-grammar)
  (export
    grammar-transforms
    generate-verify
    wildcard?

    ;; so that we can test on the REPL in Chez
    %static
    %inherits)
  (import
    (rnrs)
    (util verify-helpers)
    (harlan compile-opts)
    (util compat))
  
(define wildcard? (lambda (x) #t))

(define-syntax (%static x)
  (syntax-violation '%static "misplaced auxiliary keyword" x))
(define-syntax (%inherits x)
  (syntax-violation '%inherits "misplaced auxiliary keyword" x))

;; expands to ALL the procedures
(define-syntax (generate-verify x)
  
  (define (*form repeated inp)
    (with-syntax (((loopvar) (generate-temporaries '(inp))))
      #`(let loop ((loopvar #,inp))
          (or (null? loopvar)
              (and (pair? loopvar)
                   #,(pattern-match repeated #`(car loopvar))
                   (loop (cdr loopvar)))))))
  
  ;; outputs a boolean expression that pattern matches inp
  ;; at this point, all lower-case symbols are matched exactly

  (define (pattern-match pattern inp)
    (syntax-case pattern (* +)
      (() #`(null? #,inp))
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
      (else (pattern-match pattern inp))))
  
  ;; just so you know, these error messages are meaningful
  (define (meaningful-error left inp)
    (syntax-case x ()
      ((_ pass . clauses)
       #`(error
           '#,(verify-name #'pass)
           (string-append
             "Does not conform to this grammar")
           #,left
           #,inp
           'clauses))))
  
  ;; outputs the body of a pass verify-nonterm
  ;; catches errors, throws one if no options match
  (define (create-body left right* problems inp)
    (with-syntax (((clauses ...)
                   (map
                     (lambda (rout) (create-clause rout inp))
                     right*)))
      #`(or (and (or (guard (x ((error? x) #f)) clauses) ...)
                 (set! #,problems (remq #,inp #,problems)))
            (begin (set! #,problems (cons #,inp #,problems)) #f))))

  (define (check-nonterms pass left* right**)
    (let ((left* (map syntax->datum left*)))
      (let loop ((r right**))
        (syntax-case r ()
          ((a . d) (begin (loop #'a) (loop #'b)))
          (a 
            (or (not (nonterminal? #'a))
                (member (syntax->datum #'a) left*)
                (error (syntax->datum (verify-name pass))
                  "Unbound nonterminal in rhs" #'a)))))))
  
  ;; actual macro
  (syntax-case x ()
    ((_ pass (left right* ...) (left* right** ...) ...)
     (check-nonterms #'pass
       #'(left left* ...)
       #'((right* ...) (right** ...) ...))
     (with-syntax (((problems) (generate-temporaries `(problems))))
       (with-syntax
           (((name start nonterminals ...)
             (map verify-name #'(pass left left* ...)))
            ((body body* ...)
             (map
               (lambda (l r) (create-body #`'#,l r #'problems l))
               #'(left left* ...)
               #'((right* ...) (right** ...) ...))))
         #'(define name
             (lambda (prg)
               (let ((problems `()))
                 (if (verify)
                     (letrec
                         ((start (lambda (left) body))
                          (nonterminals (lambda (left*) body*))
                          ...)
                       (unless (start prg)
                         (error 'name
                           (string-append
                             "grammar verification failed"
                             "\nGrammar: ~a\nProblems:\n ~a\n")
                           (with-output-to-string
                             (lambda () (pretty-print
                                     (list
                                       '(left right* ...)
                                       '(left* right** ...)
                                       ...))))
                           (with-output-to-string
                             (lambda () (pretty-print problems)))))
                       prg)
                     prg)))))))))

(define-syntax (grammar-transforms x)

  (define (lookup-nt inp parent)
    (syntax-case parent ()
      (()
       (error 'lookup-nt
         "Missing nonterminal for inheritance" inp))
      (((nt . t*) . rest)
       (if (eq? (syntax->datum #'nt) (syntax->datum inp))
           #'(nt . t*)
           (lookup-nt inp #'rest)))))
  
  (define (add-nts parent child)
    (syntax-case child ()
      (() #'())
      ((nt . rest)
       #`(#,(lookup-nt #'nt parent) .
          #,(add-nts parent #'rest)))))
  
  (define syntax->datum*
    (lambda (expr)
      (let ((expr (syntax->datum expr)))
        (if (pair? expr)
            (map syntax->datum* expr)
            expr))))
  
  (define (find-connected t* sclause*)
    (cond
      ((null? sclause*) t*)
      ((let ((nt (car sclause*)))
         (let ((nt (syntax->datum nt)))
           (and (memq (car nt) t*) (cdr nt))))
       =>
       (lambda (sclause)
         (let loop ((nt* (filter* nonterminal? sclause))
                    (t^ t*))
           (cond
             ((null? nt*) (find-connected t^ (cdr sclause*)))
             ((memq (car nt*) t^) (loop (cdr nt*) t^))
             (else (loop (cdr nt*) (cons (car nt*) t^)))))))
      (else (find-connected t* (cdr sclause*)))))
  
  (define add-used-statics
    (lambda (t* sclause*)
      (let ((fsc* (map syntax->datum* sclause*)))
        (let loop ((t* (filter* nonterminal?
                         (map syntax->datum* t*))))
          (let ((t^ (find-connected t* sclause*)))
            (if (eq? t* t^)
                (filter
                  (lambda (sclause)
                    (memq (car (syntax->datum sclause)) t^))
                  sclause*)
                (loop t^)))))))

  (define (add-inherits passes)
    (syntax-case passes (%inherits)
      (((pass . grammar))
       #'((pass . grammar)))
      (((parent . grammar)
        (child (%inherits . i-nt*) clause* ...) . rest)
       (with-syntax ((inherited (add-nts #'grammar #'i-nt*)))
         #`((parent . grammar) .
            #,(add-inherits
                #'((child clause* ... . inherited) . rest)))))
      (((pass . grammar) . rest)
       #`((pass . grammar) . #,(add-inherits #'rest)))))
  
  (define (add-static sclause*)
    (lambda (pass)
      (syntax-case pass ()
        ((pass (nt* t* ...) ...)
         #`(pass (nt* t* ...) ... .
             #,(add-used-statics #'(t* ... ...) sclause*))))))
  
  (syntax-case x (%static)
    ((_ (%static sclause* ...) passes ...)
     (with-syntax (((passes ...)
                    (add-inherits #'(passes ...))))
       (with-syntax ((((pass (nt t* ...) ...) ...)
                      (map (add-static #'(sclause* ...))
                        #'(passes ...))))
         #'(begin (generate-verify pass (nt t* ...) ...) ...))))
    ((_ passes ...)
     (with-syntax ((((pass (nt t* ...) ...) ...)
                    (add-inherits #'(passes ...))))
       #'(begin (generate-verify pass (nt t* ...) ...) ...)))))

)

;; (import (util verify-grammar))

;; (let ()
;;   (grammar-transforms
;;     (%static (Var symbol))

;;     (first-grammar
;;       (Expr
;;         (let ((Var Expr)) Expr)
;;         (lambda (Var) Expr)
;;         (Expr Expr)
;;         Var))

;;     (second-grammar
;;       (Expr
;;         (lambda (Var) Expr)
;;         (Expr Expr)
;;         Var))

;;     (third-grammar
;;       (%inherits Expr)
;;       (Start Expr)
;;       (Integer integer))

;;     (fourth-grammar
;;       (%inherits Integer)
;;       (Expr
;;         Integer
;;         Var
;;         (lambda (Var *) Expr)
;;         (Expr Expr)))
;;     )

;;   (verify-first-grammar 'x)
;;   (verify-first-grammar '(lambda (x) x))
;;   (verify-first-grammar '(x y))
;;   (verify-first-grammar '(let ((x y)) ((lambda (x) x) y)))

;;   (verify-second-grammar 'x)
;;   (verify-second-grammar '(lambda (x) x))
;;   (verify-second-grammar '(x y))
;;   (verify-second-grammar '((x y) (x y)))

;;   ;; (grammar-transforms
;;   ;;   (fifth-grammar
;;   ;;     (Expr
;;   ;;       (lambda (Var) Expr)
;;   ;;       (Expr Expr)
;;   ;;       Var)
;;   ;;     (Var symbol))

;;   ;;   (sixth-grammar
;;   ;;     (%inherits Var)
;;   ;;     (Expr
;;   ;;       (lambda (Var *) Expr)
;;   ;;       (Expr Expr)
;;   ;;       Var
;;   ;;       Integer)
;;   ;;     (Integer integer))
;;   ;;   )

;;   ;; (verify-fifth-grammar '(lambda (x) x))

;;   ;; (verify-sixth-grammar 5)
;;   ;; (verify-sixth-grammar '(lambda (x y) 5))
;;   ;; (verify-sixth-grammar '(lambda (x y) ((x 5) (y 6))))

;;   (printf "All expressions verified\n")

;; )
