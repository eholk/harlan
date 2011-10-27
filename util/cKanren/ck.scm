(library
  (util cKanren ck)

  (export
    ;; goals
    build-oc
    ==
    succeed
    fail
    prt
    prefix-s
    ext-d
    ext-c
    identitym
    oc->rands
    oc->proc
    oc->rator
    any/var?
    lambdam@
    composem
    goal-construct
    
    ;; framework
    run-constraints
    run
    run*
    
    ;; parameters
    process-prefix
    enforce-constraints
    reify-constraints)

(import
  (rnrs)
  (only (chezscheme)
    make-parameter
    pretty-print)
  (util cKanren mk))

(define-syntax lambdam@
  (syntax-rules (:)
    ((_ (a : s d c) body)
     (lambda (a)
       (let ((s (car a)) (d (cadr a)) (c (cddr a)))
         body)))
    ((_ (a) body) (lambda (a) body))))

(define identitym (lambdam@ (a) a))

(define composem
  (lambda (fm f^m)
    (lambdam@ (a)
      (let ((a (fm a)))
        (and a (f^m a))))))

(define goal-construct
  (lambda (fm)
    (lambdag@ (a)
      (cond
        ((fm a) => unitg)
        (else (mzerog))))))

(define-syntax build-oc
  (syntax-rules ()
    ((_ op-c arg ...)
     (build-oc-aux op-c (arg ...) () (arg ...)))))

(define-syntax build-oc-aux  ;;; (op-c z ...) evaluates to a seq.
  (syntax-rules ()
    ((_  op-c () (z ...) (arg ...))
     (let ((z arg) ...)
       `(,(op-c z ...) op-c ,z ...)))
    ((_ op-c (arg0 arg ...) (z ...) args)
     (build-oc-aux op-c (arg ...) (z ... q) args))))

(define process-prefix (make-parameter 'dummy))
(define enforce-constraints (make-parameter 'dummy))
(define reify-constraints (make-parameter 'dummy))
    
(define oc->proc car)
(define oc->rands cddr)
(define oc->rator cadr)
(define ext-d (lambda (x fd d) (cons `(,x . ,fd) d)))
(define ext-c
  (lambda (oc c)
    (cond
     ((any/var? (oc->rands oc)) (cons oc c))
     (else c))))

(define any/var?
  (lambda (p)
    (cond
      ((var? p) #t)
      ((pair? p)
       (or (any/var? (car p)) (any/var? (cdr p))))
      (else #f))))

(define == (lambda (u v) (goal-construct (==-c u v))))

(define ==-c  ;;; returns an ma if ((process-prefix) p c) => seq.
  (lambda (u v)
    (lambdam@ (a : s d c)
      (cond
        ((unify `((,u . ,v)) s)
         => (lambda (s^)
              (cond
                ((eq? s s^) a)
                (else
                 (let ((p (prefix-s s s^))
                       (a (make-a s^ d c)))
                   (((process-prefix) p c) a))))))
        (else #f)))))

(define prefix-s
  (lambda (s s^)
    (cond
      ((null? s) s^)
      (else (let loop ((s^ s^))
              (cond
                ((eq? s^ s) '())
                (else (cons (car s^) (loop (cdr s^))))))))))
    
(define succeed (== #f #f))
(define fail (== #f #t))
(define prt (lambda (a) (pretty-print a) (succeed a)))

(define run-constraints0 ;;; unitm is a sequel
  (lambda (x*-ignored c)
    (cond
      ((null? c) identitym)
      (else
       (composem (oc->proc (car c))
         (run-constraints0 x*-ignored (cdr c)))))))

(define run-constraints1 ;;; unitm is a sequel
  (lambda (x* c)
    (cond
      ((null? c) identitym)
      ((any-relevant/var? (oc->rands (car c)) x*)
       (composem (oc->proc (car c))
         (run-constraints1 x* (cdr c))))
      (else (run-constraints1 x* (cdr c))))))

(define run-constraints ;;; unitm is a sequel
  (lambda (x* c)
    (cond
      ((null? c) identitym)
      ((any-relevant/var? (oc->rands (car c)) x*)
       (composem (rem/run (car c))
         (run-constraints x* (cdr c))))
      (else (run-constraints x* (cdr c))))))

(define rem/run  ;;; returns a seq.
  (lambda (oc)
    (lambdam@ (a : s d c)
      (cond
        ((memq oc c)
         (let ((c^ (remq oc c)))
           ((oc->proc oc) (make-a s d c^))))
        (else a)))))

(define any-relevant/var?
  (lambda (t x*)
    (cond
      ((var? t) (memq t x*))
      ((pair? t)
       (or (any-relevant/var? (car t) x*)
           (any-relevant/var? (cdr t) x*)))
      (else #f))))

;; (define smaller
;;   (lambda (d seen)
;;     (cond
;;       ((null? d) '())
;;       ((memq (caar d) seen) (smaller (cdr d) seen))
;;       (else
;;        (cons (car d) (smaller (cdr d) (cons (caar d) seen)))))))
;; (define rem/run   ;;; For tracing.
;;   (lambda (oc)
;;     (lambdam@ (a : s d c)
;;       (begin
;;         (write (smaller d '()))
;;         (newline)
;;         (write (smaller c '()))
;;         (newline)
;;         (newline)
;;         (cond
;;         ((memq oc c)
;;          (let ((c^ (remq oc c)))
;;            ((oc->proc oc) (make-a s d c^))))
;;         (else a))
;;         )
;;       )))

(define-syntax run
  (syntax-rules ()
    ((_ n (x) g0 g ...)
     (take n
       (lambdaf@ ()
         ((fresh (x) g0 g ... (reify x))
          empty-a))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x) g ...) (run #f (x) g ...))))

(define reify
  (lambda (x)
    (fresh ()
      ((enforce-constraints) x)
      (lambdag@ (a : s d c)
        (choiceg
          (let* ((v (walk* x s))
                 (r (reify-s v empty-s)))
            (cond
              ((null? r) v)
              (else
               (let ((v (walk* v r)))
                 (cond
                   ((null? c) v)
                   (else (((reify-constraints) v r) a)))))))
          empty-f)))))

 )




