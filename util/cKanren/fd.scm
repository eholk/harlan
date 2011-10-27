(library
  (fd)

  (export
    ;;; goals
    infd
    domfd
    =/=fd
    <=fd
    <fd
    plusfd
    all-difffd

    ;;; helpers
    range
    usefd

    ;;; for composition
    process-prefixfd
    enforce-constraintsfd
    reify-constraintsfd)

  (import
    (rnrs)
    (only (chezscheme) trace-define)
    (mk)
    (sminimk))

;;; little helpers

(define range
  (lambda (lb ub)
    (cond
      ((< lb ub) (cons lb (range (+ lb 1) ub)))
      (else (cons lb '())))))

;;; domains (sorted lists of integers)

(define value-dom?
  (lambda (v)
    (and (integer? v) (<= 0 v))))

(define make-dom
  (lambda (n*)  ;;; n* should be a non-empty sorted (small to large) list
                ;;; of value-doms?, with no duplicates.
    n*))

(define list-sorted?
  (lambda (pred ls)
    (cond
      ((or (null? ls) (null? (cdr ls))) #t)
      ((pred (car ls) (cadr ls))
       (list-sorted? pred (cdr ls)))
      (else #f))))

(define list-insert
  (lambda (pred x ls)
    (cond
      ((null? ls) (cons x '()))
      ((pred x (car ls)) (cons x ls))
      (else (cons (car ls) (list-insert pred x (cdr ls)))))))
  
(define map-sum
  (lambda (f)
    (letrec
      ((loop
         (lambda (ls)
           (cond
             ((null? ls) fail)
             (else
               (conde
                 ((f (car ls)))
                 ((loop (cdr ls)))))))))
      loop)))

(define null-dom?
  (lambda (x)
    (null? x)))

(define singleton-dom?
  (lambda (dom)
    (null? (cdr dom))))

(define singleton-element-dom
  (lambda (dom)
    (car dom)))

(define min-dom
  (lambda (dom)
    (car dom)))

(define max-dom
  (lambda (dom)
    (cond
      ((null? (cdr dom)) (car dom))
      (else (max-dom (cdr dom))))))

(define memv-dom?
  (lambda (v dom)
    (and (value-dom? v) (memv v dom))))

(define intersection-dom
  (lambda (dom1 dom2)
    (cond
      ((or (null? dom1) (null? dom2)) '())
      ((= (car dom1) (car dom2))
       (cons (car dom1)
         (intersection-dom (cdr dom1) (cdr dom2))))
      ((< (car dom1) (car dom2))
       (intersection-dom (cdr dom1) dom2))
      (else (intersection-dom dom1 (cdr dom2))))))

(define diff-dom
  (lambda (dom1 dom2)
    (cond
      ((or (null? dom1) (null? dom2)) dom1)
      ((= (car dom1) (car dom2))
       (diff-dom (cdr dom1) (cdr dom2)))
      ((< (car dom1) (car dom2))
       (cons (car dom1) (diff-dom (cdr dom1) dom2)))
      (else (diff-dom dom1 (cdr dom2))))))

(define copy-before   
  (lambda (pred dom)
    (cond
      ((null? dom) '())
      ((pred (car dom)) '())
      (else (cons (car dom) (copy-before pred (cdr dom)))))))

(define drop-before
  (lambda (pred dom)
    (cond
      ((null? dom) '())
      ((pred (car dom)) dom)
      (else (drop-before pred (cdr dom))))))

(define disjoint-dom?
  (lambda (dom1 dom2)
    (cond
      ((or (null? dom1) (null? dom2)) #t)
      ((= (car dom1) (car dom2)) #f)
      ((< (car dom1) (car dom2))
       (disjoint-dom? (cdr dom1) dom2))
      (else (disjoint-dom? dom1 (cdr dom2))))))

;;; procedures below this point cannot
;;; expose the representations of doms!

(define get-dom
  (lambda (x d)
    (cond
      ((assq x d) => rhs)
      (else #f))))

(define process-dom
  (lambda (v dom)
    (lambdam@ (a)
      (cond
        ((var? v) ((update-var-dom v dom) a))
        ((memv-dom? v dom) a)
        (else #f)))))

(define update-var-dom 
  (lambda (x dom)
    (lambdam@ (a : s d c)
      (cond
        ((get-dom x d)
         => (lambda (xdom)
              (let ((i (intersection-dom xdom dom)))
                (cond
                  ((null-dom? i) #f)
                  (else ((resolve-storable-dom i x) a))))))
        (else ((resolve-storable-dom dom x) a))))))

(define resolve-storable-dom
  (lambda (dom x)
    (lambdam@ (a : s d c)
      (cond
        ((singleton-dom? dom)
         (let* ((n (singleton-element-dom dom))
                (a (make-a (ext-s x n s) d c)))
           ((run-constraints `(,x) c) a)))
        (else (make-a s (ext-d x dom d) c))))))

(define force-ans
  (lambda (x)
    (lambdag@ (a : s d c)
      (let ((x (walk x s)))
        ((cond
           ((and (var? x) (get-dom x d))
            => (map-sum (lambda (v) (== x v))))
           ((pair? x)
            (fresh ()
              (force-ans (car x))
              (force-ans (cdr x))))
           (else succeed))
         a)))))

(define-syntax let-dom
  (syntax-rules (:)
    ((_ (s d) ((u : udom) ...) body)
     (let ((u (walk u s)) ...)
       (let ((udom
               (cond
                 ((var? u) (get-dom u d))
                 (else (make-dom `(,u)))))
             ...)
         body)))))

(define =/=fd-c
  (lambda (u v)
    (lambdam@ (a : s d c)
      (let-dom (s d) ((u : udom) (v : vdom))
        (cond
          ((or (not udom) (not vdom))
           (make-a s d (ext-c (build-oc =/=fd-c u v) c)))
          ((and (singleton-dom? udom)
                (singleton-dom? vdom)
                (= (singleton-element-dom udom)
                   (singleton-element-dom vdom)))
           #f)
          ((disjoint-dom? udom vdom) a)
          (else
           (let* ((c^ (ext-c (build-oc =/=fd-c u v) c))
                  (a (make-a s d c^)))
             (cond
               ((singleton-dom? udom)
                ((process-dom v (diff-dom vdom udom)) a))
               ((singleton-dom? vdom)
                ((process-dom u (diff-dom udom vdom)) a))
               (else a)))))))))

(define all-difffd-c
  (lambda (v*/x)
    (lambdam@ (a : s d c)
      (let ((v*/x (walk v*/x s)))
        (cond
          ((var? v*/x)
           (let* ((oc (build-oc all-difffd-c v*/x)))
             (make-a s d (ext-c oc c)))) 
          (else (let-values (((x* n*) (partition var? v*/x)))
                  (let ((n* (list-sort < n*)))
                    (cond
                      ((list-sorted? < n*)
                       ((all-diff/fd-c x* n*) a))
                      (else #f))))))))))

(define all-diff/fd-c
  (lambda (y* n*)
    (lambdam@ (a : s d c)
      (let loop ((y* y*) (n* n*) (x* '()))
        (cond
          ((null? y*)
           (let* ((oc (build-oc all-diff/fd-c x* n*))
                  (a (make-a s d (ext-c oc c))))
             ((exclude-from-dom (make-dom n*) d x*) a)))
          (else
            (let ((y (walk (car y*) s)))
              (cond
                ((var? y) (loop (cdr y*) n* (cons y x*)))
                ((memv-dom? y n*) #f)
                (else (let ((n* (list-insert < y n*)))
                        (loop (cdr y*) n* x*)))))))))))

(define exclude-from-dom
  (lambda (dom1 d x*)
    (let loop ((x* x*))
      (cond
        ((null? x*) identitym)
        ((get-dom (car x*) d)
         => (lambda (dom2)
              (composem
                (process-dom (car x*) (diff-dom dom2 dom1))
                (loop (cdr x*)))))
        (else (loop (cdr x*)))))))

(define-syntax c-op  ;;; returns sequeal.
  (syntax-rules (:)
    ((_ op ((u : udom) ...) body)
     (lambdam@ (a : s d c)
       (let-dom (s d) ((u : udom) ...)
         (let* ((c (ext-c (build-oc op u ...) c))
                (a (make-a s d c)))
           (cond
             ((and udom ...) (body a))
             (else a))))))))

;; (define =fd-c
;;   (lambda (u v)
;;     (c-op =fd ((u : udom) (v : vdom))
;;       (let ((i (intersection-dom udom vdom)))
;;         (composem
;;           (process-dom u i)
;;           (process-dom v i))))))

(define <=fd-c
  (lambda (u v)
    (c-op <=fd-c ((u : udom) (v : vdom))
      (let ((umin (min-dom udom))
            (vmax (max-dom vdom)))
        (composem
          (process-dom u
            (copy-before (lambda (u) (< vmax u)) udom))
          (process-dom v
            (drop-before (lambda (v) (<= umin v)) vdom)))))))

(define plusfd-c
  (lambda (u v w)
    (c-op plusfd-c ((u : udom) (v : vdom) (w : wdom))
      (let ((wmin (min-dom wdom)) (wmax (max-dom wdom))
            (umin (min-dom udom)) (umax (max-dom udom))
            (vmin (min-dom vdom)) (vmax (max-dom vdom)))
        (composem
          (process-dom w
            (range (+ umin vmin) (+ umax vmax)))
          (composem
            (process-dom u
              (range (- wmin vmax) (- wmax vmin)))
            (process-dom v
              (range (- wmin umax) (- wmax umin)))))))))

(define process-prefixfd
  (lambda (p c)
    (cond
      ((null? p) identitym)
      (else
        (let ((x (lhs (car p))) (v (rhs (car p))))
          (let ((t (composem
                     (run-constraints `(,x) c)
                     (process-prefixfd (cdr p) c))))
            (lambdam@ (a : s d c)
              (cond
                ((get-dom x d)
                 => (lambda (dom)
                      ((composem (process-dom v dom) t) a)))
                (else (t a))))))))))

(define enforce-constraintsfd
  (lambda (x)
    (fresh ()
      (force-ans x)
      (lambdag@ (a : s d c)
        (let ((bound-x* (map lhs d)))
          (verify-all-bound c bound-x*)
          ((onceo (force-ans bound-x*)) a))))))

(define verify-all-bound
  (lambda (c bound-x*)
    (unless (null? c)
      (cond
        ((find (lambda (x) (not (memq x bound-x*)))
           (filter var? (oc->rands (car c))))
         => (lambda (x)
              (error 'verify-all-bound
                "Constrained variable ~s without domain" x)))
        (else (verify-all-bound (cdr c) bound-x*))))))

(define reify-constraintsfd
  (lambda (m r)
    (error 'reify-constraintsfd "Unbound vars at end\n")))

;;; goals

(define domfd
  (lambda (x n*)
    (goal-construct (domfd-c x n*))))

(define domfd-c
  (lambda (x n*)
    (lambdam@ (a : s d c)
      ((process-dom (walk x s) (make-dom n*)) a))))

(define-syntax infd
  (syntax-rules ()
    ((_ x0 x ... e)
     (let ((n* e))
       (fresh () (domfd x0 n*) (domfd x n*) ...)))))

;; (define =fd
;;   (lambda (u v)
;;     (goal-construct (=fd-c u v))))

(define =/=fd
  (lambda (u v)
    (goal-construct (=/=fd-c u v))))

(define <fd
  (lambda (u v)
    (fresh () (<=fd u v) (=/=fd u v))))

(define <=fd
  (lambda (u v)
    (goal-construct (<=fd-c u v))))

(define plusfd
  (lambda (u v w)
    (goal-construct (plusfd-c u v w))))

(define all-difffd
  (lambda (v*/x)
    (goal-construct (all-difffd-c v*/x))))

;;; to use the fd definitions, invoke (usefd)

(define usefd
  (lambda ()
    (process-prefix process-prefixfd)
    (enforce-constraints enforce-constraintsfd)
    (reify-constraints reify-constraintsfd)))

)

