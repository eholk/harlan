(define-syntax lambdag@ ;;;  and lambdam@
  (syntax-rules (:)
    ((_ (a : s d c) b) (lam* a (s d c) () ((car a) (cadr a) (cddr a)) b))
    ((_ (a) b) (lambda (a) b)))) ;;; This could be a call to lam* with (! ! !) instead of (s d c)

(define-syntax lam*
  (syntax-rules (!)
    ((_ a () p () b) (lambda (a) (let p b)))
    ((_ a (! x ...) p (f0 f ...) b) (lam* a (x ...) p (f ...) b))
    ((_ a (x0 x ...) (p ...) (f0 f ...) b) (lam* a (x ...) (p ... (x0 f0)) (f ...) b))))

;; > (print-gensym #f)
;; > (expand '(lambdag@ (a : ! ! c) (+ a s d c)))
;; (lambda (a) (let ([c (#2%cddr a)]) (#2%+ a s d c)))
;; > (print-gensym #t)
;; > (expand '(lambdag@ (a : ! ! c) (+ a s d c)))
;; (lambda (#{a cclif7oaau1wf3oe6720i2-69})
;;   (let ([#{c cclif7oaau1wf3oe6720i2-70} (#2%cddr #{a cclif7oaau1wf3oe6720i2-69})])
;;     (#2%+ #{a cclif7oaau1wf3oe6720i2-69} s d #{c cclif7oaau1wf3oe6720i2-70})))
