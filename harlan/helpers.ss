(library
  (harlan helpers)
  (export
    ident?
    reduceop?
    harlan-type?
    harlan-c-type?
    harlan-cl-type?
    member/var
    set-add/var
    remove/var
    union/var
    >::>)
  (import
    (rnrs)
    (except (elegant-weapons helpers) ident?)
    (harlan compile-opts)
    (elegant-weapons match))

  (define (reserved-word? x)
    (memq x
     '(kernel for while vector vector-ref reduce
       let assert set! iota make-vector length)))
  
  (define (ident? x)
    (and (symbol? x)
         (not (reserved-word? x))))

  (define (reduceop? op)
    (memq op '(+ *)))

  (define (harlan-type? t)
    (or (scalar-type? t)
        (case t
          ((ofstream region_ptr region) #t)
          (else #f))))
  
  (define (harlan-c-type? t)
    (or (c-type? t)
        (case t
          ((region_ptr region) #t)
          (else #f))))
  
  (define (harlan-cl-type? t)
    (or (cl-type? t)
        (case t
          ((cl::queue cl::kernel cl::program) #t)
          (else #f))))

(define (member/var x s)
  (cond
    ((null? s) #f)
    ((eq? (caddar s) x) #t)
    (else (member/var x (cdr s)))))

(define (remove/var x s)
  (cond
    ((null? s) `())
    ((eq? (caddar s) x) (cdr s))
    (else (cons (car s) (remove/var x (cdr s))))))

(define (difference/var s r)
  (cond
    ((null? s) `())
    ((member/var (car s) r)
     (difference/var (cdr s) r))
    (else (cons (car s)
            (difference/var (cdr s) r)))))

(define (set-add/var s x)
  (if (member/var (caddr x) s) s (cons x s)))

(define (union/var . s*)
  (cond
    ((null? s*) `())
    (else
      (let ((s (car s*)) (s* (cdr s*)))
        (fold-left
          (lambda (s1 s2) (fold-left set-add/var s1 s2))
          s s*)))))

  ;; I'm a horrible person for defining this.
  (define-syntax >::>
    (syntax-rules ()
      ((_ e) e)
      ((_ e (x a ...) e* ...)
       (>::> (x e a ...) e* ...))
      ((_ e x e* ...)
       (>::> (x e) e* ...)))))
