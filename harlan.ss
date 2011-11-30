
(define-syntax kernel
  (syntax-rules ()
    ((_ ((x xs) ...) body)
     (vector-map (lambda (x ...) body) xs ...))))

(define A '#(1 2 3 4))
(define B '#(5 6 7 8))

(kernel ((a A) (b B)) (+ a b))
