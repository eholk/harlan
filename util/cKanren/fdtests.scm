;; Copyright (C) 2011 by Claire E. Alvis, Jeremiah J. Willcock, Kyle M. Carter,
;; William E. Byrd, Daniel P. Friedman
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.


(load "mk.scm")
(load "ck.scm")
(load "fd.scm")

(import
  (mk)
  (ck)
  (fd))

(usefd)

(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (cout "Testing " title nl)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (errorf 'test-check
               "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
               'tested-expression expected produced)))))))

(define nl (string #\newline))

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

(define errorf
  (lambda (tag . args)
    (printf "Failed: ~s: ~%" tag)
    (apply printf args)
    (error 'WiljaCodeTester "That's all, folks!")))

(test-check "-1"
  (run* (q)
    (fresh (w x y z)
      (infd w z (range 1 5))
      (all-difffd q)
      (== q `(,x ,y ,z))
      (== `(,x 2) `(1 ,y))
      (plusfd x y w)
      (plusfd w y z)))
  '((1 2 5)))

;;; #!eof  (when tracing, uncomment)

(test-check "0"
  (run* (q)
    (fresh (a b c)
      (all-difffd q)
      (== q `(,a ,b ,c))
      (infd a b c '(1 2 3))
      (== a 1)
      (== b 2)
      (<=fd c 5)))
  '((1 2 3)))

(test-check "1^^"
  (run* (x)
    (infd x '(1 2))
    prt)
  '(1 2))

(test-check "1^"
  (run* (x)
    (infd x '(1 2))
    (=/=fd x 1))
  `(2))

(test-check "1"
  (run* (q)
    (fresh (x)
      (infd x '(1 2))
      (=/=fd x 1)
      (== x q)))
  `(2))

(test-check "2"
  (run* (q)
    (fresh (x y z)
      (infd x '(1 2 3))
      (infd y '(3 4 5))
      (== x y)
      (infd z '(1 3 5 7 8))
      (infd z '(5 6))
      (== z 5)
      (== q `(,x ,y ,z))))
  `((3 3 5)))

(test-check "3"
  (run* (q)
    (fresh (x y z)
      (infd x '(1 2 3))
      (infd y '(3 4 5))
      (== x y)
      (infd z '(1 3 5 7 8))
      (infd z '(5 6))
      (== z x)
      (== q `(,x ,y ,z))))
  '())

(test-check "4"
  (run* (q)
    (fresh (x y z) 
      (infd x '(1 2))
      (infd y '(2 3))
      (infd z '(2 4))
      (== x y)
      (=/=fd x z)
      (== q z)))
  `(4))

(test-check "4.1"
  (run* (q)
    (fresh (x y z) 
      (== x y)
      (infd y '(2 3))
      (=/=fd x z)
      (infd z '(2 4))
      (== q z)
      (infd x '(1 2))))
  `(4))

(test-check "5"
  (run* (q)
    (fresh (x y)
      (infd x '(1 2 3))
      (infd y '(0 1 2 3 4))
      (<fd x y)
      (=/=fd x 1)
      (== y 3)
      (== q `(,x ,y))))
  `((2 3)))

(test-check "6"
  (run* (q)
    (fresh (x y)
      (infd x '(1 2))
      (infd y '(2 3))
      (== x y)
      (== q `(,x ,y))))
  `((2 2)))

(test-check "7"
  (run* (q)
    (fresh (x y z)
      (infd x y z '(1 2))
      (=/=fd x y)
      (=/=fd x z)
      (=/=fd y z))
    (infd q '(5)))
  `())

(test-check "8"
  (run* (q)
    (fresh (x) (infd x '(1 2)))
    (infd q '(5)))
  `(5))

(test-check "9"
  (run* (q)
    (== q #t))
  `(#t))

(test-check "10"
  (run* (q)
    (infd q '(1 2))
    (== q #t))
  `())

(test-check "11"
  (run* (q)
    (== q #t)
    (infd q '(1 2)))
  `())

(test-check "12"
  (run* (q)
    (fresh (x)
      (<=fd x 5)
      (infd x (range 0 10))
      (== q x)))
  `(0 1 2 3 4 5))

(test-check "13"
  (run* (q)
    (fresh (x y z)
      (infd x y z (range 0 9))
      (=/=fd x y)
      (=/=fd y z)
      (=/=fd x z)
      (== x 2)
      (== q 3)
      (plusfd y 3 z)))
  `(3))

(test-check "14"
  (run* (q)
    (fresh (x y z)
      (infd x y z (range 0 2))
      (all-difffd `(,x ,y ,z))
      (== q `(,x ,y ,z))))
  `((0 1 2) (0 2 1) (1 0 2) (2 0 1) (1 2 0) (2 1 0)))

(test-check "15"
  (run* (q)
    (fresh (a b c x)
      (infd a b c (range 1 3))
      (all-difffd `(,a ,b ,c))
      (=/=fd c x)
      (<=fd b 2)
      (== x 3)
      (== q `(,a ,b ,c))))
  '((3 1 2) (3 2 1)))

(define long-addition-stepo
  (lambda (augend addend carry-in carry digit)
    (fresh (partial-sum sum)
      (infd partial-sum (range 0 18))
      (infd sum (range 0 19))
      (plusfd augend addend partial-sum)
      (plusfd partial-sum carry-in sum)
      (conde
        ((<fd 9 sum) (== carry 1) (plusfd digit 10 sum))
        ((<=fd sum 9) (== carry 0) (== digit sum))))))

;;; 34 + 89

(run* (q)
  (fresh (digit1 digit2 carry0 carry1)
    (infd carry0 carry1 (range 0 1))
    (infd digit1 digit2 (range 0 9))
    (long-addition-stepo 4 9 0 carry0 digit1)
    (long-addition-stepo 3 8 carry0 carry1 digit2)
    (== q `(,carry1 ,digit2 ,digit1))))

;;; ((1 2 3))
  
(define send-more-moneyo
  (lambda (letters)
    (fresh (s e n d m o r y carry0 carry1 carry2)
      (== letters `(,s ,e ,n ,d ,m ,o ,r ,y))
      (all-difffd letters)
      (infd s m (range 1 9))
      (infd e n d o r y (range 0 9))
      (infd carry0 carry1 carry2 (range 0 1))      
      (long-addition-stepo d e 0 carry0 y)
      (long-addition-stepo n r carry0 carry1 e)
      (long-addition-stepo e o carry1 carry2 n)
      (long-addition-stepo s m carry2 m o))))

(define send-more-moneyo
  (lambda (letters)
    (fresh (s e n d m o r y carry0 carry1 carry2)
      (== letters `(,s ,e ,n ,d ,m ,o ,r ,y))
      (all-difffd letters)
      (infd s m (range 1 9))
      (infd e n d o r y (range 0 9))
      (infd carry0 carry1 carry2 (range 0 1))      
      (long-addition-stepo s m carry2 m o)
      (long-addition-stepo e o carry1 carry2 n)
      (long-addition-stepo n r carry0 carry1 e)
      (long-addition-stepo d e 0 carry0 y))))

(define smm
  (lambda ()
    (run* (q) (send-more-moneyo q))))

;; (define diagonals
;;   (lambda (n l)
;;     (let loop ((r l) (s (cdr l)) (i 0) (j 1))
;;       (cond
;;         ((or (null? r) (null? (cdr r))) succeed)
;;         ((null? s) (loop (cdr r) (cddr r) (+ i 1) (+ i 2)))
;;         (else
;;           (let ((qi (car r)) (qj (car s)))
;;             (fresh (si sj)
;;               (infd si sj (range 0 (* 2 n)))
;;               (=/=fd qi sj)
;;               (=/=fd qj si)
;;               (plusfd qi (- j i) si)
;;               (plusfd qj (- j i) sj)
;;               (loop r (cdr s) i (+ j 1)))))))))

;; (define n-queens
;;   (lambda (n)
;;     (lambda ()
;;       (run* (q)
;;         (let loop ((i n) (l '()))
;;           (cond
;;             ((zero? i)
;;              (fresh ()
;;                (== q l)
;;                (all-difffd l)
;;                (diagonals n l)))
;;             (else
;;               (fresh (x)
;;                 (infd x (range 1 n))
;;                 (loop (sub1 i) (cons x l))))))))))

;; (printf "~s\n" (time (smm)))
;; (let ((ans (time ((n-queens 8)))))
;;   (printf "~s\n" (length ans)))



;; (define n-queens
;;   (lambda (n)
;;     (run* (q)
;;       (let loop ((i n) (l '()))
;;         (cond
;;           ((zero? i)
;;            (fresh ()
;;              (== q l)
;;              (all-difffd l)
;;              (diagonals n l)))
;;           (else
;;            (fresh (x)
;;              (infd x (range 1 n))
;;              (loop (sub1 i) (cons x l)))))))))

(define diagonals
  (let ((range0-16 (range 0 (* 2 8))))
    (lambda (l)
      (let loop ((r l) (s (cdr l)) (i 0) (j 1))
        (cond
          ((null? (cdr r)) succeed)
          ((null? s)
           (let ((r (cdr r)))
             (loop r (cdr r) (+ i 1) (+ i 2))))
          (else
           (let ((qi (car r)) (qj (car s)))
             (fresh (si sj)
               (infd si sj range0-16)
               (=/=fd qi sj)
               (=/=fd qj si)
               (plusfd qi (- j i) si)
               (plusfd qj (- j i) sj)
               (loop r (cdr s) i (+ j 1))))))))))

;; (define driveqo
;;   (let ((range1-8 (range 1 8)))
;;     (lambda (q count acc)
;;       (cond
;;         ((zero? count)
;;          (fresh ()
;;            (== q acc)  ;;; q is a list of 8 variables in the range 1-8.
;;            (all-difffd acc) ;; Now make sure they are never the same.
;;            (diagonals acc))) ;;; 
;;         (else
;;          (fresh (x)
;;            (infd x range1-8)   ;; x is either column or row.
;;            (driveqo q (sub1 count) (cons x acc))))))))

;; driveqo creates a list of 8 variables, each of whose domain is in the range 1-8
;; and no two of these variables can be bound to the same value.

(define diagonals
  (lambda (x*)
    (let row ((x* x*) (i 0) (j 1))
      (cond
        ((null? x*) succeed)
        (else
         (let ((qi (car x*)) (x^* (cdr x*)))
           (let column ((y* x^*) (j j))
             (cond
               ((null? y*) (row x^* (+ i 1) (+ i 2)))
               (else
                (fresh ()
                  (aux-diagonal qi (car y*) (- j i))
                  (column (cdr y*) (+ j 1))))))))))))

(define aux-diagonal
  (let ((rng (range 0 (* 2 8))))
    (lambda (qi qj d)
      (fresh (si sj)
        (infd si sj rng)
        (=/=fd qi sj)
        (=/=fd qj si)
        (plusfd qi d si)
        (plusfd qj d sj)))))

(define testq
  (lambda ()
    (length
      (run* (q)
        (fresh (x0 x1 x2 x3 x4 x5 x6 x7)
          (let ((x* `(,x0 ,x1 ,x2 ,x3 ,x4 ,x5 ,x6 ,x7)))
            (fresh ()
              (dom*fd x* (range 1 8))
              (all-difffd x*)
              (diagonals x*)
              (== q x*))))))))

;; Of course, it is trivial to generate x* recursively if we pass in N.

;; (printf "~s\n" (time (smm)))
;; (printf "~s\n" (time (testq)))

(define n-queenso
  (lambda (q n)
    (let loop ((i n) (l '()))
      (cond
        ((zero? i)
         (fresh ()
           (== q l)
           (all-difffd l)
           (diagonalso n l)))
        (else
         (fresh (x)
           (infd x (range 1 n))
           (loop (sub1 i) (cons x l))))))))

(define diagonalso
  (lambda (n l)
    (let loop ((r l) (s (cdr l)) (i 0) (j 1))
      (cond
        ((or (null? r) (null? (cdr r))) succeed)
        ((null? s) (loop (cdr r) (cddr r) (+ i 1) (+ i 2)))
        (else
          (let ((qi (car r)) (qj (car s)))
            (fresh ()
              (diago qi qj (- j i) (range 0 (* 2 n)))
              (loop r (cdr s) i (+ j 1)))))))))

;; (define diago
;;   (lambda (qi qj d rng)
;;     (fresh (si sj)
;;       (infd si sj rng)
;;       (=/=fd qi sj)
;;       (plusfd qi d si)
;;       (=/=fd qj si)
;;       (plusfd qj d sj))))

(define diago
  (lambda (qi qj d rng)
    (fresh (qi+d qj+d)
      (infd qi+d qj+d rng)
      (plusfd qi d qi+d)
      (=/=fd qi+d qj)
      (plusfd qj d qj+d)
      (=/=fd qj+d qi))))

(define n-queenso
  (lambda (q* n)
    (let loop ((i n) (l '()))
      (cond
        ((zero? i)
         (fresh ()
           (all-difffd l)
           (diagonalso n l)
           (== q* l)))
        (else (fresh (x)
                (infd x(range 1 n))
                (loop (sub1 i) (cons x l))))))))

(length (run* (q) (n-queenso q 8)))

(printf "~s\n" (time (smm)))
(printf "~s\n" (time (run 1 (q) (n-queenso q 8))))
(printf "~s\n" (time (length (run* (q) (n-queenso q 8)))))
