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
(load "neq.scm")
(load "fdneq.scm")

(import
  (mk)
  (ck)
  (fd)
  (fdneq))

(usefdneq)

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

(define diago
  (lambda (qi qj d rng)
    (fresh (si sj)
      (infd si sj rng)
      (=/=fd qi sj)
      (plusfd qi d si)
      (=/=fd qj si)
      (plusfd qj d sj))))

(define all-diffo
  (lambda (l)
    (conde
      ((== l '()))
      ((fresh (a) (== l `(,a))))
      ((fresh (a ad dd)
         (== l `(,a ,ad . ,dd))
         (=/= a ad)
         (all-diffo `(,a . ,dd))
         (all-diffo `(,ad . ,dd)))))))

(pretty-print (run* (q) (n-queenso q 8) (all-diffo q)))
(pretty-print
  (let ((answers (run* (q) (n-queenso q 4))))
    (run* (q) (all-diffo answers))))
(pretty-print (run* (q) (infd q '(2 3 4)) (all-diffo `(a 3 ,q))))