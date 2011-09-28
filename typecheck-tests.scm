(import (returnify) (typecheck) (harlancompiler) (mk))

(define-syntax test-unify
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (let* ((expected expected-result)
            (produced tested-expression))
       (if (unify expected produced '())
           (printf "~s works!\n" title)
           (error
             'test
             (format "Failed ~s: ~a\nExpected: ~a\nComputed: ~a"
                     title 'tested-expression expected produced)))))))

(test 'annotate-1
  (typecheck
    (lift-vectors
      (returnify
       '(module
          (fn main ()
            (return 0))))))
  '(module
     (fn main () (() -> int)
       (return (int 0)))))

(test 'annotate-2
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (print 42)
             (return 0))))))
  '(module
     (fn main () (() -> int)
       (print (int 42))
       (return (int 0)))))

(test 'annotate-4
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (print 42))))))
  '())

(test 'annotate-4a
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (print 42)
             (return 0))))))
  '(module
     (fn main () (() -> int)
       (print (int 42))
       (return (int 0)))))

(test-unify 'annotate-6
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (print (vector 1 2 3 4))
             (return 0))))))
  (let ((v (vector 'v)))
    `(module
       (fn main () (() -> int)
         (let ,v (vector int 4)
           (vector (int 1) (int 2) (int 3) (int 4)))
         (print (var (vector int 4) ,v))
         (return (int 0))))))

(test 'annotate-3
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (return 0)
             (print 42))))))
  '(module
     (fn main () (() -> int)
       (return (int 0))
       (print (int 42)))))

(test 'annotate-9
  (typecheck
    (lift-vectors
      (returnify
       '(module
          (fn main ()
              ;; implicit return
              5)))))
  '(module
     (fn main () (() -> int)
       (return (int 5)))))

(test 'annotate-10
  (typecheck
    (lift-vectors
      (returnify
       '(module
          (fn main ()
            (return 5))))))
  '(module
     (fn main () (() -> int)
       (return (int 5)))))

(test 'annotate-11
  (typecheck
    (lift-vectors
      (returnify
       '(module
          (fn main ()
            (let X 5)
            (var X))))))
  '(module
     (fn main () (() -> int)
       (let X int (int 5))
       (return (var int X)))))

(test 'annotate-11b
  (typecheck
    (lift-vectors
      (returnify
       '(module
          (fn main ()
            (let X 5)
            (return (var X)))))))
  '(module
     (fn main () (() -> int)
       (let X int (int 5))
       (return (var int X)))))


(test 'annotate-13
  ;; var-num            
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (let x 42)
             (print (var x))
             (return 0))))))
  '(module (fn main () (() -> int)
     (let x int (int 42))
     (print (var int x))
     (return (int 0)))))

(test 'annotate-14
  ;; multi-return
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (return 0)
             (return 1))))))
  '(module
     (fn main () (() -> int)
       (return (int 0))
       (return (int 1)))))


(test-unify 'annotate-16b
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (let v1 (vector 1 2))
             (let v2 (vector 3 4))
             (return 5))))))
  (let ((v (vector 'v))
        (w (vector 'w)))
    `(module
       (fn main () (() -> int)
         (let ,v (vector int 2) (vector (int 1) (int 2)))
         (let v1 (vector int 2) (var (vector int 2) ,v))
         (let ,w (vector int 2) (vector (int 3) (int 4)))
         (let v2 (vector int 2) (var (vector int 2) ,w))
         (return (int 5))))))

(test-unify 'annotate-16j
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (let v (vector (vector 1) (vector 2)))
             (return 5))))))
  (let ((v1 (vector 'v1)) (v2 (vector 'v2)) (v3 (vector 'v3)))
    `(module
       (fn main () (() -> int)
         (let ,v1 (vector int 1) (vector (int 1)))
	 (let ,v2 (vector int 1) (vector (int 2)))
	 (let ,v3 (vector (vector int 1) 2) 
	      (vector (var (vector int 1) ,v1) 
		      (var (vector int 1) ,v2)))
         (let v (vector (vector int 1) 2) 
	   (var (vector (vector int 1) 2) ,v3))
         (return (int 5))))))

(test 'annotate-19
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (assert (= 4 5))
             (return 0))))))
  '(module
     (fn main () (() -> int)
       (assert (= (int 4) (int 5)))
       (return (int 0)))))

(test-unify 'annotate-20
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (assert (= (vector 1 2) (vector 3 4)))
             (return 0))))))
  (let ((v (vector 'v))
        (w (vector 'w)))
    `(module
       (fn main () (() -> int)
         (let ,v (vector int 2) (vector (int 1) (int 2)))
         (let ,w (vector int 2) (vector (int 3) (int 4)))
         (assert (= (var (vector int 2) ,v) (var (vector int 2) ,w)))
         (return (int 0))))))

(test-unify 'annotate-21
  ;; simple-vector      
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (let X (vector 1 2 3 4))
             (assert (= (vector-ref X 0) 1))
             (assert (= (vector-ref X 1) 2))
             (assert (= (vector-ref X 2) 3))
             (assert (= (vector-ref X 3) 4))
             (return 0))))))
  (let ((v (vector 'v)))
    `(module
       (fn main () (() -> int)
         (let ,v (vector int 4) (vector (int 1) (int 2) (int 3) (int 4)))
         (let X (vector int 4) (var (vector int 4) ,v))
         (assert (= (vector-ref int (var (vector int 4) X) (int 0)) (int 1)))
         (assert (= (vector-ref int (var (vector int 4) X) (int 1)) (int 2)))
         (assert (= (vector-ref int (var (vector int 4) X) (int 2)) (int 3)))
         (assert (= (vector-ref int (var (vector int 4) X) (int 3)) (int 4)))
         (return (int 0))))))

(test-unify 'annotate-22
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (let X (vector 1 2 3 4))
             (return (vector-ref X 0)))))))
  (let ((v (vector 'v)))
    `(module
       (fn main () (() -> int)
         (let ,v (vector int 4) (vector (int 1) (int 2) (int 3) (int 4)))
         (let X (vector int 4) (var (vector int 4) ,v))
         (return (vector-ref int (var (vector int 4) X) (int 0)))))))

(test-unify 'annotate-simple-kernel
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (let X (vector 1 2 3 4))
             (let X (kernel ((x (var X))) (+ 1 (var x))))
             (assert (= (vector-ref X 0) 2))
             (assert (= (vector-ref X 1) 3))
             (assert (= (vector-ref X 2) 4))
             (assert (= (vector-ref X 3) 5))
             (return 0))))))
  (let ((g1 (vector 'g1))
        (g2 (vector 'g2)))
    `(module
       (fn main () (() -> int)
         (let ,g1 (vector int 4) (vector (int 1) (int 2) (int 3) (int 4)))
         (let X (vector int 4) (var (vector int 4) ,g1))
         (let ,g2 (vector int 4)
           (kernel (vector int 4)
             (((x int) ((var (vector int 4) X) (vector int 4))))
             (+ (int 1) (var int x))))
         (let X (vector int 4) (var (vector int 4) ,g2))
         (assert (= (vector-ref int (var (vector int 4) X) (int 0)) (int 2)))
         (assert (= (vector-ref int (var (vector int 4) X) (int 1)) (int 3)))
         (assert (= (vector-ref int (var (vector int 4) X) (int 2)) (int 4)))
         (assert (= (vector-ref int (var (vector int 4) X) (int 3)) (int 5)))
         (return (int 0))))))

(test 'annotate-5
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (return (vector 1 2 3 4)))))))
  '())

(test 'annotate-8a
  (typecheck
    (lift-vectors
      (returnify
       '(module
          (fn main ()
              ;; implicit return
              (vector 1 2 3 4))))))
  '())

(test-unify 'annotate-8b
  (typecheck
    (lift-vectors
      (returnify
       '(module
          (fn foo ()
              ;; implicit return
              (vector 1 2 3 4))))))
  (let ((v (vector 'v)))
    `(module
       (fn foo () (() -> (vector int 4))
         (let ,v (vector int 4)
           (vector (int 1) (int 2) (int 3) (int 4)))
         (return (var (vector int 4) ,v))))))

(test 'annotate-12a
  (typecheck
    (lift-vectors
      (returnify
       '(module
          (fn main ()
            (let X (vector 1 2 3))
            (var X))))))
  '())

(test-unify 'annotate-12b
  (typecheck
    (lift-vectors
      (returnify
       '(module
          (fn foo ()
            (let X (vector 1 2 3))
            (var X))))))
  ;; this answer seems more verbose than necessary
  (let ((v (vector 'v)))
    `(module (fn foo () (() -> (vector int 3))
       (let ,v (vector int 3) 
         (vector (int 1) (int 2) (int 3))) 
       (let X (vector int 3) (var (vector int 3) ,v))
       (return (var (vector int 3) X))))))


(test 'annotate-15a
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (return (= 4 5)))))))
  '())

(test 'annotate-15b
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn foo ()
             (return (= 4 5)))))))
  '(module
     (fn foo () (() -> bool)
       (return (= (int 4) (int 5))))))

(test 'annotate-16a1
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (let v1 (vector 1 2))
             (return (var v1)))))))
  '())

(test-unify 'annotate-16a2
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn foo ()
             (let v1 (vector 1 2))
             (return (var v1)))))))
  (let ((v (vector 'v)))
    `(module
       (fn foo () (() -> (vector int 2))
         (let ,v (vector int 2) (vector (int 1) (int 2)))
         (let v1 (vector int 2) (var (vector int 2) ,v))
         (return (var (vector int 2) v1))))))

(test 'annotate-16c1
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (let v1 (vector 1 2))
             (let v2 (vector 3 4))
             (return (var v2)))))))
  '())

(test-unify 'annotate-16c2
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn foo ()
             (let v1 (vector 1 2))
             (let v2 (vector 3 4))
             (return (var v2)))))))
  (let ((v (vector 'v))
        (w (vector 'w)))
    `(module
       (fn foo () (() -> (vector int 2))
         (let ,v (vector int 2) (vector (int 1) (int 2)))
         (let v1 (vector int 2) (var (vector int 2) ,v))
         (let ,w (vector int 2) (vector (int 3) (int 4)))
         (let v2 (vector int 2) (var (vector int 2) ,w))
         (return (var (vector int 2) v2))))))

(test 'annotate-16d1
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (let v1 (vector 1 2))
             (let v2 (vector 3 4))
             (return (var v1)))))))
  '())

(test-unify 'annotate-16d2
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn foo ()
             (let v1 (vector 1 2))
             (let v2 (vector 3 4))
             (return (var v1)))))))
  (let ((v (vector 'v))
        (w (vector 'w)))
    `(module
       (fn foo () (() -> (vector int 2))
         (let ,v (vector int 2) (vector (int 1) (int 2)))
         (let v1 (vector int 2) (var (vector int 2) ,v))
         (let ,w (vector int 2) (vector (int 3) (int 4)))
         (let v2 (vector int 2) (var (vector int 2) ,w))
         (return (var (vector int 2) v1))))))

(test 'annotate-17a
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (let v1 (vector 1 2))
             (let v2 (vector 3 4))             
             (return (= (var v1) (var v2))))))))
    '())

(test-unify 'annotate-17b
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn foo ()
             (let v1 (vector 1 2))
             (let v2 (vector 3 4))             
             (return (= (var v1) (var v2))))))))
    (let ((v (vector 'v))
          (w (vector 'w)))
    `(module
       (fn foo () (() -> bool)
         (let ,v (vector int 2) (vector (int 1) (int 2)))
         (let v1 (vector int 2) (var (vector int 2) ,v))
         (let ,w (vector int 2) (vector (int 3) (int 4)))
         (let v2 (vector int 2) (var (vector int 2) ,w))
         (return (= (var (vector int 2) v1) (var (vector int 2) v2)))))))

(test 'annotate-18a
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()               
             (return (= (vector 1 2) (vector 3 4))))))))
  '())

(test-unify 'annotate-18b
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn bar ()               
             (return (= (vector 1 2) (vector 3 4))))))))
  (let ((v (vector 'v))
        (w (vector 'w)))
    `(module
       (fn bar () (() -> bool)
         (let ,v (vector int 2) (vector (int 1) (int 2)))
         (let ,w (vector int 2) (vector (int 3) (int 4)))
         (return (= (var (vector int 2) ,v) (var (vector int 2) ,w)))))))

(test 'annotate-23a
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (let X (vector 1 2 3 4))
             (return (= (vector-ref X 0) (vector-ref X 1))))))))
  '())

(test-unify 'annotate-23b
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn foo ()
             (let X (vector 1 2 3 4))
             (return (= (vector-ref X 0) (vector-ref X 1))))))))
  (let ((v (vector 'v)))
    `(module
       (fn foo () (() -> bool)
         (let ,v (vector int 4) (vector (int 1) (int 2) (int 3) (int 4)))
         (let X (vector int 4) (var (vector int 4) ,v))
         (return (= (vector-ref int (var (vector int 4) X) (int 0)) (vector-ref int (var (vector int 4) X) (int 1))))))))

(test-unify 'annotate-reduce0
  (typecheck
    (lift-vectors
      (returnify
       '(module
          (fn main ()
            (let v (vector 1 1 1 1))
            (let w (reduce + (var v)))
            (assert (= 4 (var w)))
            (return 0))))))
  (let ((g1 (vector 'v))
        (g2 (vector 'w)))
    `(module
       (fn main () (() -> int)
         (let ,g1 (vector int 4) (vector (int 1) (int 1) (int 1) (int 1)))
         (let v (vector int 4) (var (vector int 4) ,g1))         
         (let ,g2 int (reduce int + (var (vector int 4) v)))
         (let w int (var int ,g2))
         (assert (= (int 4) (var int w)))
         (return (int 0))))))

(test-unify 'annotate-reduce1
  (typecheck
    (lift-vectors
      (returnify
       '(module
          (fn main ()
            (assert (= 4 (reduce + (vector 1 1 1 1))))
            (return 0))))))
  (let ((v (vector 'v))
        (w (vector 'w)))
    `(module
       (fn main () (() -> int)
         (let ,v (vector int 4) (vector (int 1) (int 1) (int 1) (int 1)))
         (let ,w int (reduce int + (var (vector int 4) ,v)))
         (assert (= (int 4) (var int ,w)))
         (return (int 0))))))

(test-unify 'annotate-add-kernel-apl-syntax
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (let X (vector 1 2 3 4))
             (let Y (vector 5 1 1 7))
             (let Z (kernel ((x (var X)) (y (var Y)))
                      (+ (var x) (var y))))
             (assert (= (vector-ref Z 0) 6))
             (assert (= (vector-ref Z 1) 3))
             (assert (= (vector-ref Z 2) 4))
             (assert (= (vector-ref Z 3) 11))
             (return 0))))))
  (let ((g1 (vector 'g1))
        (g2 (vector 'g2))
        (g3 (vector 'g3)))
    `(module
       (fn main () (() -> int)
         (let ,g1 (vector int 4)
           (vector (int 1) (int 2) (int 3) (int 4)))
         (let X (vector int 4) (var (vector int 4) ,g1))
         (let ,g2 (vector int 4)
           (vector (int 5) (int 1) (int 1) (int 7)))
         (let Y (vector int 4) (var (vector int 4) ,g2))
         (let ,g3 (vector int 4)
           (kernel (vector int 4)
             (((x int) ((var (vector int 4) X) (vector int 4)))
              ((y int) ((var (vector int 4) Y) (vector int 4))))
             (+ (var int x) (var int y))))
         (let Z (vector int 4) (var (vector int 4) ,g3))
         (assert (= (vector-ref int (var (vector int 4) Z) (int 0)) (int 6)))
         (assert (= (vector-ref int (var (vector int 4) Z) (int 1)) (int 3)))
         (assert (= (vector-ref int (var (vector int 4) Z) (int 2)) (int 4)))
         (assert (= (vector-ref int (var (vector int 4) Z) (int 3)) (int 11)))
         (return (int 0))))))

(test-unify 'length
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (let A (vector 0 1 2))
             (let n (length (var A)))
             (assert (= (var n) 3))
             (return (length (var A))))))))
  (let ((g1 (vector 'g1)))
    `(module
       (fn main () (() -> int)
         (let ,g1 (vector int 3) (vector (int 0) (int 1) (int 2)))
         (let A (vector int 3) (var (vector int 3) ,g1))
         (let n int (length (var (vector int 3) A)))
         (assert (= (var int n) (int 3)))
         (return (length (var (vector int 3) A)))))))

(test-unify 'iota
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (let I (iota 5))
             (assert (= (var I) (vector 0 1 2 3 4)))
             (return (vector-ref I 4)))))))
  (let ((g1 (vector 'g1)))
    `(module
       (fn main () (() -> int)
         (let I (vector int 5) (iota (int 5)))
         (let ,g1 (vector int 5) (vector (int 0) (int 1) (int 2) (int 3) (int 4)))
         (assert (= (var (vector int 5) I) (var (vector int 5) ,g1)))
         (return (vector-ref int (var (vector int 5) I) (int 4)))))))

(test-unify 'let-vec-vec
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (let A (vector
                     (vector 1 0 0 0)
                     (vector 0 1 0 0)
                     (vector 0 0 1 0)
                     (vector 0 0 0 1)))
             (return (vector-ref (vector-ref A 1) 2)))))))
  (let ((g1 (vector 'g1))
        (g2 (vector 'g2))
        (g3 (vector 'g3))
        (g4 (vector 'g4))
        (g5 (vector 'g5)))
    `(module
       (fn main () (() -> int)
         (let ,g1 (vector int) (vector (int 1) (int 0) (int 0) (int 0)))
         (let ,g2 (vector int) (vector (int 0) (int 1) (int 0) (int 0)))
         (let ,g3 (vector int) (vector (int 0) (int 0) (int 1) (int 0)))
         (let ,g4 (vector int) (vector (int 0) (int 0) (int 0) (int 1)))
         (let ,g5 (vector (vector int))
            (vector (var (vector int) ,g1)
                    (var (vector int) ,g2)
                    (var (vector int) ,g3)
                    (var (vector int) ,g4)))
         (let A (vector (vector int)) (var (vector (vector int)) ,g5))
         (return (vector-ref int (vector-ref (vector int) (var (vector (vector int)) A) (int 1)) (int 2)))))))

(test-unify 'annotate-print-vec-vec
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (print (vector (vector 1)
                            (vector 2 3)
                            (vector 4 5 6)))
             (return 0))))))
  (let ((g1 (vector 'g1))
        (g2 (vector 'g2))
        (g3 (vector 'g3))
        (g4 (vector 'g4)))
  `(module
     (fn main () (() -> int)
       (let ,g1 (vector int) (vector (int 1)))
       (let ,g2 (vector int) (vector (int 2) (int 3)))
       (let ,g3 (vector int) (vector (int 4) (int 5) (int 6)))
       (let ,g4 (vector (vector int)) (vector (var (vector int) ,g1) (var (vector int) ,g2) (var (vector int) ,g3)))
       (print (var (vector (vector int)) ,g4))
       (return (int 0))))))

(test-unify 'nested-kernels
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (let Rows (vector (vector 1 2 3)
                               (vector 4 5 6)
                               (vector 7 8 9)
                               (vector 10 11 12)))
             (let total (reduce + (kernel ((row (var Rows)))
                                    (reduce + (kernel ((x (var row)))
                                                (var x))))))
             (assert (= (var total) 78))
             (return 0))))))
  (let ((g1 (vector 'g1))
        (g2 (vector 'g2))
        (g3 (vector 'g3))
        (g4 (vector 'g4))
        (g5 (vector 'g5))
        (g6 (vector 'g6))
        (g7 (vector 'g7))
        (g8 (vector 'g8))
        (g9 (vector 'g9)))
    `(module
       (fn main () (() -> int)
         (let ,g1 (vector int) (vector (int 1) (int 2) (int 3)))
         (let ,g2 (vector int) (vector (int 4) (int 5) (int 6)))
         (let ,g3 (vector int) (vector (int 7) (int 8) (int 9)))
         (let ,g4 (vector int) (vector (int 10) (int 11) (int 12)))
         (let ,g5 (vector (vector int))
           (vector (var (vector int) ,g1)
                   (var (vector int) ,g2)
                   (var (vector int) ,g3)
                   (var (vector int) ,g4)))
         (let Rows (vector (vector int)) (var (vector (vector int)) ,g5))
         (let ,g6 (vector int)
           (kernel (vector int)
             (((row (vector int)) ((var (vector (vector int)) Rows) (vector (vector int)))))
             (let ,g7 (vector int)
               (kernel (vector int)
                 (((x int) ((var (vector int) row) (vector int))))
                 (var int x)))
             (let ,g8 int (reduce int + (var (vector int) ,g7)))
             (var int ,g8)))
         (let ,g9 int (reduce int + (var (vector int) ,g6)))
         (let total int (var int ,g9))
         (assert (= (var int total) (int 78)))
         (return (int 0))))))

(test-unify 'annotate-simplified-problem-kernel
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (let A (kernel ((j (vector 1 2 3)))
                      (var j)))             
             (return 0))))))
  (let ((g1 (vector 'g1))
        (g2 (vector 'g2)))
    `(module
       (fn main () (() -> int)
         (let ,g1 (vector int) (vector (int 1) (int 2) (int 3)))
         (let ,g2 (vector int)
           (kernel (vector int)
             (((j int) ((var (vector int) ,g1) (vector int))))
             (var int j)))
         (let A (vector int) (var (vector int) ,g2))
         (return (int 0))))))

(test-unify 'annotate-dmm
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (let A (vector
                     (vector 1 0 0 0)
                     (vector 0 1 0 0)
                     (vector 0 0 1 0)
                     (vector 0 0 0 1)))
             (let B (vector
                     (vector 1 0 0 0)
                     (vector 0 1 0 0)
                     (vector 0 0 1 0)
                     (vector 0 0 0 1)))
             (let Bt (kernel ((j (iota (length (vector-ref B 0)))))
                       (kernel ((i (iota (length (var B)))))
                         (vector-ref (vector-ref B (var j)) (var i)))))
             (let C (kernel ((row (var A)))
                      (kernel ((col (var Bt)))
                        (reduce + (kernel ((x (var row)) (y (var col)))
                                    (* (var x) (var y)))))))
             (assert (= (var C) (var A)))
             (return 0))))))
  (let ((g1 (vector 'g1))
        (g2 (vector 'g2))
        (g3 (vector 'g3))
        (g4 (vector 'g4))
        (g5 (vector 'g5))
        (g6 (vector 'g6))
        (g7 (vector 'g7))
        (g8 (vector 'g8))
        (g9 (vector 'g9))
        (g10 (vector 'g10))
        (g11 (vector 'g11))
        (g12 (vector 'g12))
        (g13 (vector 'g13))
        (g14 (vector 'g14))
        (g15 (vector 'g15))
        (g16 (vector 'g16)))
    `(module
       (fn main () (() -> int)
         (let ,g1 (vector int) (vector (int 1) (int 0) (int 0) (int 0)))
         (let ,g2 (vector int) (vector (int 0) (int 1) (int 0) (int 0)))
         (let ,g3 (vector int) (vector (int 0) (int 0) (int 1) (int 0)))
         (let ,g4 (vector int) (vector (int 0) (int 0) (int 0) (int 1)))
         (let ,g5 (vector (vector int)) (vector (var (vector int) ,g1)
                                                (var (vector int) ,g2)
                                                (var (vector int) ,g3)
                                                (var (vector int) ,g4)))
         (let A (vector (vector int)) (var (vector (vector int)) ,g5))
         (let ,g6 (vector int) (vector (int 1) (int 0) (int 0) (int 0)))
         (let ,g7 (vector int) (vector (int 0) (int 1) (int 0) (int 0)))
         (let ,g8 (vector int) (vector (int 0) (int 0) (int 1) (int 0)))
         (let ,g9 (vector int) (vector (int 0) (int 0) (int 0) (int 1)))
         (let ,g10 (vector (vector int)) (vector (var (vector int) ,g6)
                                                 (var (vector int) ,g7)
                                                 (var (vector int) ,g8)
                                                 (var (vector int) ,g9)))
         (let B (vector (vector int)) (var (vector (vector int)) ,g10))
         (let ,g11 (vector (vector int))
           (kernel (vector (vector int))
             (((j int) ((iota (length (vector-ref (vector int) (var (vector (vector int)) B) (int 0)))) (vector int))))
             (let ,g12 (vector int)
               (kernel (vector int)
                 (((i int) ((iota (length (var (vector (vector int)) B))) (vector int))))
                 (vector-ref int
                   (vector-ref (vector int)
                     (var (vector (vector int)) B) (var int j)) (var int i))))
             (var (vector int) ,g12)))
         (let Bt (vector (vector int)) (var (vector (vector int)) ,g11))
         (let ,g13 (vector (vector int))
           (kernel (vector (vector int))
             (((row (vector int)) ((var (vector (vector int)) A) (vector (vector int)))))
             (let ,g14 (vector int)
               (kernel (vector int)
                 (((col (vector int)) ((var (vector (vector int)) Bt) (vector (vector int)))))
                 (let ,g15 (vector int)
                   (kernel (vector int)
                     (((x int) ((var (vector int) row) (vector int))) ((y int) ((var (vector int) col) (vector int))))
                     (* (var int x) (var int y))))
                 (let ,g16 int (reduce int + (var (vector int) ,g15)))
                 (var int ,g16)))
             (var (vector int) ,g14)))
         (let C (vector (vector int)) (var (vector (vector int)) ,g13))
         (assert (= (var (vector (vector int)) C) (var (vector (vector int)) A)))
         (return (int 0))))))

(test-unify 'annotate-smv
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (let Ai (vector
                      (vector 0)
                      (vector 1)
                      (vector 2)
                      (vector 3)))
             (let Av (vector
                      (vector 1)
                      (vector 1)
                      (vector 1)
                      (vector 1)))
             (let X (vector 1 2 3 4))
             (let Y (kernel ((is (var Ai)) (vs (var Av)))
                      (reduce + (kernel ((i (var is)) (v (var vs)))
                                  (* (var v) (vector-ref X (var i)))))))
             (assert (= (var X) (var Y)))
             (return 0))))))
  (let ((g1 (vector 'g1))
        (g2 (vector 'g2))
        (g3 (vector 'g3))
        (g4 (vector 'g4))
        (g5 (vector 'g5))
        (g6 (vector 'g6))
        (g7 (vector 'g7))
        (g8 (vector 'g8))
        (g9 (vector 'g9))
        (g10 (vector 'g10))
        (g11 (vector 'g11))
        (g12 (vector 'g12))
        (g13 (vector 'g13))
        (g14 (vector 'g14)))
    `(module
       (fn main () (() -> int)
         (let ,g1 (vector int) (vector (int 0)))
         (let ,g2 (vector int) (vector (int 1)))
         (let ,g3 (vector int) (vector (int 2)))
         (let ,g4 (vector int) (vector (int 3)))
         (let ,g5 (vector (vector int)) (vector (var (vector int) ,g1)
                                                (var (vector int) ,g2)
                                                (var (vector int) ,g3)
                                                (var (vector int) ,g4)))
         (let Ai (vector (vector int)) (var (vector (vector int)) ,g5))
         (let ,g6 (vector int) (vector (int 1)))
         (let ,g7 (vector int) (vector (int 1)))
         (let ,g8 (vector int) (vector (int 1)))
         (let ,g9 (vector int) (vector (int 1)))
         (let ,g10 (vector (vector int)) (vector (var (vector int) ,g6)
                                                 (var (vector int) ,g7)
                                                 (var (vector int) ,g8)
                                                 (var (vector int) ,g9)))
         (let Av (vector (vector int)) (var (vector (vector int)) ,g10))
         (let ,g11 (vector int) (vector (int 1) (int 2) (int 3) (int 4)))
         (let X (vector int) (var (vector int) ,g11))
         (let ,g12 (vector int)
           (kernel (vector int)
             (((is (vector int)) ((var (vector (vector int)) Ai) (vector (vector int))))
              ((vs (vector int)) ((var (vector (vector int)) Av) (vector (vector int)))))
             (let ,g13 (vector int)
               (kernel (vector int)
                 (((i int) ((var (vector int) is) (vector int)))
                  ((v int) ((var (vector int) vs) (vector int))))
                 (* (var int v) (vector-ref int (var (vector int) X) (var int i)))))
             (let ,g14 int (reduce int + (var (vector int) ,g13)))
             (var int ,g14)))
         (let Y (vector int) (var (vector int) ,g12))
         (assert (= (var (vector int) X) (var (vector int) Y)))
         (return (int 0))))))

(test-unify 'foo
  (typecheck
    (lift-vectors
      (returnify
       '(module (fn main ()
                  (let y 0)
                  (for (x 0 10)
                    (print (var x))
                    (set! (var y) (+ (var x) (var y))))
                  (assert (= (var y) 45))
                  (return 0))))))
  '(module
     (fn main () (() -> int)
       (let y int (int 0))
       (for ((x int) (int 0) (int 10))
         (print (var int x))
         (set! (var int y) (+ (var int x) (var int y))))
       (assert (= (var int y) (int 45)))
       (return (int 0)))))

(test-unify 'bar
  (typecheck
   (lift-vectors
    (returnify
     '(module
        (fn main ()
            (let X (vector 1 2 3))
            (for (i 0 (length (var X)))
              (let x (+ (vector-ref X (var i)) 1))
              (vector-set! (var X) (var i) (var x)))
            (assert (= (vector-ref X 0) 2))
            (return 0))))))
  (let ((g1 (vector 'g1)))
    `(module
       (fn main () (() -> int)
         (let ,g1 (vector int) (vector (int 1) (int 2) (int 3)))
         (let X (vector int) (var (vector int) ,g1))
         (for ((i int) (int 0) (length (var (vector int) X)))
           (let x int (+ (vector-ref int (var (vector int) X) (var int i)) (int 1)))
           (vector-set! int (var (vector int) X) (var int i) (var int x)))
         (assert (= (vector-ref int (var (vector int) X) (int 0)) (int 2)))
         (return (int 0))))))

(test-unify 'baz
  (typecheck
   (lift-vectors
    (returnify
     '(module
        (fn main ()
            (let X (vector 1 2 3))
            (for (i 0 (length (var X)))
              (vector-set! (var X) (var i) (+ (vector-ref X (var i)) 1)))
            (assert (= (vector-ref X 0) 2))
            (return 0))))))
  (let ((g1 (vector 'g1)))
    `(module
       (fn main () (() -> int)
         (let ,g1 (vector int) (vector (int 1) (int 2) (int 3)))
         (let X (vector int) (var (vector int) ,g1))
         (for ((i int) (int 0) (length (var (vector int) X)))
           (vector-set!
             int
             (var (vector int) X)
             (var int i)
             (+ (vector-ref int (var (vector int) X) (var int i)) (int 1))))
         (assert (= (vector-ref int (var (vector int) X) (int 0)) (int 2)))
         (return (int 0))))))

(test-unify 'dmm-1
  (typecheck
   (lift-vectors
    (returnify
     '(module
        (fn main ()
            (let A (vector
                    (vector 1 0 0 0)
                    (vector 0 1 0 0)
                    (vector 0 0 1 0)
                    (vector 0 0 0 1)))
            (let B (vector
                    (vector 1 0 0 0)
                    (vector 0 1 0 0)
                    (vector 0 0 1 0)
                    (vector 0 0 0 1)))
            (let Bt (make-vector (vector int) (length (var B))))
            (return 0))))))
  (let ((g1 (vector 'g1))
        (g2 (vector 'g2))
        (g3 (vector 'g3))
        (g4 (vector 'g4))
        (g5 (vector 'g5))
        (g6 (vector 'g6))
        (g7 (vector 'g7))
        (g8 (vector 'g8))
        (g9 (vector 'g9))
        (g10 (vector 'g10))
        (g11 (vector 'g11)))
    `(module
       (fn main () (() -> int)
         (let ,g1 (vector int) (vector (int 1) (int 0) (int 0) (int 0)))
         (let ,g2 (vector int) (vector (int 0) (int 1) (int 0) (int 0)))
         (let ,g3 (vector int) (vector (int 0) (int 0) (int 1) (int 0)))
         (let ,g4 (vector int) (vector (int 0) (int 0) (int 0) (int 1)))
         (let ,g5 (vector (vector int))
           (vector (var (vector int) ,g1)
                   (var (vector int) ,g2)
                   (var (vector int) ,g3)
                   (var (vector int) ,g4)))
         (let A (vector (vector int)) (var (vector (vector int)) ,g5))
         (let ,g6 (vector int) (vector (int 1) (int 0) (int 0) (int 0)))
         (let ,g7 (vector int) (vector (int 0) (int 1) (int 0) (int 0)))
         (let ,g8 (vector int) (vector (int 0) (int 0) (int 1) (int 0)))
         (let ,g9 (vector int) (vector (int 0) (int 0) (int 0) (int 1)))
         (let ,g10 (vector (vector int))
           (vector (var (vector int) ,g6)
                   (var (vector int) ,g7)
                   (var (vector int) ,g8)
                   (var (vector int) ,g9)))
         (let B (vector (vector int)) (var (vector (vector int)) ,g10))
         (let ,g11 int (length (var (vector (vector int)) B)))
         (let Bt (vector (vector int)) (make-vector (vector int) (var int ,g11)))
         (return (int 0))))))

(test-unify 'dmm-2
  (typecheck
   (lift-vectors
    (returnify
     '(module
  (fn main ()
      (let A (vector
              (vector 1 0 0 0)
              (vector 0 1 0 0)
              (vector 0 0 1 0)
              (vector 0 0 0 1)))
      (let B (vector
              (vector 1 0 0 0)
              (vector 0 1 0 0)
              (vector 0 0 1 0)
              (vector 0 0 0 1)))      
      (let Bt (make-vector (vector int) (length (var B))))
      (for (j 0 (length (vector-ref B 0)))
        (let row (kernel ((i (iota (length (var B)))))
                         (vector-ref (vector-ref B (var j)) (var i))))
        (vector-set! (var Bt) (var j) (var row)))

      (let C (make-vector (vector int) (length (var A))))
      (for (i 0 (length (var A)))
        (let C-row (make-vector int (length (var Bt))))
        (vector-set! (var C) (var i) (var C-row)))      

      (for (row-index 0 (length (var A)))
        (let row (vector-ref A (var row-index)))
        (for (col-index 0 (length (var Bt)))
          (let col (vector-ref Bt (var col-index)))
           (let c (reduce +
                    (kernel ((x (var row)) (y (var col)))
                            (* (var x) (var y)))))
           (vector-set! (vector-ref C (var row-index)) (var col-index) (var c))))
      
      (assert (= (var C) (var A)))
      (return 0))))))
  (let ((g1 (vector 'g1))
        (g2 (vector 'g2))
        (g3 (vector 'g3))
        (g4 (vector 'g4))
        (g5 (vector 'g5))
        (g6 (vector 'g6))
        (g7 (vector 'g7))
        (g8 (vector 'g8))
        (g9 (vector 'g9))
        (g10 (vector 'g10))
        (g11 (vector 'g11))
        (g12 (vector 'g12))
        (g13 (vector 'g13))
        (g14 (vector 'g14))
        (g15 (vector 'g15))
        (g16 (vector 'g16)))
    `(module
       (fn main () (() -> int)
         (let ,g1 (vector int) (vector (int 1) (int 0) (int 0) (int 0)))
         (let ,g2 (vector int) (vector (int 0) (int 1) (int 0) (int 0)))
         (let ,g3 (vector int) (vector (int 0) (int 0) (int 1) (int 0)))
         (let ,g4 (vector int) (vector (int 0) (int 0) (int 0) (int 1)))
         (let ,g5 (vector (vector int))
           (vector
            (var (vector int) ,g1)
            (var (vector int) ,g2)
            (var (vector int) ,g3)
            (var (vector int) ,g4)))
         (let A (vector (vector int)) (var (vector (vector int)) ,g5))
         (let ,g6 (vector int) (vector (int 1) (int 0) (int 0) (int 0)))
         (let ,g7 (vector int) (vector (int 0) (int 1) (int 0) (int 0)))
         (let ,g8 (vector int) (vector (int 0) (int 0) (int 1) (int 0)))
         (let ,g9 (vector int) (vector (int 0) (int 0) (int 0) (int 1)))
         (let ,g10 (vector (vector int))
           (vector (var (vector int) ,g6)
                   (var (vector int) ,g7)
                   (var (vector int) ,g8)
                   (var (vector int) ,g9)))
         (let B (vector (vector int)) (var (vector (vector int)) ,g10))

         (let ,g14 int (length (var (vector (vector int)) B)))
         (let Bt (vector (vector int)) (make-vector (vector int) (var int ,g14)))
         
         (for ((j int) (int 0) (length (vector-ref (vector int) (var (vector (vector int)) B) (int 0))))
           (let ,g11 (vector int)
             (kernel (vector int)
               (((i int) ((iota (length (var (vector (vector int)) B))) (vector int))))
               (vector-ref int
                 (vector-ref (vector int) (var (vector (vector int)) B) (var int j))
                 (var int i))))
           (let row (vector int) (var (vector int) ,g11))
           (vector-set! (vector int) (var (vector (vector int)) Bt) (var int j) (var (vector int) row)))

         (let ,g15 int (length (var (vector (vector int)) A)))
         (let C (vector (vector int)) (make-vector (vector int) (var int ,g15)))        
         (for ((i int) (int 0) (length (var (vector (vector int)) A)))
           (let ,g16 int (length (var (vector (vector int)) Bt)))
           (let C-row (vector int) (make-vector int (var int ,g16)))
           (vector-set! (vector int) (var (vector (vector int)) C) (var int i) (var (vector int) C-row)))
         (for ((row-index int) (int 0) (length (var (vector (vector int)) A)))
           (let row (vector int) (vector-ref (vector int) (var (vector (vector int)) A) (var int row-index)))
           (for ((col-index int) (int 0) (length (var (vector (vector int)) Bt)))
             (let col (vector int)
               (vector-ref (vector int) (var (vector (vector int)) Bt) (var int col-index)))
             (let ,g12 (vector int)
               (kernel (vector int)
                 (((x int) ((var (vector int) row) (vector int)))
                  ((y int) ((var (vector int) col) (vector int))))
                 (* (var int x) (var int y))))
             (let ,g13 int (reduce int + (var (vector int) ,g12)))
             (let c int (var int ,g13))
             (vector-set! int (vector-ref (vector int) (var (vector (vector int)) C) (var int row-index)) (var int col-index) (var int c))))
         (assert (= (var (vector (vector int)) C) (var (vector (vector int)) A)))
         (return (int 0))))))

#!eof

;; add-kernel
(module
  (fn main ()
      (let X (vector 1 2 3 4))
      (let Y (vector 5 1 1 7))
      (let Z (kernel ((x X) (y Y))
               (+ (var x) (var y))))
      (assert (= (vector-ref Z 0) 6))
      (assert (= (vector-ref Z 1) 3))
      (assert (= (vector-ref Z 2) 4))
      (assert (= (vector-ref Z 3) 11))
      (return 0)))

;; add-reduce-kernel-apl-syntax
(module
  (fn main ()
      (let X (vector 2 2 3 4))
      (let Y (vector 5 3 4 7))
      (let z (reduce + (kernel ((x X) (y Y))
               (* (var x) (var y))))
      (assert (= z 56))
      (return 0)))

;; async-kernel
(module
  (fn main ()
      (let X (vector 1 2 3 4))
      (let X (vector 2 3 4 5))
      (let handle (async/kernel ((x X) (y Y))
      	            (* (var x) (var y))))
      (let z (reduce + (wait handle)))
      (assert (= z 40))
      (return 0)))

;; kanor-kernel
(module
  (fn main ()
    (let X (vector 1 2 3 4))
    (let X (vector 2 3 4 5))
    (let Z (kernel ((x X) (y Y))
             (* (var x) (var y))))
    (assert (= (vector-ref Z 0) 2))
    (assert (= (vector-ref Z 1) 6))
    (assert (= (vector-ref Z 2) 12))
    (assert (= (vector-ref Z 3) 20))
    (let NUM_NODES 5)
    (where* ((r world)
             (i 0 (length Y)))
      (transfer
        (at (vector-ref Z i) (modulo (+ r 1) NUM_NODES))
        (at (vector-ref Y i) r)))
    (let Z (kernel ((x X) (y Y))
             (* (var x) (var y))))
    (assert (= (vector-ref Z 0) (* (vector-ref X 0) (vector-ref Y 0))))
    (assert (= (vector-ref Z 1) (* (vector-ref X 1) (vector-ref Y 1))))
    (assert (= (vector-ref Z 2) (* (vector-ref X 2) (vector-ref Y 2))))
    (assert (= (vector-ref Z 3) (* (vector-ref X 3) (vector-ref Y 3))))      
    (return 0)))

;; simple-stencil
(module
  (fn main ()
    (let X (vector 1 2 3 4))
    (let X (kernel ((x X) (y (rotate X 1))) (+ (var x) (var y))))
    (assert (= (vector-ref X 0) 5))
    (assert (= (vector-ref X 1) 3))
    (assert (= (vector-ref X 2) 5))
    (assert (= (vector-ref X 3) 7))
    (return 0))

(test-unify 'annotate-5d
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (return (call foo)))
           (fn foo ()
             (return 5))))))
  (let ((v (vector 'v)))
    `(module
       (fn main () (() -> int)
         (return (call int foo)))
       (fn foo () (() -> int)
         (return (int 5))))))

(test-unify 'annotate-5e
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn foo ()
             (return 5))
           (fn main ()
             (return (call foo)))))))
  (let ((v (vector 'v)))
    `(module
       (fn foo () (() -> int)
         (return 5))
       (fn main () (() -> int)
         (return (call int foo))))))

(test-unify 'annotate-5f
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (return (call foo 3)))
           (fn foo (x)
             (return (+ (var x) 1)))))))
  (let ((v (vector 'v)))
    `(module
       (fn main () (() -> int)
         (return (call int foo (int 3))))
       (fn foo (x) (int -> int)
         (return (+ (var x) (int 1)))))))

(test-unify 'annotate-5g
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (return (vector-ref (call foo 3) 0)))
           (fn foo (x)
             (return (vector (+ (var x) 1))))))))
  (let ((v (vector 'v)))
    `(module
       (fn main () (() -> int)
         (let ,v (vector int)
           (call (vector int) foo (int 3)))
         (return (vector-ref (var ,v) (int 0))))
       (fn foo (x) (int -> (vector int))
         (return (vector (+ (var x) (int 1))))))))

(test-unify 'annotate-5c
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn foo ()
             (return (vector 1 2 3 4)))
           (fn main ()
             (return 0))))))
  (let ((v (vector 'v)))
    `(module
       (fn foo () (() -> (vector int))
         (let ,v (vector int)
           (vector (int 1) (int 2) (int 3) (int 4)))
         (return (var ,v)))
       (fn main () (() -> int)
         (return (int 0))))))

(test-unify 'annotate-5b
  (typecheck
    (lift-vectors
      (returnify
        '(module
           (fn main ()
             (return 0))
           (fn foo ()
             (return (vector 1 2 3 4)))))))
  (let ((v (vector 'v)))
    `(module
       (fn main () (() -> int)
         (return (int 0)))
       (fn foo () (() -> (vector int))
         (let ,v (vector int)
           (vector (int 1) (int 2) (int 3) (int 4)))
         (return (var ,v))))))

;; careful!  don't lift a vector outside of the scope of it's free variables
(kernel ((i (iota 5))) (vector i i i i))
