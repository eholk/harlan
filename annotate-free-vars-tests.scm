(import
  (annotate-free-vars)
  (verify-compile-module)
  (returnify)
  (uglify-vectors)
  (lower-vectors)
  (typecheck)
  (returnify-kernels)
  (harlancompiler)
  (mk))

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

(test-unify 'annotate-0c
  (verify-compile-module
   (compile-module
    (uglify-vectors
     (returnify-kernels
      (lower-vectors
       (compile-harlan-frontend '(module
                                  (fn main ()
                                      (let X (vector 1 2 3 4))
                                      (let Y (vector 5 1 1 7))
                                      (let Z (kernel ((x (var X)) (y (var Y)))
                                                     (+ (var x) (var y))))
                                      (assert (= (vector-ref (var Z) 0) 6))
                                      (assert (= (vector-ref (var Z) 1) 3))
                                      (assert (= (vector-ref (var Z) 2) 4))
                                      (assert (= (vector-ref (var Z) 3) 11))
                                      (return 0)))))))))
  (let ((g1 (vector 'g1))
        (g2 (vector 'g2))
        (g3 (vector 'g3)))
    `((func int main ()
        (let ,g1 vec (mk_vec 1 4 (sizeof int)))
        (set! (deref (cast (ptr int) (vec_ref_1d ,g1 0))) 1)
        (set! (deref (cast (ptr int) (vec_ref_1d ,g1 1))) 2)
        (set! (deref (cast (ptr int) (vec_ref_1d ,g1 2))) 3)
        (set! (deref (cast (ptr int) (vec_ref_1d ,g1 3))) 4)
        (let X vec ,g1)
        (let ,g2 vec (mk_vec 1 4 (sizeof int)))
        (set! (deref (cast (ptr int) (vec_ref_1d ,g2 0))) 5)
        (set! (deref (cast (ptr int) (vec_ref_1d ,g2 1))) 1)
        (set! (deref (cast (ptr int) (vec_ref_1d ,g2 2))) 1)
        (set! (deref (cast (ptr int) (vec_ref_1d ,g2 3))) 7)
        (let Y vec ,g2)
        (let ,g3 vec (mk_vec 1 (vec_length X) (sizeof int)))
        (kernel (((retval_1 int) ((var (vector int) ,g3) (vector int)))
                 ((x int) ((var (vector int) X) (vector int)))
                 ((y int) ((var (vector int) Y) (vector int))))
           (set! retval_1 (+ x y)))
        (let Z vec ,g3)
        (do (assert (= (deref (cast (ptr int) (vec_ref_1d Z 0))) 6)))
        (do (assert (= (deref (cast (ptr int) (vec_ref_1d Z 1))) 3)))
        (do (assert (= (deref (cast (ptr int) (vec_ref_1d Z 2))) 4)))
        (do (assert (= (deref (cast (ptr int) (vec_ref_1d Z 3))) 11)))
        (return 0)))))

(let ((prog (verify-compile-module
             (compile-module
              (uglify-vectors
               (returnify-kernels
                (lower-vectors
                 (compile-harlan-frontend '(module
                                             (fn main ()
                                                 (let X (vector 1 2 3 4))
                                                 (let Y (vector 5 1 1 7))
                                                 (let Z (kernel ((x (var X)) (y (var Y)))
                                                                (+ (var x) (var y))))
                                                 (assert (= (vector-ref (var Z) 0) 6))
                                                 (assert (= (vector-ref (var Z) 1) 3))
                                                 (assert (= (vector-ref (var Z) 2) 4))
                                                 (assert (= (vector-ref (var Z) 3) 11))
                                                 (return 0)))))))))))
  (test-unify 'annotate-0d
    (annotate-free-vars prog)
    (let ((g1 (vector 'g1))
          (g2 (vector 'g2))
          (g3 (vector 'g3))
          (g4 (vector 'g4)))
      `((func int main ()
          (let ,g1 vec (mk_vec 1 4 (sizeof int)))
          (set! (deref (cast (ptr int) (vec_ref_1d ,g1 0))) 1)
          (set! (deref (cast (ptr int) (vec_ref_1d ,g1 1))) 2)
          (set! (deref (cast (ptr int) (vec_ref_1d ,g1 2))) 3)
          (set! (deref (cast (ptr int) (vec_ref_1d ,g1 3))) 4)
          (let X vec ,g1)
          (let ,g2 vec (mk_vec 1 4 (sizeof int)))
          (set! (deref (cast (ptr int) (vec_ref_1d ,g2 0))) 5)
          (set! (deref (cast (ptr int) (vec_ref_1d ,g2 1))) 1)
          (set! (deref (cast (ptr int) (vec_ref_1d ,g2 2))) 1)
          (set! (deref (cast (ptr int) (vec_ref_1d ,g2 3))) 7)
          (let Y vec ,g2)
          (let ,g3 vec (mk_vec 1 (vec_length X) (sizeof int)))
          (kernel (((,g4 int) ((var (vector int) ,g3) (vector int)))
                   ((x int) ((var (vector int) X) (vector int)))
                   ((y int) ((var (vector int) Y) (vector int))))
                  (free-vars)
                  (set! ,g4 (+ x y)))
          (let Z vec ,g3)
          (do (assert (= (deref (cast (ptr int) (vec_ref_1d Z 0))) 6)))
          (do (assert (= (deref (cast (ptr int) (vec_ref_1d Z 1))) 3)))
          (do (assert (= (deref (cast (ptr int) (vec_ref_1d Z 2))) 4)))
          (do (assert (= (deref (cast (ptr int) (vec_ref_1d Z 3))) 11)))
          (return 0))))))

(let ((prog (verify-compile-module
             (compile-module
              (uglify-vectors
               (returnify-kernels
                (lower-vectors
                 (compile-harlan-frontend '(module
                                             (fn main ()
                                                 (let X (vector 1 2 3))
                                                 (let y 4)
                                                 (let Z (kernel ((x (var X)))
                                                                (+ (var y) (var x))))
                                                 (assert (= (vector-ref (var Z) 1) 6))
                                                 (return 0)))))))))))
  (test-unify 'annotate-free-y
              (annotate-free-vars prog)
              (let ((g1 (vector 'g1))
                    (g2 (vector 'g2))
                    (g3 (vector 'g3))
                    (g4 (vector 'g4)))
                `((func int main ()
                    (let ,g1 vec (mk_vec 1 3 (sizeof int)))
                    (set! (deref (cast (ptr int) (vec_ref_1d ,g1 0))) 1)
                    (set! (deref (cast (ptr int) (vec_ref_1d ,g1 1))) 2)
                    (set! (deref (cast (ptr int) (vec_ref_1d ,g1 2))) 3)
                    (let X vec ,g1)
                    (let y int 4)
                    (let ,g2 vec (mk_vec 1 (vec_length X) (sizeof int)))
                    (kernel (((,g3 int) ((var (vector int) ,g2) (vector int))) ((x int) ((var (vector int) X) (vector int))))
                            (free-vars (y int))
                            (set! ,g3 (+ y x)))
                    (let Z vec ,g2)
                    (do (assert (= (deref (cast (ptr int) (vec_ref_1d Z 1))) 6)))
                    (return 0))))))

(test-unify 'annotate-0a
  (verify-compile-module
   (compile-module
    (uglify-vectors
     (returnify-kernels
      (lower-vectors
       (compile-harlan-frontend '(module
                                   (fn main ()
                                     (let a (reduce + (vector 1 1 1 1)))
                                     (assert (= 4 (var a)))
                                     (return 0)))))))))
  (let ((g1 (vector 'g1))
        (g2 (vector 'g2))
        (g3 (vector 'g3)))
    `((func int main ()
        (let ,g1 vec (mk_vec 1 4 (sizeof int)))
        (set! (deref (cast (ptr int) (vec_ref_1d ,g1 0))) 1)
        (set! (deref (cast (ptr int) (vec_ref_1d ,g1 1))) 1)
        (set! (deref (cast (ptr int) (vec_ref_1d ,g1 2))) 1)
        (set! (deref (cast (ptr int) (vec_ref_1d ,g1 3))) 1)
        (let ,g2 int (deref (cast (ptr int) (vec_ref_1d ,g1 0))))
        (for (,g3 1 (vec_length ,g1)) (set! ,g2 (+ ,g2 (deref (cast (ptr int) (vec_ref_1d ,g1 ,g3))))))
        (let a int ,g2)
        (do (assert (= 4 a)))
        (return 0)))))

(test-unify 'annotate-0b
  (verify-compile-module
   (compile-module
    (uglify-vectors
     (returnify-kernels
      (lower-vectors
       (compile-harlan-frontend '(module
                                  (fn main ()
                                      (print (vector (vector 1)
                                                     (vector 2 3)
                                                     (vector 4 5 6)))
                                      (return 0)))))))))
  (let ((g1 (vector 'g1))
        (g2 (vector 'g2))
        (g3 (vector 'g3))
        (g4 (vector 'g4)))
    `((func int main ()
        (let ,g1 vec (mk_vec 1 1 (sizeof int)))
        (set! (deref (cast (ptr int) (vec_ref_1d ,g1 0))) 1)
        (let ,g2 vec (mk_vec 1 2 (sizeof int)))
        (set! (deref (cast (ptr int) (vec_ref_1d ,g2 0))) 2)
        (set! (deref (cast (ptr int) (vec_ref_1d ,g2 1))) 3)
        (let ,g3 vec (mk_vec 1 3 (sizeof int)))
        (set! (deref (cast (ptr int) (vec_ref_1d ,g3 0))) 4)
        (set! (deref (cast (ptr int) (vec_ref_1d ,g3 1))) 5)
        (set! (deref (cast (ptr int) (vec_ref_1d ,g3 2))) 6)
        (let ,g4 vec (mk_vec 2 3 (sizeof int)))
        (do (vec_set_vec (addressof ,g4) 0 ,g1))
        (do (vec_set_vec (addressof ,g4) 1 ,g2))
        (do (vec_set_vec (addressof ,g4) 2 ,g3))
        (print ,g4)
        (return 0)))))


(let ((prog (verify-compile-module
             (compile-module
              (uglify-vectors
               (returnify-kernels
                (lower-vectors
                 (compile-harlan-frontend '(module
                                             (fn main ()
                                                 (let a (reduce + (vector 1 1 1 1)))
                                                 (assert (= 4 (var a)))
                                                 (return 0)))))))))))
  (test 'annotate-1
    (annotate-free-vars prog)
    prog))


(let ((prog (verify-compile-module
             (compile-module
              (uglify-vectors
               (returnify-kernels
                (lower-vectors
                 (compile-harlan-frontend
                  '(module
                     (fn main ()
                         (let X (vector 1 2 3 4))
                         (let X (kernel ((x (var X))) (+ 1 (var x))))
                         (assert (= (vector-ref X 0) 2))
                         (assert (= (vector-ref X 1) 3))
                         (assert (= (vector-ref X 2) 4))
                         (assert (= (vector-ref X 3) 5))
                         (return 0)))))))))))
  (test-unify 'annotate-2
        (annotate-free-vars prog)
        (let ((g1 (vector 'g1))
              (g2 (vector 'g2))
              (g3 (vector 'g3))
              (g4 (vector 'g4)))
          `((func int main ()
              (let ,g1 vec (mk_vec 1 4 (sizeof int)))
              (set! (deref (cast (ptr int) (vec_ref_1d ,g1 0))) 1)
              (set! (deref (cast (ptr int) (vec_ref_1d ,g1 1))) 2)
              (set! (deref (cast (ptr int) (vec_ref_1d ,g1 2))) 3)
              (set! (deref (cast (ptr int) (vec_ref_1d ,g1 3))) 4)
              (let X vec ,g1)
              (let ,g2 vec (mk_vec 1 (vec_length X) (sizeof int)))
              (kernel (((,g3 int) ((var (vector int) ,g2) (vector int)))
                       ((x int) ((var (vector int) X) (vector int))))
                      (free-vars)
                      (set! ,g3 (+ 1 x)))
              (let X vec ,g2)
              (do (assert (= (deref (cast (ptr int) (vec_ref_1d X 0))) 2)))
              (do (assert (= (deref (cast (ptr int) (vec_ref_1d X 1))) 3)))
              (do (assert (= (deref (cast (ptr int) (vec_ref_1d X 2))) 4)))
              (do (assert (= (deref (cast (ptr int) (vec_ref_1d X 3))) 5))) (return 0))))))

(let ((prog (verify-compile-module
             (compile-module
              (uglify-vectors
               (returnify-kernels
                (lower-vectors
                 (compile-harlan-frontend
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
                         (return 0)))))))))))
  (test-unify 'annotate-3
    (annotate-free-vars prog)
    (let ((g1 (vector 'g1))
          (g2 (vector 'g2))
          (g3 (vector 'g3))
          (g4 (vector 'g4)))
      `((func int main ()
          (let ,g1 vec (mk_vec 1 4 (sizeof int)))
          (set! (deref (cast (ptr int) (vec_ref_1d ,g1 0))) 1)
          (set! (deref (cast (ptr int) (vec_ref_1d ,g1 1))) 2)
          (set! (deref (cast (ptr int) (vec_ref_1d ,g1 2))) 3)
          (set! (deref (cast (ptr int) (vec_ref_1d ,g1 3))) 4)
          (let X vec ,g1)
          (let ,g2 vec (mk_vec 1 4 (sizeof int)))
          (set! (deref (cast (ptr int) (vec_ref_1d ,g2 0))) 5)
          (set! (deref (cast (ptr int) (vec_ref_1d ,g2 1))) 1)
          (set! (deref (cast (ptr int) (vec_ref_1d ,g2 2))) 1)
          (set! (deref (cast (ptr int) (vec_ref_1d ,g2 3))) 7)
          (let Y vec ,g2)
          (let ,g3 vec (mk_vec 1 (vec_length X) (sizeof int)))
          (kernel (((,g4 int) ((var (vector int) ,g3) (vector int)))
                   ((x int) ((var (vector int) X) (vector int)))
                   ((y int) ((var (vector int) Y) (vector int))))
                  (free-vars)
                  (set! ,g4 (+ x y)))
          (let Z vec ,g3)
          (do (assert (= (deref (cast (ptr int) (vec_ref_1d Z 0))) 6)))
          (do (assert (= (deref (cast (ptr int) (vec_ref_1d Z 1))) 3)))
          (do (assert (= (deref (cast (ptr int) (vec_ref_1d Z 2))) 4)))
          (do (assert (= (deref (cast (ptr int) (vec_ref_1d Z 3))) 11)))
          (return 0))))))

(let ((prog (verify-compile-module
             (compile-module
              (uglify-vectors
               (returnify-kernels
                (lower-vectors
                 (compile-harlan-frontend '(module
                                             (fn main ()
                                                 (let X (vector 1 2 3))
                                                 (let y 4)
                                                 (let W (vector 5 6))
                                                 (let Z (kernel ((x (var X)))
                                                                (* (+ (var y) (var x)) (vector-ref (var W) 1))))
                                                 (assert (= (vector-ref (var Z) 1) 36))
                                                 (return 0)))))))))))
  (test-unify 'annotate-free-yw
              (annotate-free-vars prog)
              (let ((g1 (vector 'g1))
                    (g2 (vector 'g2))
                    (g3 (vector 'g3))
                    (g4 (vector 'g4)))
                `((func int main ()
                    (let v_26 vec (mk_vec 1 3 (sizeof int)))
                    (set! (deref (cast (ptr int) (vec_ref_1d v_26 0))) 1)
                    (set! (deref (cast (ptr int) (vec_ref_1d v_26 1))) 2)
                    (set! (deref (cast (ptr int) (vec_ref_1d v_26 2))) 3)
                    (let X vec v_26)
                    (let y int 4)
                    (let v_25 vec (mk_vec 1 2 (sizeof int)))
                    (set! (deref (cast (ptr int) (vec_ref_1d v_25 0))) 5)
                    (set! (deref (cast (ptr int) (vec_ref_1d v_25 1))) 6)
                    (let W vec v_25)
                    (let v_24 vec (mk_vec 1 (vec_length X) (sizeof int)))
                    (kernel (((retval_6 int) ((var (vector int) v_24) (vector int)))
                             ((x int) ((var (vector int) X) (vector int))))
                            (free-vars (y int) (W vec))
                            (set! retval_6 (* (+ y x) (deref (cast (ptr int) (vec_ref_1d W 1))))))
                    (let Z vec v_24)
                    (do (assert (= (deref (cast (ptr int) (vec_ref_1d Z 1))) 36)))
                    (return 0))))))

(let ((prog (verify-compile-module
             (compile-module
              (uglify-vectors
               (returnify-kernels
                (lower-vectors
                 (compile-harlan-frontend '(module
                                             (fn main ()
                                                 (let X (vector 1 2 3))
                                                 (for (i 0 (length (var X)))
                                                   (let x (+ (vector-ref X (var i)) 1))
                                                   (vector-set! (var X) (var i) (var x)))
                                                 (assert (= (vector-ref X 0) 2))
                                                 (return 0)))))))))))
  (test-unify 'annotate-incr
              (annotate-free-vars prog)
              (let ((g1 (vector 'g1))
                    (g2 (vector 'g2))
                    (g3 (vector 'g3))
                    (g4 (vector 'g4)))
                `???)))

(let ((prog (verify-compile-module
             (compile-module
              (uglify-vectors
               (returnify-kernels
                (lower-vectors
                 (compile-harlan-frontend '(module
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
                                                 (return 0)))))))))))
  (test-unify 'annotate-dmm-loop
              (annotate-free-vars prog)
              (let ((g1 (vector 'g1))
                    (g2 (vector 'g2))
                    (g3 (vector 'g3))
                    (g4 (vector 'g4)))
                `???)))

(let ((prog (verify-compile-module
             (compile-module
              (uglify-vectors
               (returnify-kernels
                (lower-vectors
                 (compile-harlan-frontend '(module
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
                                                 (return 0)))))))))))
  (test-unify 'annotate-smv
              (annotate-free-vars prog)
              (let ((g1 (vector 'g1))
                    (g2 (vector 'g2))
                    (g3 (vector 'g3))
                    (g4 (vector 'g4)))
                `???)))
