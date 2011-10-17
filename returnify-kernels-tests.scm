(import (returnify) (lift-vectors) (typecheck) (returnify-kernels) (harlancompiler) (mk))

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

;;; (kernel ((x (var row)) (o (var out-vec)))
;;;   (set! (var o) (var x)))


;;; (kernel ((row (var Rows)))
;;;   (reduce + (kernel ((x (var row)))
;;;               (var x))))
;;; =>
;;; (let Outvecs (copy Rows))
;;; (let Outrows (vector (length Rows))) ;; Make a vector with the same
;;;                                      ;; length as rows.
;;; (kernel ((row (var Rows)) (out-vec (var Outvecs)) (row-out (Outrows)))
;;;         (kernel ((x (var row)) (o (var out-vec)))
;;;                 (set! (var o) (var x)))
;;;         (set! row-out (reduce + out-vec)))
;;; (set! result (reduce + Outrows)) ;; result is a scalar


;;; In addition to this pass, need to take care of free variable capture stuff:
;;; need a parameter list for all for the free variables in a kernel:  this parameter list
;;; won't be iterated over.
;;; This pass should happen in two steps:  annotate free variables that need to be captured,
;;; then generate the parameter list in hoist-kernels, in the generate-kernel helper function.


;;; Turn nested kernels into for loops.


(test-unify 'annotate-1
  (annotate-types
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
        (g9 (vector 'g9))
        (g10 (vector 'g10)))
    `(module
       (fn main () (() -> int)
         (let ,g1 (vector int) (vector (int 1) (int 2) (int 3)))
         (let ,g2 (vector int) (vector (int 4) (int 5) (int 6)))
         (let ,g3 (vector int) (vector (int 7) (int 8) (int 9)))
         (let ,g4 (vector int) (vector (int 10) (int 11) (int 12)))
         (let ,g5 (vector (vector int))
          (vector
            (var (vector int) ,g1)
            (var (vector int) ,g2)
            (var (vector int) ,g3)
            (var (vector int) ,g4)))
         (let Rows (vector (vector int))
           (var (vector (vector int)) ,g5))
;;;         
         (let ,g6 (vector int)
          (kernel (vector int)
            (((row (vector int))
               ((var (vector (vector int)) Rows) (vector (vector int)))))
            (let ,g7 (vector int)
              (kernel
                (vector int)
                (((x int) ((var (vector int) row) (vector int))))
                (var int x)))
            (let ,g8 int (reduce int + (var (vector int) ,g7)))
            (var int ,g8)))
;;;         
         (let ,g9 int (reduce int + (var (vector int) ,g6)))
         (let total int (var int ,g9))
         (assert (= (var int total) (int 78))) (return (int 0))))))

(test-unify 'returnify-kernels-1
  (returnify-kernels
    '(module
       (fn main () (() -> int)
         (let g1 (vector int) (vector (int 1) (int 2) (int 3)))
         (let g2 (vector int) (vector (int 4) (int 5) (int 6)))
         (let g3 (vector int) (vector (int 7) (int 8) (int 9)))
         (let g4 (vector int) (vector (int 10) (int 11) (int 12)))
         (let g5 (vector (vector int))
          (vector
            (var (vector int) g1)
            (var (vector int) g2)
            (var (vector int) g3)
            (var (vector int) g4)))
         (let Rows (vector (vector int))
           (var (vector (vector int)) g5))
;;;         
         (let g6 (vector int)
          (kernel (vector int)
            (((row (vector int))
               ((var (vector (vector int)) Rows) (vector (vector int)))))
            (let g7 (vector int)
              (kernel
                (vector int)
                (((x int) ((var (vector int) row) (vector int))))
                (var int x)))
            (let g8 int (reduce int + (var (vector int) g7)))
            (var int g8)))
;;;         
         (let g9 int (reduce int + (var (vector int) g6)))
         (let total int (var int g9))
         (assert (= (var int total) (int 78))) (return (int 0)))))
  '(module
       (fn main () (() -> int)
         (let g1 (vector int) (vector (int 1) (int 2) (int 3)))
         (let g2 (vector int) (vector (int 4) (int 5) (int 6)))
         (let g3 (vector int) (vector (int 7) (int 8) (int 9)))
         (let g4 (vector int) (vector (int 10) (int 11) (int 12)))
         (let g5 (vector (vector int))
          (vector
            (var (vector int) g1)
            (var (vector int) g2)
            (var (vector int) g3)
            (var (vector int) g4)))
         (let Rows (vector (vector int))
           (var (vector (vector int)) g5))
;;;
         (let g6 (vector int) (make-vector (length (var Rows))))
         ;;; is it okay to have only one g7? (basically it is a temporary vector)
         ;;; can we ensure each thread gets its own g7?
         (let g7 (vector int) (make-vector (length (vector-ref (var Rows) 0))))
         (kernel (vector int)
                 (((row (vector int))
                   ((var (vector (vector int)) Rows) (vector (vector int)))))
                 (kernel (vector int)
                         (((x int) ((var (vector int) row) (vector int))))
                         (set! g7 (var int x)))
                 (let g8 int (reduce int + (var (vector int) g7)))
                 (set! g6 (var int g8)))
;;;         
         (let g9 int (reduce int + (var (vector int) g6)))
         (let total int (var int g9))
         (assert (= (var int total) (int 78))) (return (int 0)))))

(test-unify 'returnify-dot-prod
            (returnify-kernels
             '(module
                (fn main () (() -> int)
                    (let v_4 (vector int) (int 4))
                    (vector-set! v_4 0 (int 1))
                    (vector-set! v_4 1 (int 2))
                    (vector-set! v_4 2 (int 3))
                    (vector-set! v_4 3 (int 4))
                    (let X (vector int) (var (vector int) v_4))
                    (let v_3 (vector int) (int 4))
                    (vector-set! v_3 0 (int 4))
                    (vector-set! v_3 1 (int 3))
                    (vector-set! v_3 2 (int 2))
                    (vector-set! v_3 3 (int 1))
                    (let Y (vector int) (var (vector int) v_3))
                    (let v_1 (vector int)
                      (kernel (vector int)
                              (((x int) ((var (vector int) X) (vector int)))
                               ((y int) ((var (vector int) Y) (vector int))))
                              (* (var int x) (var int y))))
                    (let v_2 int (vector-ref int (var (vector int) v_1)
                                             (int 0)))
                    (for (i_5 (int 1) (length (var (vector int) v_1)))
                         (set! (var int v_2)
                               (+ (var int v_2)
                                  (vector-ref int (var (vector int) v_1)
                                              (var int i_5)))))
                    (let dot int (var int v_2))
                    (assert (= (var int dot) (int 30)))
                    (return (int 0)))))
             '(correct-answer-goes-here))