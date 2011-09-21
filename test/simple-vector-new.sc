;; -*- scheme -*-

;; This is for an older representation of vectors.
; xfail

;; Doesn't work with new vector representation
; xfail
((func int main () 
       (block
        (let t_0 vec (mk_vec 1 4 (sizeof int)))
        (set! (deref (cast (ptr int) (vec_ref_1d t_0 0))) 1)
        (set! (deref (cast (ptr int) (vec_ref_1d t_0 1))) 2)
        (set! (deref (cast (ptr int) (vec_ref_1d t_0 2))) 3)
        (set! (deref (cast (ptr int) (vec_ref_1d t_0 3))) 4)
        (do (print_int_vec t_0)
            (assert (= (deref (cast (ptr int) (vec_ref_1d t_0 0))) 1))
          (assert (= (deref (cast (ptr int) (vec_ref_1d t_0 1))) 2))
          (assert (= (deref (cast (ptr int) (vec_ref_1d t_0 2))) 3))
          (assert (= (deref (cast (ptr int) (vec_ref_1d t_0 3))) 4))
          (vec_destroy t_0))
       (return 0))))
