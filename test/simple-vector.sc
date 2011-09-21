;; -*- scheme -*-

((func int main () 
       (block
        (let t_0 (vector int) 4)
        (vector-set! t_0 0 1)
        (vector-set! t_0 1 2)
        (vector-set! t_0 2 3)
        (vector-set! t_0 3 4)
        (print t_0))
       (return 0)))
