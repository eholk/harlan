;; -*- scheme -*-

;; Simple file showing the use of global variables, and ensuring we
;; can represent stuff from CL++.

((global int x 4)
 (func int main ()
       (do (assert (= x 4)))
       (return 0)))
