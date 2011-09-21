;; -*- scheme -*-

;; code for adding two vectors, from Harlan proposal

(module
  (fn main ()
      (let X (vector 1 2 3 4))
      (let Y (vector 0 0 0 0))
      (set! Y (kernel ((x (var X)))
                (var x)))
      (return 0)))
