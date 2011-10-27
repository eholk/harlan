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


(library
  (fdneq)

  (export
    =/=
    usefdneq)

  (import
    (rnrs)
    (mk)
    (ck)
    (only (fd)
      process-prefixfd
      enforce-constraintsfd
      =/=fd)
    (only (neq) 
      reify-constraintsneq)
    (rename (neq) (=/= =/=neq)))

  ;; goals

  (define =/=
    (lambda (u v)
      (project (u v)
        (cond
          ((and (integer? u) (integer? v)) (=/=fd u v))
          (else (=/=neq u v))))))

  ;; to use the combined definitions, envoke (usefdneq)

  (define usefdneq
    (lambda ()
      (process-prefix process-prefixfd)
      (enforce-constraints enforce-constraintsfd)
      (reify-constraints reify-constraintsneq)))

  )

