

(define verbose-mode 
  (let ((mode (getenv "HARLAN_VERBOSE")))
    (cond 
      [(not mode) 0]
      [(equal? mode "") 0]
      [(string->number mode) => (lambda (x) x)]
      [else (error "invalid setting for HARLAN_VERBOSE: ~a" mode)])))
(define (vprintf lvl . args) (when (>= verbose-mode lvl) (apply printf args)))

(vprintf 1 " <Harlan> Loading compiler, machine type ~a...\n" (machine-type))(flush-output-port)

(import
  (chezscheme)
  (harlan compile-opts)
  (harlan driver)
  (elegant-weapons print-c)
  (harlan compiler))

(vprintf 1 " <Harlan>    Done loading compiler.\n")

;; Done loading and initializing, Now return control to the enclosing
;; C program in which we are embedded.
(exit 0)
