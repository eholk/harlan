

(define verbose-mode 
  (let ((mode (getenv "HARLAN_VERBOSE")))
    (cond 
      [(not mode) 0]
      [(equal? mode "") 0]
      [(string->number mode) => (lambda (x) x)]
      [else (error "invalid setting for HARLAN_VERBOSE: ~a" mode)])))
(define (vprintf lvl . args) (when (>= verbose-mode lvl) (apply printf args)))

(define (HarlanJit insig outsig name def)
  (printf "  *** JITing: ~a ~a ~a ~a (FINISH ME)\n" insig outsig name def)
  999
  )

(define HarlanJit-entry 
  (let ([x (foreign-callable HarlanJit (string string string string) int)])
    (lock-object x)
    (foreign-callable-entry-point x)))

(define (HarlanRun hndl insig outsig arrin arrout)
  (printf "  *** Running: ~a ~a (FINISH ME)\n" insig outsig)
  )

(define HarlanRun-entry 
  (let ([x (foreign-callable HarlanRun (int string string void* void*) void)])
    (lock-object x)
    (foreign-callable-entry-point x)))


(vprintf 1 " <Harlan> Loading compiler, machine type ~a...\n" (machine-type))(flush-output-port)

(library-directories (cons (getenv "HARLAND") (library-directories)))

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
