(library
  (harlan compile-opts)
  (export
    passes
    verbose
    benchmark
    verify
    generate-debug
    make-shared-object
    trace-pass
    untrace-pass
    parse-args
    quiet
    timing)
  (import
    (rnrs)
    (util color)
;;    (harlan helpers)
    (only (chezscheme) pretty-print make-parameter time))
  
(define verbose            (make-parameter #f))
(define verify             (make-parameter #t))
(define quiet              (make-parameter #f))
(define timing             (make-parameter #f))
(define benchmark          (make-parameter #f))
(define generate-debug     (make-parameter #f))
(define make-shared-object (make-parameter #f))
(define optimize-level     (make-parameter 1))

(define trace-passes '())

(define (trace-pass? pass)
  (or (verbose) (memq pass trace-passes)))

(define (trace-pass . passes)
  (map (lambda (pass)
         (unless (memq pass trace-passes)
           (set! trace-passes (cons pass trace-passes))))
       passes))

(define (untrace-pass . passes)
  (map (lambda (pass)
         (set! trace-passes (remove pass trace-passes)))
       passes))

(define-syntax match-args
  (syntax-rules ()
    ;; pat-args is not yet used, but it's supposed to be for things
    ;; like "-o file.out"
    ((_ args
        (((long short) pat-args ...) body body* ...) ...)
     (let loop ((x args))
       (cond
         ((null? x) '())
         ((or (string=? long  (car x))
              (string=? short (car x)))
          body body* ...
          (loop (cdr x)))
         ...
         (else (cons (car x) (loop (cdr x)))))))))
          
(define (parse-args command-line)
  (match-args command-line
    ((("--no-optimize" "-O0")) (optimize-level 0))
    ((("--verbose" "-v"))      (verbose #t))
    ((("--debug" "-g"))        (generate-debug #t))
    ((("--shared" "-s"))       (make-shared-object #t))
    ((("--quiet" "-q"))        (quiet #t))
    ((("--no-verify" "-V"))    (verify #f))
    ((("--time" "-t"))         (timing #t))))

(define-syntax add-time
  (syntax-rules ()
    ((_ pass-name expr)
     (begin
       (newline)
       (with-color 'green
         (display "Timing pass ") (display pass-name))
       (newline)
       (time expr)))))

(define do-trace-pass
  (lambda (pass-name pass expr)
    (when (trace-pass? pass-name)
      (newline)
      (with-color 'green
        (display "Beginning pass ") (display pass-name))
      (newline))
    (let ((expr (if (timing)
                    (add-time pass-name (pass expr))
                    (pass expr))))
      (when (trace-pass? pass-name)
        (with-color 'green
          (display "Pass ") (display pass-name) (display " output:"))
        (newline)
        (pretty-print expr)
        (newline))
      expr)))

(define do-verify-pass
  (lambda (pass-name pass expr)
    (when (trace-pass? pass-name)
      (newline)
      (with-color 'green
        (display "Beginning ") (display pass-name)))
    (if (timing) (add-time pass-name (pass expr)) (pass expr))
    (when (trace-pass? pass-name)
      (with-color 'green
        (display "...passed!"))
      (newline))))

(define-syntax single-pass
  (syntax-rules ()
    ((_ (pass-name))
     (lambda (expr)
       (do-trace-pass 'pass-name pass-name expr)))
    ((_ (pass-name verify-pass))
     (lambda (expr)
       (let ((expr (do-trace-pass 'pass-name pass-name expr)))
         (begin
           (if (verify)
               (do-verify-pass 'verify-pass verify-pass expr))
           expr))))
    ((_ (pass-name verify-pass olevel))
     (lambda (expr)
       (if (<= olevel (optimize-level))
           ((single-pass (pass-name verify-pass)) expr)
           expr)))))

(define-syntax passes
  (syntax-rules ()
    ((_ pass)
     (single-pass pass))
    ((_ pass rest ...)
     (lambda (expr)
       ((passes rest ...)
        ((single-pass pass) expr))))))

;; end library
)