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
    parse-args)
  (import
    (rnrs)
    (util color)
    (only (chezscheme) pretty-print make-parameter))
  
(define verbose (make-parameter #f))
(define verify (make-parameter #t))
(define benchmark (make-parameter #f))
(define generate-debug (make-parameter #f))
(define make-shared-object (make-parameter #f))

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
        (((long short) pat-args ...) body ...) ...)
     (let loop ((x args))
       (cond
         ((null? x) '())
         ((or (string=? long  (car x))
              (string=? short (car x)))
          body ...
          (loop (cdr x)))
         ...
         (else (cons (car x) (loop (cdr x)))))))))
          
(define (parse-args command-line)
  (match-args command-line
    ((("--verbose" "-v")) (verbose #t))
    ((("--debug" "-g"))   (generate-debug #t))
    ((("--shared" "-s"))  (make-shared-object #t))))

(define do-trace-pass
  (lambda (pass-name pass expr)
    (if (trace-pass? pass-name)
        (begin
          (newline)
          (set-color 'green) (display "Beginning pass ") (display pass-name)
          (set-color 'default) (newline)
          (let ((expr (pass expr)))
            (set-color 'green)
            (display "Pass ") (display pass-name) (display " output:")
            (set-color 'default) (newline)
            (pretty-print expr) (newline)
            expr))
        (pass expr))))

(define-syntax passes
  (syntax-rules ()
    ((_ pass-name ...)
     (lambda (expr)
       (let* ((expr (do-trace-pass 'pass-name pass-name expr))
              ...)
         expr)))))

;; end library
)