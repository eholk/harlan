(library
  (harlan compile-opts)
  (export
    passes
    verbose
    benchmark
    verify
    trace-pass)
  (import
    (rnrs)
    (util color)
    (only (chezscheme) pretty-print make-parameter))
  
(define verbose (make-parameter #f))
(define verify (make-parameter #t))
(define benchmark (make-parameter #f))

(define trace-pass
  (lambda (m pass expr)
    (if (verbose)
        (begin
          (newline)
          (set-color 'green) (display "Beginning pass ") (display m)
          (set-color 'default) (newline)
          (let ((expr (pass expr)))
            (set-color 'green)
            (display "Pass ") (display m) (display " output:")
            (set-color 'default) (newline)
            (pretty-print expr) (newline)
            expr))
        (pass expr))))

(define-syntax passes
  (syntax-rules ()
    ((_ pass-name ...)
     (lambda (expr)
       (let* ((expr (trace-pass (symbol->string 'pass-name) pass-name expr))
              ...)
         expr)))))

;; end library
)