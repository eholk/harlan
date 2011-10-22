(library
  (harlan compile-opts)
  (export
    passes
    verbose
    trace-pass)
  (import
    (rnrs)
    (util color)
    (only (chezscheme) pretty-print))

(define verbose
  (let ((flag #f))
    (case-lambda
      (() flag)
      ((x) (set! flag x)))))

(define trace-pass
  (lambda (m pass expr)
    (if (verbose)
        (begin
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