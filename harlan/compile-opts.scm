(library
  (harlan compile-opts)
  (export
    passes
    verbose
    benchmark
    verify
    generate-debug
    make-shared-object
    no-kernels
    test-tags
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
(define no-kernels         (make-parameter #f))
(define test-tags  (make-parameter '((xfail . -) (bench . -))))

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

(define-syntax decode-pattern
  (syntax-rules ()
    ((_ loop args () body ...)
     (begin
       body ...
       (loop args)))
    ((_ loop args (pat pat* ...) body ...)
     (let ((pat (car args))
           (x (cdr args)))
       (decode-pattern loop x (pat* ...) body ...)))))
    
(define-syntax match-args
  (syntax-rules ()
    ;; pat-args is not yet used, but it's supposed to be for things
    ;; like "-o file.out"
    ((_ args
        (((long short ...) pat-args ...) body body* ...) ...)
     (let loop ((x args))
       (cond
         ((null? x) '())
         ((or (string=? long  (car x))
              (string=? short (car x)) ...)
          (decode-pattern loop (cdr x) (pat-args ...) body body* ...))
         ...
         (else (cons (car x) (loop (cdr x)))))))))
          
(define (parse-args command-line)
  (match-args command-line
    ((("--no-optimize" "-O0")) (optimize-level 0))
    ((("--verbose" "-v"))      (verbose #t))
    ((("--debug" "-g"))        (generate-debug #t))
    ((("--shared" "-s"))       (make-shared-object #t))
    ((("--quiet" "-q"))        (quiet #t))
    ((("--no-kernels"))        (no-kernels #t))
    ((("--no-verify" "-V"))    (verify #f))
    ((("--time" "-t"))         (timing #t))
    ((("--tags" "-x") tags)    (parse-tags tags))))

(define (string-search needle haystack)
  (let loop ((i 0))
    (cond
      ((>= i (string-length haystack)) #f)
      ((char=? needle (string-ref haystack i)) i)
      (else (loop (+ 1 i))))))

(define (parse-tags tags)
  (for-each (lambda (t)
              (let ((tag (string->symbol (substring t 1 (string-length t))))
                    (op (string->symbol (substring t 0 1))))
                (test-tags (cons (cons tag op) (test-tags)))))
            (reverse (string-split #\, tags))))

(define (string-split sep str)
  (let ((i (string-search sep str)))
    (if i
        (cons (substring str 0 i)
              (string-split sep (substring str (+ 1 i) (string-length str))))
        (list str))))

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
