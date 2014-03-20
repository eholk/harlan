(library
  (harlan compile-opts)
  (export
    passes
    allow-complex-kernel-args
    benchmark
    danger-zone
    verbose
    generate-debug
    harlan-library-path
    harlan-runtime-path
    make-shared-object
    no-kernels
    test-tags
    trace-pass
    untrace-pass
    parse-args
    print-failed-logs
    use-doubles
    quiet
    dump-call-graph
    timing
    verify)
  (import
    (rnrs)
    (util color)
    (except (elegant-weapons compat) make-parameter parameterize)
    (only (elegant-weapons helpers) join)
    (util compat))

(define allow-complex-kernel-args (make-parameter #f))
(define danger-zone        (make-parameter #f))
(define verbose            (make-parameter #f))
(define verify             (make-parameter #f))
(define quiet              (make-parameter #f))
(define timing             (make-parameter #f))
(define benchmark          (make-parameter #f))
(define generate-debug     (make-parameter #f))
(define make-shared-object (make-parameter #f))
(define optimize-level     (make-parameter 1))
(define no-kernels         (make-parameter #f))
(define use-doubles        (make-parameter #f))
(define test-tags          (make-parameter '((xfail . -) (bench . -))))
(define dump-call-graph    (make-parameter #f))
(define harlan-library-path (make-parameter "lib/harlan"))
(define harlan-runtime-path (make-parameter "rt"))
(define print-failed-logs  (make-parameter #f))

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

(define-syntax if-docstring
  (lambda (x)
    (syntax-case x ()
      ((_ s conseq alt)
       (if (string? (syntax->datum #'s))
           #'conseq
           #'alt)))))

(define-syntax decode-pattern
  (syntax-rules ()
    ((_ loop args () body body* ...)
     (begin
       (if-docstring body (if #f 5) body)
       body* ...
       (loop args)))
    ((_ loop args (pat pat* ...) body ...)
     (let ((pat (car args))
           (x (cdr args)))
       (decode-pattern loop x (pat* ...) body ...)))))

(define-syntax display-one-help
  (syntax-rules ()
    ((_ (((long short ...) args ...) body))
     (let ((arg-string (join ", " (list long short ...))))
       (display arg-string)
       (if-docstring body
                     (cond
                       ((< (string-length arg-string) 30)
                        (display (make-string
                                  (- 30 (string-length arg-string))
                                  #\space))
                        (display body)
                        (newline))
                       (else (display (make-string 30 #\space))
                             (display body)
                             (newline)))
                     (if #f 5))
       (newline)))))
                                    

(define-syntax generate-help
  (syntax-rules ()
    ((_ (((long short ...) args ...) body) ...)
     (lambda ()
       (display "Harlan supports these command line arguments:\n\n")
       (display-one-help (((long short ...) args ...) body)) ...))))
  
(define-syntax match-args
  (syntax-rules ()
    ;; pat-args is not yet used, but it's supposed to be for things
    ;; like "-o file.out"
    ((_ args
        (((long short ...) pat-args ...) body body* ...) ...)
     (let ((display-help (generate-help
                          (((long short ...) pat-args ...) body) ...)))
       (let loop ((x args))
         (cond
           ((null? x) '())
           ((string=? "--help" (car x))
            (display-help)
            (exit))
           ((or (string=? long  (car x))
                (string=? short (car x)) ...)
            (decode-pattern loop (cdr x) (pat-args ...) body body* ...))
           ...
           (else
            (if (eq? #\- (string-ref (car x) 0))
                (error 'match-args "unrecognized option" (car x))
                (cons (car x) (loop (cdr x)))))))))))
          
(define (parse-args command-line)
  (match-args command-line
    ((("--danger-zone"))       "Disable runtime safety checks"
     (danger-zone #t))
    ((("--no-optimize" "-O0")) "Disable optimization"
     (optimize-level 0))
    ((("--verbose" "-v"))      "Output intermediate compilation results"
     (verbose #t))
    ((("--display-failure-logs"))
                               "Display logs for failed tests"
     (print-failed-logs #t))
    ((("--debug" "-g"))        "Generate debugging information"
     (generate-debug #t))
    ((("--enable-double"))     "Use double precision math"
     (use-doubles #t))
    ((("--libdirs" "-L") path) "Search these directories for libraries"
     (harlan-library-path path))
    ((("--shared" "-s"))       "Generate a library instead of an executable"
     (make-shared-object #t))
    ((("--quiet" "-q"))        "Generate less output"
     (quiet #t))
    ((("--no-kernels"))        "Do not generate OpenCL kernels"
     (no-kernels #t))
    ((("--no-verify" "-V"))    "Disable verification passes"
     (verify #f))
    ((("--rt-dir" "-R") path)  "Specify the location of Harlan's runtime"
     (harlan-runtime-path path))
    ((("--time" "-t"))         "Output pass timing information"
     (timing #t))
    ((("--tags" "-x") tags)    "Run tests matching this tag set"
     (parse-tags tags))
    ((("--dump-call-graph"))   "Save the call graph to a file"
     (dump-call-graph #t))
    ((("--Zallow-complex-kernel-args"))
     "Allow unboxed complex kernel parameters"
     (allow-complex-kernel-args #t))))

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
       (let ((e (time expr)))
         (with-color 'green
           (display "Timing complete for ") (display pass-name) (newline))
         e)))))

(define do-trace-pass
  (lambda (pass-name pass expr)
    (when (trace-pass? pass-name)
      (newline)
      (with-color 'green
        (display "Beginning pass ") (display pass-name))
      (newline)
      (flush-output-port (current-output-port)))
    (let ((expr (if (timing)
                    (add-time pass-name (pass expr))
                    (pass expr))))
      (when (trace-pass? pass-name)
        (with-color 'green
          (display "Pass ") (display pass-name) (display " output:"))
        (newline)
        (pretty-print expr)
        (newline)
        (flush-output-port (current-output-port)))
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
  (syntax-rules (nanopasses)
    ((_ (nanopasses . passes))
     (do-nanopasses . passes))
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

(define-syntax do-nanopasses
  (syntax-rules (: -> unless)
    ((_) (lambda (e) e))
    ((_ (unless e . clause*) . rest)
     (lambda (expr)
       ((do-nanopasses . rest)
        (if e
            expr
            ((do-nanopasses . clause*) expr)))))
    ((_ (pass : input -> output) . rest)
     (lambda (expr)
       ((np-middle-passes output (pass : input -> output) . rest)
        ((np-entry input) expr))))))

(define-syntax np-entry
  (lambda (x)
    (syntax-case x ()
      ((_ input)
       (let* ((parser (string->symbol
                       (string-append "parse-" (symbol->string
                                                (syntax->datum #'input)))))
              (parser (datum->syntax #'input parser)))
         #`(lambda (expr)
             (#,parser expr)))))))

(define-syntax np-exit
  (lambda (x)
    (syntax-case x ()
      ((_ output)
       (let* ((unparser (string->symbol
                         (string-append "unparse-" (symbol->string
                                                    (syntax->datum #'output)))))
              (unparser (datum->syntax #'output unparser)))
         #`(lambda (expr)
             (#,unparser expr)))))))

(define-syntax np-middle-passes
  (syntax-rules (: ->)
    ((_ output)
     (np-exit output))
    ((_ output^ (pass : input -> output) . rest)
     (lambda (expr)
       ((np-middle-passes output . rest)
        (if (timing)
            (add-time 'pass (pass expr))
            (pass expr)))))))

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
