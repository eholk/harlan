; run-tests.scm <filename-with-extension>
; For example,
;   ./run-tests.scm bench-add-vector.kfc
; generates the file
;   test.bin/bench-add-vector.kfc.out
; containing timing information

(import
  (rnrs)
  (util color)
  (elegant-weapons match)
  (elegant-weapons sets)
  (except (elegant-weapons compat) make-parameter parameterize)
  (util system)
  (util compat)
  (harlan driver)
  (harlan compiler)
  (harlan compile-opts))

(define failures  (make-parameter '()))
(define successes (make-parameter 0))
(define ignored   (make-parameter 0))

(define include-tags (make-parameter '()))
(define exclude-tags (make-parameter '()))

(define (is-test? filename)
  (let ((len (string-length filename)))
    (and (> len 4)
         (string=? (substring filename (- len 4) len) ".kfc"))))

(define (enumerate-tests)
  (filter is-test? (directory-list "test")))

(define-syntax try
  (syntax-rules (catch)
    ((_ (catch (x) handler-body ... e)
        body ...)
     (call/cc (lambda (k)
                (with-exception-handler
                 (lambda (x)
                   handler-body ... (k e))
                 (lambda ()
                   body ...)))))))

(define (decode-tags)
  (let loop ((tags (test-tags)))
    (unless (null? tags)
      (begin
        (loop (cdr tags))
        (let ((tag (caar tags))
              (op (cdar tags)))
          (case op
            ((-) (begin (include-tags (remove tag (include-tags)))
                        (exclude-tags (cons tag (exclude-tags)))))
            ((+) (begin (exclude-tags (remove tag (exclude-tags)))
                        (include-tags (cons tag (include-tags)))))))))))

(define (do-test test)
  (let* ((path (join-path "test" test))
         (bin-path (join-path "./test.bin" (string-append test ".bin")))
         (out-path (join-path "./test.bin" (string-append test ".out"))))
    (printf "Test ~a\n" path)
    (flush-output-port (current-output-port))
    (let-values (((source spec) (read-source path)))
      (let ((test-source
             (lambda ()
               (call/cc
                (lambda (k)
                  (let ((succeed (lambda ()
                                   (successes (add1 (successes)))
                                   (with-color 'green (printf "OK\n"))
                                   (k #t)))
                        (fail (lambda message
                                (failures (cons path (failures)))
                                (unless (null? message)
                                  (newline)
                                  (display (car message))
                                  (newline))
                                (with-color 'red (printf "FAILED\n"))
                                (k #f))))
                    (printf "Generating C++...")
                    (flush-output-port (current-output-port))
                    (try (catch (x)
                           (if (or (error? x) (condition? x))
                               (fail)))
                         (let ((c++ (if (assq 'compile-fail spec)
                                        (try (catch (x)
                                               (if (or (error? x)
                                                       (condition? x))
                                                   (succeed)))
                                             (harlan->c++ source)
                                             (fail "program compiled when it shouldn't"))
                                        (harlan->c++ source))))
                           (printf "OK\n")
                           (printf "Compiling...")
                           (g++-compile-stdin c++ bin-path)
                           (printf "OK\n")
                           (printf "Running test")
                           (if (assq 'run-fail spec)
                               (printf " [failure expected]"))
                           (printf "...")
                           (flush-output-port (current-output-port))
                           (if (zero? (system (string-append bin-path
                                                             " >> " out-path
                                                             " 2>&1")))
                               (if (assq 'run-fail spec)
                                   (fail "programm ran successfully but shouldn't have")
                                   (succeed))
                               (if (assq 'run-fail spec)
                                   (succeed)
                                   (begin
                                     (if (print-failed-logs)
                                         (system (string-append "cat " out-path)))
                                     (error 'do-test
                                            "Test execution failed.")))))))))))
            (tags (assq 'tags spec)))
        (let ((tags (if tags (cdr tags) '())))
          (if (and (subset? (include-tags) tags)
                   (null? (intersection (exclude-tags) tags)))
              (begin
                (if (file-exists? out-path) (delete-file out-path))
                (test-source))
              (begin
            (ignored (add1 (ignored)))
            (with-color 'yellow (printf "IGNORED\n")))))))))

(define (do-*all*-the-tests)
  (begin
    (decode-tags)
    (map do-test (enumerate-tests))
    (unless (null? (failures))
      (set-color 'red)
      (printf "Some tests failed:\n")
      (for-each (lambda (name)
                  (printf "    ~a\n" name))
                (failures))
      (set-color 'default))
    (printf "Successes: ~a; Failures: ~a; Ignored: ~a; Total: ~a\n"
      (format-in-color 'green (successes))
      (format-in-color (if (zero? (length (failures))) 'green 'red)
                       (length (failures)))
      (format-in-color (if (zero? (ignored)) 'green 'yellow) (ignored))
      (+ (successes) (length (failures)) (ignored)))
    (flush-output-port (current-output-port))
    (zero? (length (failures)))))

(define (run-tests cl)
  (let loop ((cl (parse-args (cdr cl))))
    (cond
     ((null? cl)
      (if (do-*all*-the-tests) (exit) (exit #f)))
     (else
      (begin (do-test (car cl)) (exit))))))

(run-tests (command-line))
