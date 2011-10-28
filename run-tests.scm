#! /usr/bin/env scheme-script

(import (chezscheme)
        (util color)
        (util match)
        (only (util helpers) join)
        (harlan compiler))

(define failures (make-parameter 0))
(define successes (make-parameter 0))
(define ignored (make-parameter 0))

(define (join-path . components)
  (join (string (directory-separator)) components))

(define (is-test? filename)
  (equal? (path-extension filename) "kfc"))

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

(define (do-test test)
  (let ((path (join-path "test" test))
        (bin-path (join-path "./test.bin" (string-append test ".bin")))
        (out-path (join-path "./test.bin" (string-append test ".out"))))
    (printf "Test ~a\n" path)
    (let ((source (read (open-input-file path))))
      (match source
        ((%testspec xfail)
         (ignored (add1 (ignored)))
         (with-color 'yellow (printf "IGNORED\n")))
        (,else
         (printf "Generating C++...")
         (try (catch (x)
                (if (error? x)
                    (begin
                      (failures (add1 (failures)))
                      (with-color 'red (printf "FAILED\n")))))
              (let ((c++ (harlan->c++ source)))
                (printf "OK\n")
                (printf "Compiling...")
                (g++-compile-stdin c++ bin-path)
                (printf "OK\n")
                (printf "Running test...")
                (if (zero? (system (string-append bin-path " > " out-path)))
                    (begin
                      (successes (add1 (successes)))
                      (with-color 'green (printf "OK\n")))
                    (error 'do-test "Test execution failed.")))))))))

(define (do-*all*-the-tests)
  (begin
    (map do-test (enumerate-tests))
    (printf "Successes: ~a; Failures: ~a; Ignored: ~a; Total: ~a\n"
      (format-in-color 'green (successes))
      (format-in-color (if (zero? (failures)) 'green 'red) (failures))
      (format-in-color (if (zero? (ignored)) 'green 'yellow) (ignored))
      (+ (successes) (failures) (ignored)))
    (zero? (failures))))

(if (null? (cdr (command-line)))
    (if (do-*all*-the-tests)
        (exit)
        (exit #f))
    (let ((test-name (cadr (command-line))))
      (do-test test-name)))
