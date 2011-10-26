#! /usr/bin/env scheme-script

(import (chezscheme)
        (util color)
        (only (util helpers) join)
        (harlan compiler))

(define failures (make-parameter 0))
(define successes (make-parameter 0))

(define (join-path . components)
  (join (string (directory-separator)) components))

(define (is-test? filename)
  (equal? (path-extension filename) "kfc"))

(define (enumerate-tests)
  (filter is-test?
          (map (lambda (f) (join-path "test" f)) (directory-list "test"))))

(define (do-test path)
  (printf "Test ~a\n" path)
  (let ((source (read (open-input-file path))))
    (printf "Generating C++...")
    (call/cc (lambda (k)
               (with-exception-handler
                (lambda (x)
                  (if (error? x)
                      (begin
                        (failures (add1 (failures)))
                        (set-color 'red)
                        (printf "FAILED\n")
                        (set-color 'default)
                        (k 'failed))))
                (lambda ()
                  (let ((c++ (harlan->c++ source)))
                    (begin
                      (successes (add1 (successes)))
                      (printf "OK\n")
                      'success))))))))

(define (do-all-tests)
  (begin
    (map do-test (enumerate-tests))
    (printf "Successes: ~s\nFailures: ~s\nTotal: ~s\n"
      (successes) (failures) (+ (successes) (failures)))))

(if (null? (cdr (command-line)))
    (do-all-tests)
    (let ((test-name (cadr (command-line))))
      (harlan->c++ (read (open-input-file test-name)))))
