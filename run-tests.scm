#! /usr/bin/env scheme-script

(import (chezscheme)
        (util color)
        (only (util helpers) join)
        (harlan compiler))

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
                        (set-color 'red)
                        (printf "FAILED\n")
                        (set-color 'default)
                        (k 'failed))))
                (lambda ()
                  (let ((c++ (harlan->c++ source)))
                    (printf "OK\n")
                    'success)))))))

(define (do-all-tests)
  (map do-test (enumerate-tests)))

(do-all-tests)
