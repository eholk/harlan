#! /usr/bin/env scheme-script

; run-tests.scm <filename-with-extension>
; For example,
;   ./run-tests.scm bench-add-vector.kfc
; generates the file
;   test.bin/bench-add-vector.kfc.out
; containing timing information

(import
  (chezscheme)
  (util color)
  (elegant-weapons match)
  (util system)
  (harlan driver)
  (harlan compiler)
  (harlan compile-opts))

(define failures  (make-parameter 0))
(define successes (make-parameter 0))
(define ignored   (make-parameter 0))

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

(define (iterate source iters yield)
  (if (null? iters)
      (yield source)
      (begin
        (match (car iters)
          ((,x (range ,start ,stop))
           (if (benchmark)
               (let loop ((i start))
                 (if (= i stop)
                     (iterate (substq i x source) (cdr iters) yield)
                     (begin
                       (iterate (substq i x source) (cdr iters) yield)
                       (loop (add1 i)))))
               (iterate (substq start x source) (cdr iters) yield)))
          ((,x (range ,start ,stop ,step))
           (if (benchmark)
               (let loop ((i start))
                 (if (<= i stop)
                     (begin
                       (iterate (substq i x source) (cdr iters) yield)
                       (loop (+ step i)))))
               (iterate (substq start x source) (cdr iters) yield)))
          (,else (error 'iterate "Invalid iteration clause" else))))))


(define (do-test test)
  (let* ((path (join-path "test" test))
         (bin-path (join-path "./test.bin" (string-append test ".bin")))
         (out-path (join-path "./test.bin" (string-append test ".out")))
         (test-source
          (lambda (source)
            (printf "Generating C++...")
            (try (catch (x)
                   (if (or (error? x) (condition? x))
                       (begin
                         (failures (add1 (failures)))
                         (with-color 'red (printf "FAILED\n")))))
                 (let ((c++ (harlan->c++ source)))
                   (printf "OK\n")
                   (printf "Compiling...")
                   (g++-compile-stdin c++ bin-path)
                   (printf "OK\n")
                   (printf "Running test...")
                   (if (zero? (system (string-append bin-path " >> " out-path)))
                       (begin
                         (successes (add1 (successes)))
                         (with-color 'green (printf "OK\n")))
                       (error 'do-test "Test execution failed.")))))))
    (printf "Test ~a\n" path)
    (let-values (((source spec) (read-source path)))
      (if (assq 'xfail spec)
          (begin
            (ignored (add1 (ignored)))
            (with-color 'yellow (printf "IGNORED\n")))
          (begin
            (delete-file out-path)
            (if (assq 'iterate spec)
                (iterate source (cdr (assq 'iterate spec)) test-source)
                (test-source source)))))))

(define (do-*all*-the-tests)
  (begin
    (map do-test (enumerate-tests))
    (printf "Successes: ~a; Failures: ~a; Ignored: ~a; Total: ~a\n"
      (format-in-color 'green (successes))
      (format-in-color (if (zero? (failures)) 'green 'red) (failures))
      (format-in-color (if (zero? (ignored)) 'green 'yellow) (ignored))
      (+ (successes) (failures) (ignored)))
    (zero? (failures))))

(define (run-tests cl)
  (let loop ((cl (parse-args (cdr cl))))
    (cond
     ((null? cl)
      (if (do-*all*-the-tests) (exit) (exit #f)))
     (else
      (begin (do-test (car cl)) (exit))))))

; Uncomment this line to run the benchmarks
(benchmark #t)

(run-tests (command-line))
