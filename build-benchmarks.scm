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

(define (enumerate-tests)
  '("bench-add-vector.kfc"
    "bench-dot-prod.kfc"
    "bench-nbody.kfc"
    "bench-bfs-color.kfc"))

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
  (let* ((path (join-path "test" test))
         (bin-path (join-path "./test.bin" (string-append test ".bin")))
         (out-path (join-path "./test.bin" (string-append test ".out")))
         (test-source
          (lambda (name source)
            (printf "Generating C++...")
            (flush-output-port (current-output-port))
            (try (catch (x)
                   (if (or (error? x) (condition? x))
                       (begin
                         (with-color 'red (printf "FAILED\n")))))
                 (let ((c++ (harlan->c++ source)))
                   (printf "OK\n")
                   (printf "Compiling...")
                   (g++-compile-stdin c++ bin-path)
                   (printf "OK\n"))))))
    (printf "Test ~a\n" path)
    (flush-output-port (current-output-port))
    (let-values (((source spec) (read-source path)))
      (begin
        (if (file-exists? out-path) (delete-file out-path))
        (test-source path source)))))

(define (do-*all*-the-tests)
  (begin
    (map do-test (enumerate-tests))
    (flush-output-port (current-output-port))))

(define (run-tests cl)
  (let loop ((cl (parse-args (cdr cl))))
    (cond
     ((null? cl)
      (if (do-*all*-the-tests) (exit) (exit #f)))
     (else
      (begin (do-test (car cl)) (exit))))))

(run-tests (command-line))
