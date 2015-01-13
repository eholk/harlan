#! /usr/bin/env scheme-script
;; -*- scheme -*-
(import
  (rnrs)
  (harlan compile-opts)
  (harlan driver)
  (harlan backend print-c)
  (harlan compiler)
  (util compat)
  (util system) ;; HARLAND
  )

;; This could be set from the env var HARLAND, or based on the
;; directory in which this script resides.  Using the latter:
(HARLAND (path-parent (car (command-line))))

(define print-compile-harlan
  (lambda (filename)
    (let-values (((input testspec) (read-source filename)))
      (if (assq 'iterate testspec)
          (error 'harlanc
                 "Test iteration is not supported. Use run-tests.scm instead.")
          (let* ((c-expr (compile-harlan input))
                 (c-code (if (timing)
                             (time (harlan-format-c c-expr))
                             (harlan-format-c c-expr))))
            (if (verbosity? trace-pass-verbosity-level) (printf c-code))
            (g++-compile-stdin c-code (output-filename filename)))))))

(define (harlanc args)
  (let ((args (parse-args (cdr args))))
    (unless (null? args)
      ;; There should be a usage.
      (let ((filename (car args)))
        (print-compile-harlan filename)))))

;;(trace-pass 'hoist-kernels)

(harlanc (command-line))
