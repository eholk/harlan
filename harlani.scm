(import
 (elegant-weapons match)
 (except (chezscheme) compile debug)
 (harlan compiler)
 (harlan compile-opts)
 (harlan driver)
 (only (util system) HARLAND))

;;; copied from harlanc.scm.

;; This could be set from the env var HARLAND, or based on the
;; directory in which this script resides.  Using the latter:
(HARLAND (path-parent (car (command-line))))

(define (compile path)
  (let-values (((source testspec) (read-source path)))
    (g++-compile-stdin (harlan->c++ source) (output-filename path))))

(parse-args (command-line))

(define (help)
  (display "Some useful options:\n")
  (display " (exit)                -- Exit harlani\n")
  (display " (make-shared-object)  -- Compile to a shared library instead of an executable\n")
  (display " (harlan-library-path) -- Where Harlan finds imported libraries\n")
  (display " (harlan-runtime-path) -- Where Harlan finds its runtime files\n")
  (display " (verbose)             -- Make Harlan print detailed information\n")
  (display " (debug)               -- Generate debugging information\n")
  (display " (timing)              -- Display pass timing information\n")
  (display " (trace-pass)          -- Turn on tracing for a given pass\n")
  (display " (untrace-pass)        -- Turn off tracing for a given pass\n")
  (display "\n")
  (display "Compile a file with (compile \"filename\")\n"))

(help)

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

(define-syntax match-commands
  (syntax-rules (=>)
    ((_ e cmd ...)
     (match e
       ((cmd . ,args)
        (try (catch (x) (display-condition x) (newline))
             (begin (display (apply cmd args)) (newline)))) ...
       (,else (display "unrecognized command\n"))))))

(define debug generate-debug)

(call/cc
 (lambda (exit)
   (let repl ()
     (display "> ")
     (match-commands (read)
      help
      make-shared-object
      harlan-library-path
      harlan-runtime-path
      compile
      no-kernels
      verbose
      debug
      timing
      trace-pass
      untrace-pass
      exit)
     (repl))))
