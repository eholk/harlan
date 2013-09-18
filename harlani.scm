(import
 (elegant-weapons match)
 (except (chezscheme) compile)
 (harlan compiler)
 (harlan compile-opts)
 (harlan driver))

;; copied from harlanc.scm.
(define (output-filename input)
  (let ((base (path-last (path-root input))))
    (if (make-shared-object)
        (string-append base ".so")
        base)))
 
(define (compile path)
  (let-values (((source testspec) (read-source path)))
    (g++-compile-stdin (harlan->c++ source) (output-filename path))))

(parse-args (command-line))

(display "Some useful options:\n")
(display " (make-shared-object)  -- Compile to a shared library instead of an executable\n")
(display " (harlan-library-path) -- Where Harlan finds imported libraries\n")
(display " (harlan-runtime-path) -- Where Harlan finds its runtime files\n")
(display " (verbose)             -- Make Harlan print detailed information\n")
(display " (debug)               -- Generate debugging information\n")
(display " (timing)              -- Display pass timing information\n")
(display "\n")
(display "Compile a file with (compile \"filename\")\n")

(define-syntax match-commands
  (syntax-rules ()
    ((_ e cmd ...)
     (match e
       ((cmd . ,args) (display (apply cmd args)) (newline)) ...
       (else (display "unrecognized command\n"))))))

(call/cc
 (lambda (exit)
   (let repl ()
     (display "> ")
     (match-commands
      (read)
      make-shared-object
      harlan-library-path
      harlan-runtime-path
      compile
      verbose
      debug
      timing
      exit)
     (repl))))
