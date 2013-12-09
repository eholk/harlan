;; -*- scheme -*-
(library
    (util compat)
  (export directory-list printf system unlink path-last path-root path-parent
          time with-output-to-string pretty-print make-parameter parameterize
          get-os)
  (import (chezscheme))

  ;; Like system, but returns a string containing what the process
  ;; printed to stdout.
  (define (shell command)
    (let-values (((to-stdin from-stdout from-stderr proccess-id)
                  (open-process-ports command 'block (native-transcoder))))
      (get-string-all from-stdout)))
  
  (define (get-os)
    (let ((uname (shell "uname")))
      (cond
        ((string=? uname "Darwin\n") 'darwin)
        ((string=? uname "Linux\n") 'linux)
        (else 'unknown))))
  
  (define unlink delete-file))
