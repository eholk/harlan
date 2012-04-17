(library
    (util system)
  (export read-all shell get-os join-path HARLAND)
  (import
   (chezscheme)
   (only (elegant-weapons helpers) join))

  (define (read-all port)
  (let loop ((result ""))
    (let ((read (get-string-some port)))
      (if (eof-object? read)
          result
          (loop (string-append result read))))))

  ;; Like system, but returns a string containing what the process
  ;; printed to stdout.
  (define (shell command)
    (let-values (((to-stdin from-stdout from-stderr proccess-id)
                  (open-process-ports command 'block (native-transcoder))))
      (read-all from-stdout)))
  
  (define (get-os)
    (let ((uname (shell "uname")))
      (cond
        ((string=? uname "Darwin\n") 'darwin)
        ((string=? uname "Linux\n") 'linux)
        (else 'unknown))))

  (define (join-path . components)
    (join (string (directory-separator)) components))

  (define HARLAND (make-parameter "."))
  
  ;; end library
  )
   