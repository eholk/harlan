(library
    (util system)
  (export read-all shell get-os join-path HARLAND)
  (import
   (rnrs)
   (only (util compat) make-parameter)
   (only (elegant-weapons helpers) join))

  (define read-all get-string-all)

  ;; Like system, but returns a string containing what the process
  ;; printed to stdout.
  (define (shell command)
    (assert #f)
    #;(let-values (((to-stdin from-stdout from-stderr proccess-id)
                  (open-process-ports command 'block (native-transcoder))))
      (read-all from-stdout)))
  
  (define (get-os)
    'linux
    #;(let ((uname (shell "uname")))
      (cond
        ((string=? uname "Darwin\n") 'darwin)
        ((string=? uname "Linux\n") 'linux)
        (else 'unknown))))

  (define (join-path . components)
    (join "/" components))

  (define HARLAND (make-parameter "."))
  
  ;; end library
  )
   
