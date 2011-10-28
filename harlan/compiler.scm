(library
  (harlan compiler)
  (export compile-harlan harlan->c++ g++-compile-stdin)
  (import
   (chezscheme)
   (only (util helpers) join)
   (harlan compile-opts)
   (harlan front compile-front)
   (harlan middle compile-middle)
   (harlan back print-c))

(define compile-harlan
  (passes
    compile-harlan-frontend
    compile-harlan-middle))

(define harlan->c++
  (passes compile-harlan format-c))

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

(define (get-cflags)
  (case (get-os)
    ('darwin '("-framework OpenCL"))
    ('linux  '("-I/opt/cuda/include" "-lOpenCL" "-lrt"))))

(define (g++-compile-stdin src outfile . args)
  (let ((command
         (join " " (append `("g++"
                             "-x c++ - -x none"
                             "rt/libharlanrt.a"
                             "gc/lib/libgc.a"
                             "-Irt"
                             "-Igc/include"
                             "-o" ,outfile)
                           (get-cflags)
                           args))))
    (let-values (((to-stdin from-stdout from-stderr proccess-id)
                  (open-process-ports command 'block (native-transcoder))))
      (display src to-stdin)
      (close-output-port to-stdin)
      (let ((errors (read-all from-stderr)))
        (if (string=? "" errors)
            #t ;; Assume that if we get no stderr data then g++ succeeded.
            (error 'g++-compile-stdin errors))))))
)
