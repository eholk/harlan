(library
    (harlan driver)
  (export get-cflags g++-compile-stdin read-source)
  (import
   (chezscheme)
   (only (util helpers) join)
   (util match)
   (util system))

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

  (define (read-source path)
    (let* ((file (open-input-file path))
           (source (read file)))
      (match source
        ((%testspec ,[parse-testspec -> spec*] ...)
         (values (read file) spec*))
        ((module ,decl* ...)
         (values source '())))))

  (define (parse-testspec spec)
    (match spec
      (xfail `(xfail))
      ((iterate ,iterspec* ...)
       `(iterate . ,iterspec*))
      (,else (error 'parse-testspec "Invalid test specification" else))))

  ;;end library
  )
