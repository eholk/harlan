(library
  (harlan driver)
  (export get-cflags g++-compile-stdin read-source)
  (import
    (chezscheme)
    (only (elegant-weapons helpers) join)
    (elegant-weapons match)
    (util system)
    (harlan compile-opts))

  (define (get-cflags)
    (case (get-os)
      ('darwin '("-framework OpenCL"))
      ('linux  '("-I/opt/cuda/include" "-I/usr/local/cuda/include"
                 "-lOpenCL" "-lrt"))))

  (define (g++-compile-stdin src outfile . args)
    (let* ((src-tmp (if (generate-debug)
                         (string-append outfile ".cpp")
                         "-"))
           (command
            (join " " (append `("clang++"
                                ,(if (generate-debug) "-g" "")
                                ,(if (make-shared-object) "-shared" "")
                                "-Wno-unused-value"
                                "-Wno-comment"
                                "-O2"
                                "-x c++"
                                ,src-tmp "-x none"
                                ,(string-append (HARLAND) "/rt/libharlanrt.a")
                                ,(string-append "-I" (HARLAND) "/rt")
                                "-o" ,outfile
                                "-lm")
                              (get-cflags)
                              args))))
      (if (verbose)
          (begin (display command) (newline)))
      (if (generate-debug)
          (let ((out (open-output-file src-tmp '(truncate))))
            (display src out)
            (close-output-port out)))
      (let-values (((to-stdin from-stdout from-stderr proccess-id)
                    (open-process-ports command 'block (native-transcoder))))
        (unless (generate-debug)
          (begin
            (display src to-stdin)
            (close-output-port to-stdin)))
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
      (xfail (error 'parse-testspec "xfail is now a tag"))
      (run-fail '(run-fail))
      ((iterate ,iterspec* ...)
       (error 'parse-testspec "iteration is no longer supported"))
      ((%tags ,tags ...) `(tags ,tags ...))
      (,else (error 'parse-testspec "Invalid test specification" else))))

  ;;end library
  )
