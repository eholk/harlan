(library
  (harlan driver)
  (export get-cflags g++-compile-stdin read-source output-filename)
  (import
    (rnrs)
    (only (elegant-weapons helpers) join)
    (elegant-weapons match)
    (util system)
    (util compat)
    (harlan compile-opts))

  (define (get-cflags)
    (case (get-os)
      ('darwin '("-framework OpenCL"))
      ('linux  '("-I/opt/cuda/include" "-I/usr/local/cuda/include"
		 "-I/opt/nvidia/cudatoolkit/default/include"
		 "-L/opt/cray/nvidia/default/lib64/"
                 "-lOpenCL" "-lrt"))))

  (define (get-runtime)
    (if (make-shared-object)
        (string-append (HARLAND) "/rt/libharlanrts.a")
        (string-append (HARLAND) "/rt/libharlanrt.a")))
  
  ;; Converts foo/bar.kfc to bar
  (define (output-filename input)
    (let ((base (path-last (path-root input))))
      (if (make-shared-object)
          (string-append base (case (get-os)
                                ('linux ".so")
                                ('darwin ".dylib")))
          base)))

(define (g++-compile-stdin src outfile . args)
    (let* ((src-tmp (string-append outfile ".cpp"))
           (command
            (join " " (append `("g++"
                                ,(if (generate-debug) "-g" "")
                                ,(if (make-shared-object) "-shared -fPIC" "")
                                "-Wno-unused-value"
                                "-Wno-comment"
                                "-O2"
                                "-x c++"
                                ,src-tmp "-x none"
                                ,(get-runtime)
                                ,(string-append "-I" (HARLAND) "/rt")
                                "-o" ,outfile
                                "-lm")
                              (get-cflags)
                              args))))
      (if (verbose)
          (begin (display command) (newline)))
      (if (file-exists? src-tmp) (unlink src-tmp))
      (let ((out (open-output-file src-tmp)))
        (display src out)
        (close-output-port out))
      (system command)
      (unless (generate-debug)
        (unlink src-tmp))))

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
