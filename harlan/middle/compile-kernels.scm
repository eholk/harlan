(library
  (harlan middle compile-kernels)
  (export
    compile-kernels)
  (import
    (only (chezscheme) format)
    (rnrs)
    (util match)
    (util verify-grammar)
    (util helpers)
    (harlan back print-c))
  
(define format-kernel-arg
  (lambda (arg)
    (format-arg arg)))

(define compile-kernel
  (lambda (kernel)
    (match kernel
      ((kernel ,name (,[format-kernel-arg -> args*] ...)
         . ,stmts)
       (string-append
         "__kernel void " (format-ident name)
         "(" (join ", " args*) ") "
         (format-block `(block . ,stmts))))
      (,else
        (error 'compile-kernel (format "bad kernel expression: ~s" kernel))))))

(define (unpack-type x)
  (match x
    ((var (vector ,t ,n) ,x^) t)
    (,else (error 'unpack-type "invalid kernel argument" else))))

(define compile-kernels
  (lambda (mod)
    (map (lambda (decl)
           (match decl
             ((gpu-module ,[compile-kernel -> kernel*] ...)
              `(global cl::program g_prog
                 ((field g_ctx createProgramFromSource)
                  ,(join "\n" kernel*))))
             ((func ,t ,x ,args
                    ,stmt* ...)
              `(func ,t ,x ,args ,stmt* ...))
             (,else else)))
      mod))))
