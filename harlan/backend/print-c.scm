(library
  (harlan backend print-c)
  (export
    format-as-harlan
    define-with-harlan-fns
    harlan-print-c
    harlan-format-c
    harlan-format-decl
    harlan-format-stmt
    harlan-format-expr
    harlan-format-ident
    harlan-format-type
    harlan-format-arg
    harlan-format-binop
    harlan-format-relop)
  (import
    (rnrs)
    (harlan compile-opts)
    (elegant-weapons print-c)
    (elegant-weapons insert-prototypes)
    (elegant-weapons match)
    (elegant-weapons compat)
    (elegant-weapons helpers))

  (define-match compile-kernel
    ((kernel
       ,[format-ident -> name]
       (,[format-arg -> arg*] ...)
       ,[format-stmt -> stmt])
     (string-append
       "__kernel void " name "(" (join ", " arg*) ") " stmt))
    (,else (harlan-format-decl else)))

  (define (read-rt-include name)
    (let* ((port (open-file-input-port (string-append (harlan-runtime-path)
                                                      "/"
                                                      name)
                                       (file-options no-create)
                                       (buffer-mode line)
                                       (native-transcoder)))
           (data (get-string-all port)))
      (close-port port)
      data))
  
  (define (build-kernel-programs kernel*)
    `(global cl::program g_prog
       (call
         (field (var g_ctx) createAndBuildProgramFromSource)
         (str
           ,(string-append
             (read-rt-include "gpu_common.h")
             (read-rt-include "gpu_only.h")
             (join "\n" (map compile-kernel (insert-prototypes kernel*))))))))
  
  (define (harlan-decl decl ft)
    (match decl
      ((gpu-module ,kernel* ...)
       (ft (build-kernel-programs kernel*)))
      (,else (ft else))))
  
  (define-syntax format-as-harlan
    (syntax-rules ()
      ((_ expr ...)
       (call-with-fns ((decl-fns harlan-decl))
         (begin expr ...)))))

  (define-syntax define-with-harlan-fns
    (syntax-rules ()
      ((_ (name base-fn) ...)
       (begin
         (define (name x)
           (format-as-harlan
             (base-fn x)))
         ...))))

  (define-with-harlan-fns
    (harlan-print-c      print-c)
    (harlan-format-c     format-c)
    (harlan-format-decl  format-decl)
    (harlan-format-stmt  format-stmt)
    (harlan-format-expr  format-expr)
    (harlan-format-ident format-ident)
    (harlan-format-type  format-type)
    (harlan-format-arg   format-arg)
    (harlan-format-binop format-binop)
    (harlan-format-relop format-relop))

)

