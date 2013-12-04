(library
    (harlan middle remove-complex-kernel-args)
  (export remove-complex-kernel-args)
  (import
   (rnrs)
   (nanopass)
   (harlan middle languages))

  (define-pass remove-complex-kernel-args : M7.0 (m) -> M7.0 ()
    ;;
    ;;(Kernel
    ;; : Kernel (kernel) -> Kernel
    ;; 
    ;; ((kernel ,[t] (,[e*] ...) ,fv ,[e])
    ;;  
    ;;    )
    )
  )
