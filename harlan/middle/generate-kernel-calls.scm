(library
  (harlan middle generate-kernel-calls)
  (export generate-kernel-calls)
  (import (rnrs) (elegant-weapons helpers))
  
(define-match generate-kernel-calls
  ((module ,[Decl -> decl*] ...)
   `(module . ,decl*)))

(define-match Decl
  ((fn ,name ,args ,type ,[Stmt -> stmt*] ...)
   `(fn ,name ,args ,type . ,stmt*))
  (,else else))

(define (get-arg-length a)
  (match (type-of a)
    ((vector ,t ,n) n)
    (,else (error 'get-arg-length "Invalid kernel argument" a))))

(define-match Stmt
  ((apply-kernel ,k ,arg* ...)
   (let ((kernel (gensym k)))
     `(begin
        (let ,kernel cl::kernel
             (call cl::kernel
               (field (var cl::program g_prog)
                 createKernel)
               (str ,(symbol->string k))))
        ,@(map (lambda (arg i)
                 `(do (call void
                        (field (var cl::kernel ,kernel) setArg)
                        (int ,i)
                        ,arg)))
            arg* (iota (length arg*)))
        (do (call void (field (var cl::queue g_queue) execute)
              (var cl::kernel ,kernel)
              (int ,(get-arg-length (car arg*))) ;; global size
              (int 1)))))) ;; local size
  ((begin ,[stmt*] ...)
   `(begin . ,stmt*))
  ((for (,i ,start ,end) ,[stmt*] ...)
   `(for (,i ,start ,end) . ,stmt*))
  ((while ,expr ,[stmt*] ...)
   `(while ,expr . ,stmt*))
  (,else else))

(define-match unpack-arg
  ((var ,t ,x^) x^)
  (,x^ (guard (symbol? x^)) x^))

;; end library
)