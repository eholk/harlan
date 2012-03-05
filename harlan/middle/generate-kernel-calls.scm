(library
  (harlan middle generate-kernel-calls)
  (export generate-kernel-calls)
  (import (rnrs) (except (elegant-weapons helpers) type-of))
  
(define-match generate-kernel-calls
  ((module ,[Decl -> decl*] ...)
   `(module . ,decl*)))

(define-match Decl
  ((fn ,name ,args ,type ,[Stmt -> stmt*] ...)
   `(fn ,name ,args ,type . ,stmt*))
  (,else else))

(define-match type-of
  ((deref ,[e]) e)
  ((vector-ref ,t ,v ,i) t)
  ((var ,t ,x) t)
  ((c-expr ,t ,e) t)
  ((call ,[f] ,_ ...)
   (match f
     ((,_ -> ,t) t)
     (,else (error 'type-of "Invalid function type" else)))))

(define (get-arg-length a)
  (match (type-of a)
    ((vec ,t ,n) n)
    (,else (error 'get-arg-length "Invalid kernel argument" a))))

(define-match Stmt
  ((apply-kernel ,k ,arg* ...)
   (let ((kernel (gensym k)))
     `(begin
        (let ,kernel cl::kernel
             (call
              (field (var cl::program g_prog)
                     createKernel)
              (str ,(symbol->string k))))
        (do (call (c-expr (((ptr region)) -> void) unmap_region)
                  (var (ptr region) g_region)))
        (do (call
             (field (var cl::kernel ,kernel) setArg)
             (int 0)
             (call (c-expr (((ptr region)) -> cl_mem) get_cl_buffer)
                   (var (ptr region) g_region))))
        ,@(map (lambda (arg i)
                 `(do (call
                       (field (var cl::kernel ,kernel) setArg)
                       (int ,(+ 1 i))
                       ,arg)))
            arg* (iota (length arg*)))
        (do (call (field (var cl::queue g_queue) execute)
              (var cl::kernel ,kernel)
              (int ,(get-arg-length (car arg*))) ;; global size
              (int 1)))
        (do (call (c-expr (((ptr region)) -> void) map_region)
                  (var (ptr region) g_region)))))) ;; local size
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