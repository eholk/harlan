(library
  (harlan middle generate-kernel-calls)
  (export generate-kernel-calls)
  (import (rnrs) (elegant-weapons helpers)
    (except (harlan helpers) type-of))
  
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

(define-match Stmt
  ((apply-kernel ,k ,dims ,arg* ...)
   (let ((kernel (gensym k)))
     `(begin
        (let ,kernel cl::kernel
             (call
              (field (var cl::program g_prog)
                     createKernel)
              (str ,(symbol->string k))))
        (do (call (c-expr (((ptr region)) -> void) unmap_region)
                  (var (ptr region) g_region)))
        ,@(map (lambda (arg i)
                 `(do (call
                       (field (var cl::kernel ,kernel) setArg)
                       (int ,i)
                       ,arg)))
            arg* (iota (length arg*)))
        (do (call (field (var cl::queue g_queue) execute)
              (var cl::kernel ,kernel)
              ,(car dims) ;; global size
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