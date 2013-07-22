(library
  (harlan middle generate-kernel-calls)
  (export generate-kernel-calls)
  (import
   (rnrs)
   (only (chezscheme) pretty-print)
   (nanopass)
   (except (elegant-weapons helpers) ident?)
   (harlan middle languages)
   (harlan helpers))
  
(define-match generate-kernel-calls^
  ((module ,[generate-decl -> decl*] ...)
   `(module . ,decl*)))

(define (generate-kernel-calls m)
  (begin
    ;;(pretty-print (language->s-expression M8))
    (parse-M8 m)
    (generate-kernel-calls^ m)))

(define-match generate-decl
  ((fn ,name ,args ,type ,[generate-stmt -> stmt])
   `(fn ,name ,args ,type ,stmt))
  (,else else))

(define (region? arg)
  (match arg
    ((var (ptr region) ,name) #t)
    (,else #f)))

;; This deserves a helper function now...
(define-match generate-stmt
  ((apply-kernel ,k ,dims ,arg* ...)
   (let ((kernel (gensym k))
         (region* (filter region? arg*))
         (dim-names (map (lambda (_) (gensym 'dim)) dims)))
     `(begin
        (let ,kernel cl::kernel
             (call
              (field (var cl::program g_prog)
                     createKernel)
              (str ,(symbol->string k))))
        ,@(map (lambda (n d)
                 `(let ,n int ,d))
               dim-names dims)
        ,@(map (lambda (region)
                 `(do (call
                        (c-expr (((ptr region)) -> void)
                          unmap_region)
                        ,region)))
            region*)
        ,@(map (lambda (arg i)
                 `(do (call
                       (field (var cl::kernel ,kernel) setArg)
                       (int ,i)
                       ,(match arg
                          ((var (ptr region) ,x)
                           `(call
                              (c-expr (((ptr region)) -> cl_mem) get_cl_buffer)
                              (var (ptr region) ,x)))
                          (,else else)))))
            arg* (iota (length arg*)))
        ,(if (null? (cdr dims))
             `(do (call (field (var cl::queue g_queue) execute)
                        (var cl::kernel ,kernel)
                        (var int ,(car dim-names)) ;; global size
                        ;;(int 1) ;; local size
                        ))
             (begin
               (assert (= (length dims) 2))
             `(do (call (field (var cl::queue g_queue) execute2d)
                        (var cl::kernel ,kernel)
                        (var int ,(car dim-names)) ;; global size
                        (var int ,(cadr dim-names))
                        (int 1)))))
        ;;,@(map (lambda (region)
        ;;         `(do (call
        ;;                (c-expr (((ptr region)) -> void)
        ;;                  map_region)
        ;;                ,region)))
        ;;    region*)
        ))) ;; local size
  ((begin ,[stmt*] ...)
   `(begin . ,stmt*))
  ((for (,i ,start ,end ,step) ,[stmt])
   `(for (,i ,start ,end ,step) ,stmt))
  ((while ,expr ,[stmt])
   `(while ,expr ,stmt))
  ((if ,t ,[conseq] ,[alt])
   `(if ,t ,conseq ,alt))
  ((if ,t ,[conseq])
   `(if ,t ,conseq))
  (,else else))

;; end library
)
