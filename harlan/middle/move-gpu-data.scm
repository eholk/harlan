(library
  (harlan middle move-gpu-data)
  (export move-gpu-data)
  (import (rnrs) (elegant-weapons helpers))
  
(define-match move-gpu-data
  ((module ,[Decl -> decl*] ...)
   `(module . ,decl*)))

(define-match Decl
  ((gpu-module . ,kernel*)
   `(gpu-module . ,kernel*))
  ((extern ,name ,args -> ,type)
   `(extern ,name ,args -> ,type))
  ((fn ,name ,args ,type ,[Stmt -> stmt])
   `(fn ,name ,args ,type ,stmt)))

(define-match Stmt
  ((do ,e)
   `(do ,e))
  ((let ,x ,t ,expr)
   `(let ,x ,t ,expr))
  ((begin ,[stmt*] ...)
   `(begin . ,stmt*))
  ((for (,x ,start ,end) ,[stmt])
   `(for (,x ,start ,end) ,stmt))
  ((if ,test ,[conseq])
   `(if ,test ,conseq))
  ((if ,test ,[conseq] ,[alt])
   `(if ,test ,conseq ,alt))
  ((while ,test ,[stmt])
   `(while ,test ,stmt))
  ((set! ,x ,v)
   `(set! ,x ,v))
  ((apply-kernel ,k . ,expr*)
   (let-values (((prologue args epilogue)
                 (make-gpu-decls expr*)))
     `(begin
        ,@prologue
        (apply-kernel ,k ,@args)
        ,@epilogue)))
  ((print ,expr)
   `(print ,expr))
  ((assert ,expr)
   `(assert ,expr))
  ((return ,expr)
   `(return ,expr)))

(define-match make-gpu-decls
  (() (values '() '() '()))
  ((,[make-gpu-decl -> prologue arg epilogue] .
    ,[prologue* arg* epilogue*])
   (values
     (append prologue prologue*)
     (cons arg arg*)
     (append epilogue epilogue*))))

(define-match make-gpu-decl
  ((var ,t ,x)
   (match t
     ((vec ,t^ ,n)
      (let ((gpu-var (gensym 'gpu))
            (gpu-ptr (gensym 'ptr)))
        (values
          `((let-gpu ,gpu-var ,t)
            (map-gpu ((,gpu-ptr (var ,t ,gpu-var)))
              (set! (var ,t ,gpu-ptr) (var ,t ,x))))
          `(var ,t ,gpu-var)
          `((map-gpu ((,gpu-ptr (var ,t ,gpu-var)))
              (set! (var ,t ,x) (var ,t ,gpu-ptr)))))))
     (,scalar
       (guard (symbol? scalar))
       (values '() `(var ,t ,x) '())))))

;; end library
)