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
  ((return) `(return))
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

(define (unmap e t) 
  (match t
    ((vec (vec ,t^ ,inner) ,outer)
     (let ((i (gensym 'i)))
       ;; TODO: use length here once vectors are dynamic
       `((for (,i (int 0) (int ,outer))
              (begin
                ,@(unmap `(vector-ref (vec ,t^ ,inner) ,e (var int ,i))
                         `(vec ,t^ ,inner))))
         (do (call (c-expr (((ptr void)) -> void) unmap_buffer) ,e)))))
    ((vec ,t ,n)
     `((do (call (c-expr (((ptr void)) -> void) unmap_buffer) ,e))))))

(define (remap e t) 
  (match t
    ((vec (vec ,t^ ,inner) ,outer)
     (let ((i (gensym 'i)))
       ;; TODO: use length here once vectors are dynamic
       `((do (call (c-expr (((ptr void)) -> void) map_buffer) ,e))
         (for (,i (int 0) (int ,outer))
              (begin
                ,@(remap `(vector-ref (vec ,t^ ,inner) ,e (var int ,i))
                         `(vec ,t^ ,inner)))))))
    ((vec ,t ,n)
     `((do (call (c-expr (((ptr void)) -> void) map_buffer) ,e))))))

(define-match make-gpu-decl
  ((var ,t ,x)
   (match t
     ((vec ,t^ ,n)
      (values
       (unmap `(var ,t ,x) t)
       `(call (c-expr (((ptr void)) -> ,t) get_mem_object) (var ,t ,x))
       (remap `(var ,t ,x) t)))
     (,scalar
      (guard (symbol? scalar))
      (values '() `(var ,t ,x) '())))))

;; end library
)