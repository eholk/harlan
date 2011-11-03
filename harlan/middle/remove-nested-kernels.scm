(library
  (harlan middle remove-nested-kernels)
  (export remove-nested-kernels)
  (import
    (rnrs)
    (elegant-weapons helpers)
    (elegant-weapons match))

;; This pass takes a nest of kernels and turns all but the innermost
;; one into for loops. This isn't the best way to do this, but it's
;; the easiest way to support nested kernels and will give us
;; something to build off of.

(define-match remove-nested-kernels
  ((module ,[Decl -> decl*] ...)
   `(module . ,decl*)))

(define-match Decl
  ((fn ,name ,args ,type ,[Stmt -> stmt* _*] ...)
   `(fn ,name ,args ,type . ,(apply append stmt*)))
  ((extern . ,rest)
   `(extern . ,rest)))

(define (any? ls)
  (if (null? ls)
      #f
      (or (car ls) (any? (cdr ls)))))

(define-match Stmt
  ((let ,x (vector ,t ,n)
        (kernel (vector ,t ,n)
          (((,x* ,t*) (,xs* ,ts*)) ...)
          ,[Stmt -> stmt* _*] ... ,e))
   (values
     (if (any? _*)
         (let ((i (gensym 'i)))
           `((let ,x (vector ,t ,n) (int ,n))
             (for (,i (int 0) (int ,n))
               ,@(map (lambda (x t xs ts)
                        `(let ,x ,t (vector-ref ,t ,xs (var int ,i))))
                   x* t* xs* ts*)
               ,@(apply append stmt*)
               (vector-set! ,t (var (vector ,t ,n) ,x) (var int ,i)
                 ,e))))
         `((let ,x (vector ,t ,n)
                (kernel (vector ,t ,n)
                  (((,x* ,t*) (,xs* ,ts*)) ...)
                  ,@(apply append stmt*) ,e))))
     #t))
  ((let ,x ,t ,e) (values `((let ,x ,t ,e)) #f))
  ((begin ,[stmt* has-kernel*] ...)
   (values `(,(make-begin (apply append stmt*))) (any? has-kernel*)))
  ((for (,i ,start ,end) ,[stmt* has-kernel*] ...)
   (values `((for (,i ,start ,end) . ,(apply append stmt*)))
     (any? has-kernel*)))
  ((while ,t ,[stmt* has-kernel*] ...)
   (values `((while ,t . ,(apply append stmt*))) (any? has-kernel*)))
  ((if ,test ,[conseq chas-kernel])
   (values `((if ,test ,@conseq)) chas-kernel))
  ((if ,test ,[conseq chas-kernel] ,[alt ahas-kernel])
   (values `((if ,test ,conseq ,alt)) (or chas-kernel ahas-kernel)))
  ((set! ,lhs ,rhs)
   (values `((set! ,lhs ,rhs)) #f))
  ((vector-set! ,t ,v ,i ,e)
   (values `((vector-set! ,t ,v ,i ,e)) #f))
  ((do . ,e) (values `((do . ,e)) #f))
  ((print ,e) (values `((print ,e)) #f))
  ((assert ,e) (values `((assert ,e)) #f))
  ((return ,e) (values `((return ,e)) #f)))

;;end library
)
