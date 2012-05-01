(library
  (harlan middle lower-vectors)
  (export lower-vectors
    lower-stmt
    lower-decl
    lower-expr)
  (import
    (rnrs)
    (elegant-weapons helpers)
    (elegant-weapons sets))

  

(define-match lower-vectors
  ((module ,[lower-decl -> decl*] ...)
   `(module . ,decl*)))

(define-match lower-decl
  ((fn ,name ,info ... ,[lower-stmt -> s])
   `(fn ,name ,info ... ,s))
  ((extern ,name ,args -> ,t)
   `(extern ,name ,args -> ,t)))

(define-match lower-stmt
  ((error ,x) (values `(error ,x) '()))
  ((let ,b ,[s])
   (lower-lifted-expr b s))
  ((begin ,[stmt* ] ...)
   (make-begin stmt*))
  ((if ,t ,[c])
   `(if ,t ,c))
  ((if ,t ,[c] ,[a])
   `(if ,t ,c ,a))
  ((while ,e ,[s])
   `(while ,e ,s))
  ((for (,i ,start ,end ,step) ,[stmt])
   `(for (,i ,start ,end ,step) ,stmt))
  ((set! ,lhs ,rhs)
   `(set! ,lhs ,rhs))
  ((return)
   `(return))
  ((return ,e)
   `(return ,e))
  ((assert ,e)
   `(assert ,e))
  ((print ,e* ...)
   `(print . ,e*))
  ((kernel (,dims ...) ,fv* ,[stmt])
   `(kernel ,dims ,fv* ,stmt))
  ((do ,e) `(do ,e)))

(define (lower-lifted-vector b s)
  (match b
    (() s)
    (((,x (vec ,t) (vector ,t ,e*)) . ,[rest])
     `(let ((,x (vec ,t)
                (make-vector ,t (int ,(length e*)))))
        ,(make-begin
          (let loop ((e* e*) (i 0))
            (if (null? e*)
                `(,rest)
                `((set! (vector-ref ,t
                                    (var (vec ,t) ,x)
                                    (int ,i))
                        ,(car e*))
                  . ,(loop (cdr e*) (+ 1 i)))))))))
  (((,x ,t ,e) . ,[rest])
   `(let ((,x ,t ,e)) . ,rest)))

)