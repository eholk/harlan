(library
  (harlan middle lower-vectors)
  (export lower-vectors)
  (import (rnrs) (elegant-weapons helpers))

(define-match lower-vectors
  ((module ,[lower-decl -> fn*] ...)
   `(module . ,fn*)))

(define-match lower-decl
  ((fn ,name ,args ,t ,[lower-stmt -> stmt])
   `(fn ,name ,args ,t ,stmt))
  (,else else))

(define-match (lower-let finish)
  (() finish)
  (((,x (vector (vec ,t ,n) . ,e*))
    . ,[(lower-let finish) -> rest])
   `(let ((,x (make-vector ,t (int ,n))))
      ,(make-begin
         (let loop ((e* e*) (i 0))
           (if (null? e*)
               `(,rest)
               `((vector-set!
                   ,t (var (vec ,t ,n) ,x) (int ,i) ,(car e*))
                 . ,(loop (cdr e*) (+ 1 i))))))))
  
  (((,x (iota (int ,n))) . ,[(lower-let finish) -> rest])
   (let ((i (gensym 'i)))
     `(let ((,x (make-vector int (int ,n))))
        (begin
          (for (,i (int 0) (int ,n))
            (vector-set! int
              (var (vec int ,n) ,x) (var int ,i) (var int ,i)))
          ,rest))))
  
  (((,x (reduce ,t2 ,op (var ,tv ,v))) . ,[(lower-let finish) -> rest])
   (let ((i (gensym 'i)) (t t2))
     `(let ((,x (vector-ref ,t (var ,tv ,v) (int 0))))
        (begin
          (for (,i (int 1) (length (var ,tv ,v)))
            (set! (var ,t ,x)
              (,op (var ,t ,x)
                (vector-ref ,t (var ,tv ,v) (var int ,i)))))
          ,rest))))
  
  (((,x ,e) . ,[(lower-let finish) -> rest])
   `(let ((,x ,e)) ,rest)))


(define-match lower-stmt
  ((let ((,x ,[lower-expr -> e]) ...) ,[stmt])
   ((lower-let stmt) `((,x ,e) ...)))
  ((begin ,[stmt*] ...)
   (make-begin stmt*))
  ((kernel ,t ,dims ,b ,[stmt])
   `(kernel ,t ,dims ,b ,stmt))
  ((print ,[lower-expr -> expr])
   `(print ,expr))      
  ((assert ,[lower-expr -> expr])
   `(assert ,expr))
  ((set! ,x ,i) `(set! ,x ,i))
  ((if ,test ,[conseq])
   `(if ,test ,conseq))
  ((if ,test ,[conseq] ,[alt])
   `(if ,test ,conseq ,alt))
  ((vector-set! ,t ,[lower-expr -> e1] ,i ,[lower-expr -> e2])
   `(vector-set! ,t ,e1 ,i ,e2))
  ((while ,[lower-expr -> expr] ,[body])
   `(while ,expr ,body))
  ((for (,x ,[lower-expr -> start] ,[lower-expr -> end]) ,[body])
   `(for (,x ,start ,end) ,body))
  ((return) `(return))
  ((return ,[lower-expr -> expr])
   `(return ,expr))
  ((do ,[lower-expr -> expr]) `(do ,expr)))

(define-match lower-expr
  ((begin ,[lower-stmt -> stmt*] ... ,[expr])
   `(begin ,@stmt* ,expr))
  ((let ((,x ,[e]) ...) ,[expr])
   ((lower-let expr) `((,x ,e) ...)))
  (,else else))
)
