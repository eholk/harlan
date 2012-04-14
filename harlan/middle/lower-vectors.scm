(library
  (harlan middle lower-vectors)
  (export lower-vectors)
  (import (rnrs) (elegant-weapons helpers)
    (harlan helpers))

(define-match lower-vectors
  ((module ,[lower-decl -> fn*] ...)
   `(module . ,fn*)))

(define-match lower-decl
  ((fn ,name ,args ,t ,[lower-stmt -> stmt])
   `(fn ,name ,args ,t ,stmt))
  (,else else))

(define-match (lower-let finish)
  (() finish)
  (((,x ,xt (vector (vec ,t) . ,e*))
    . ,[(lower-let finish) -> rest])
   `(let ((,x ,xt (make-vector ,t (int ,(length e*)))))
      ,(make-begin
         (let loop ((e* e*) (i 0))
           (if (null? e*)
               `(,rest)
               `((vector-set!
                   ,t (var (vec ,t) ,x) (int ,i) ,(car e*))
                 . ,(loop (cdr e*) (+ 1 i))))))))
  
  (((,x ,xt (iota ,[lower-expr -> e])) . ,[(lower-let finish) -> rest])
   (let ((i (gensym 'i)) (vlen (gensym 'vlen)))
     `(let ((,vlen int ,e))
        (let ((,x ,xt (make-vector int (var int ,vlen))))
          (begin
            (for (,i (int 0) (var int ,vlen))
              (vector-set! int (var ,xt ,x) (var int ,i) (var int ,i)))
            ,rest)))))
  
  (((,x ,xt (reduce ,t ,op ,e)) . ,[(lower-let finish) -> rest])
   (let ((i (gensym 'i))
         (v (gensym 'v)))
     `(let ((,v ,t ,e))
        (let ((,x ,xt (vector-ref ,t (var ,t ,v) (int 0))))
          (begin
            (for (,i (int 1) (length (var ,t ,v)))
              (set! (var ,xt ,x)
                (,op (var ,xt ,x)
                  (vector-ref ,t (var ,t ,v) (var int ,i)))))
            ,rest)))))
  
  (((,x ,xt ,e) . ,[(lower-let finish) -> rest])
   `(let ((,x ,xt ,e)) ,rest)))


(define-match lower-stmt
  ((let ((,x ,t ,[lower-expr -> e]) ...) ,[stmt])
   ((lower-let stmt) `((,x ,t ,e) ...)))
  ((begin ,[stmt*] ...)
   (make-begin stmt*))
  ((kernel ,t ,dims ,b ,[stmt])
   `(kernel ,t ,dims ,b ,stmt))
  ((print ,[lower-expr -> expr] ...)
   `(print . ,expr))      
  ((assert ,[lower-expr -> expr])
   `(assert ,expr))
  ((set! ,x ,i) `(set! ,x ,i))
  ((if ,[lower-expr -> test] ,[conseq])
   `(if ,test ,conseq))
  ((if ,[lower-expr -> test] ,[conseq] ,[alt])
   `(if ,test ,conseq ,alt))
  ((vector-set! ,t
     ,[lower-expr -> e1]
     ,[lower-expr -> i]
     ,[lower-expr -> e2])
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
  ((let ((,x ,t ,[e]) ...) ,[expr])
   ((lower-let expr) `((,x ,t ,e) ...)))
  (,else else))
)
