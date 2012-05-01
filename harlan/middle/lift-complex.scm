(library
  (harlan middle lift-complex)
  (export lift-complex
          lift-expr)
  (import (rnrs) (elegant-weapons helpers)
    (harlan helpers))
  
(define lift-expr
  (lambda (expr finish)
    (match expr
      ((void) (finish `(void)))
      ((,t ,n) (guard (scalar-type? t)) (finish `(,t ,n)))
      ((var ,t ,x) (finish `(var ,t ,x)))
      ((int->float ,e)
       (lift-expr e (lambda (e) (finish `(int->float ,e)))))
      ((begin ,[lift-stmt -> stmt*] ... ,e)
       (lift-expr e
         (lambda (e) `(begin ,@stmt* ,(finish e)))))
      ((let () ,expr)
       (lift-expr expr finish))
      ((let ((,x ,t ,e) . ,rest) ,expr)
       (Expr e
         (lambda (e)
           `(let ((,x ,t ,e))
              ,(lift-expr `(let ,rest ,expr) finish)))))
      ((if ,test ,conseq ,alt)
       (lift-expr
         test
         (lambda (t)
           (lift-expr
             conseq
             (lambda (c)
               (lift-expr
                 alt
                 (lambda (a) (finish `(if ,t ,c ,a)))))))))
      ((vector-ref ,t ,e1 ,e2)
       (lift-expr
         e1
         (lambda (e1^)
           (lift-expr
             e2 (lambda (e2^)
                  (finish `(vector-ref ,t ,e1^ ,e2^)))))))
      ((make-vector ,t ,e)
       (lift-expr e (lambda (e^) (finish `(make-vector ,t ,e^)))))
      ;; There are TWO kernel lines, don't forget.
      ((kernel ,t ,dims (((,x* ,t*) (,e* ,ts*) ,dim*) ...) ,body)
       (let ((finish
               (lambda (dims^ e*^)
                 (let ((v (gensym 'v)))
                   `(let ((,v ,t
                            (kernel ,t ,dims^
                               (((,x* ,t*) (,e*^ ,ts*) ,dim*) ...)
                              ,(lift-expr body (lambda (b) b)))))
                      ,(finish `(var ,t ,v)))))))
         (let loop ((e* e*) (e*^ '()) (dims dims) (dims^ `()))
           (cond
             ((and (null? dims) (null? e*))
              (finish (reverse dims^) (reverse e*^)))
             ((and (null? dims) (not (null? e*)))
              (lift-expr
               (car e*)
               (lambda (e^)
                 (loop (cdr e*) (cons e^ e*^) dims dims^))))
             (else
              (lift-expr
               (car dims)
               (lambda (d)
                 (loop e* e*^ (cdr dims) (cons d dims^)))))))))
      ((make-vector ,t ,c)
       (finish `(make-vector ,t ,c)))
      ((vector ,t ,e* ...)
       (let loop ((e* e*) (e^* `()))
         (cond
          ((null? e*)
           (finish `(vector ,t ,(reverse e^*))))
          (else
           (lift-expr
            (car e*)
            (lambda (e^)
              (loop (cdr e*) (cons e^ e^*))))))))
      ((length ,e) 
       (lift-expr
         e (lambda (e^)
             (finish `(length ,e^)))))
      ((c-expr ,t ,v) (finish `(c-expr ,t ,v)))
      ((,op ,e1 ,e2) (guard (or (binop? op) (relop? op)))
       (lift-expr
         e1 (lambda (e1^)
              (lift-expr
                e2 (lambda (e2^)
                     (finish `(,op ,e1^ ,e2^)))))))
      ((call ,rator . ,rand*)
       (let loop ((e* (cons rator rand*)) (e*^ '()))
         (if (null? e*)
             (finish `(call . ,(reverse e*^)))
             (lift-expr
               (car e*)
               (lambda (e^)
                 (loop (cdr e*) (cons e^ e*^)))))))
      (,else (error 'lift-expr "unmatched datum" else)))))

(define Expr
  (lambda (expr finish)
    (match expr
      ;; There are TWO kernel lines, don't forget.
      ((kernel ,t ,dims (((,x* ,t*) (,e* ,ts*) ,dim*) ...) ,body)
       (let ((finish
              (lambda (dims^ e*^)
                (finish `(kernel ,t ,dims^
                                 (((,x* ,t*) (,e*^ ,ts*) ,dim*) ...)
                                 ,(lift-expr body (lambda (b) b)))))))
         (let loop ((e* e*) (e*^ '()) (dims dims) (dims^ `()))
           (cond
             ((and (null? dims) (null? e*))
              (finish (reverse dims^) (reverse e*^)))
             ((and (null? dims) (not (null? e*)))
              (lift-expr
               (car e*)
               (lambda (e^)
                 (loop (cdr e*) (cons e^ e*^) dims dims^))))
             (else
              (lift-expr
               (car dims)
               (lambda (d)
                 (loop e* e*^ (cdr dims) (cons d dims^)))))))))
      ((make-vector ,t ,e)
       (lift-expr e
          (lambda (e)
            (finish `(make-vector ,t ,e)))))
      ((vector ,t ,e* ...)
       (let loop ((e* e*) (e^* `()))
         (cond
          ((null? e*)
           (finish `(vector ,t ,(reverse e^*))))
          (else
           (lift-expr
            (car e*)
            (lambda (e^)
              (loop (cdr e*) (cons e^ e^*))))))))
      (,else (lift-expr else finish)))))

(define-match lift-stmt
  ((begin ,[lift-stmt -> stmt*] ...)
   (make-begin stmt*))
  ((error ,x) `(error ,x))
  ((print ,expr)
   (lift-expr expr (lambda (e^) `(print ,e^))))
  ((print ,expr ,op)
   (lift-expr expr
     (lambda (e^)
       (lift-expr op
         (lambda (op^)
           `(print ,e^ ,op^))))))
  ((assert ,expr)
   (lift-expr expr (lambda (e^) `(assert ,e^))))
  ((set! ,x ,e)
   (lift-expr
    x
    (lambda (x)
      (lift-expr e (lambda (e^) `(set! ,x ,e^))))))
  ((let () ,[stmt]) stmt)
  ((let ((,x ,t ,e) . ,rest) ,stmt)
   (Expr e
     (lambda (e)
       `(let ((,x ,t ,e))
          ,(lift-stmt `(let ,rest ,stmt))))))
  ((if ,test ,[conseq])
   (lift-expr test (lambda (t) `(if ,t ,conseq))))
  ((if ,test ,[conseq] ,[alt])
   (lift-expr test (lambda (t) `(if ,t ,conseq ,alt))))
  ((return) `(return))
  ((return ,expr)
   (lift-expr expr (lambda (e^) `(return ,e^))))
  ((for (,x ,start ,end ,step) ,[stmt])
   (lift-expr
     start
     (lambda (start)
       (lift-expr
         end
         (lambda (end)
           (lift-expr step
             (lambda (step)
               `(for (,x ,start ,end ,step) ,stmt))))))))
  ((do ,e)
   (lift-expr e (lambda (e) `(do ,e))))
  ((while ,expr ,[stmt])
   (lift-expr expr (lambda (expr) `(while ,expr ,stmt)))))

(define-match lift-decl
  ((fn ,name ,args ,t ,[lift-stmt -> stmt])
   `(fn ,name ,args ,t ,stmt))
  ((extern ,name ,args -> ,rtype)
   `(extern ,name ,args -> ,rtype)))

(define-match lift-complex
  ((module ,[lift-decl -> fn*] ...)
   `(module . ,fn*)))

)
