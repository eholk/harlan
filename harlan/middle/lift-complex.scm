(library
  (harlan middle lift-complex)
  (export lift-complex)
  (import (rnrs) (elegant-weapons helpers))
  
(define lift-expr->stmt
  (lambda (expr finish)
    (match expr
      ((int ,n) (guard (integer? n)) (finish `(int ,n)))
      ((u64 ,n) (guard (integer? n)) (finish `(u64 ,n)))
      ((float ,f) (finish `(float ,f)))
      ((str ,str) (guard (string? str)) (finish `(str ,str)))
      ((var ,t ,x) (finish `(var ,t ,x)))
      ((int->float ,e)
       (lift-expr->stmt e (lambda (e) (finish `(int->float ,e)))))
      ((begin ,[lift-stmt -> stmt*] ... ,e)
       (lift-expr->stmt e
         (lambda (e) (finish `(begin ,@stmt* ,e)))))
      ((let ((,x* ,e*) ...) ,expr)
       (let loop ((e* e*) (e*^ '()))
         (if (null? e*)
             (lift-expr->stmt
               expr
               (lambda (expr)
                 `(let (,@(map list x* (reverse e*^))) ,(finish expr))))
             (lift-expr->stmt
               (car e*)
               (lambda (e^) (loop (cdr e*) (cons e^ e*^)))))))
      ((if ,test ,conseq ,alt)
       (lift-expr->stmt
         test
         (lambda (t)
           (lift-expr->stmt
             conseq
             (lambda (c)
               (lift-expr->stmt
                 alt
                 (lambda (a) (finish `(if ,t ,c ,a)))))))))
      ((vector-ref ,t ,e1 ,e2)
       (if (symbol? e1)
           (error 'lift-expr->stmt "This form is not legal" e1))
       (lift-expr->stmt
         e1
         (lambda (e1^)
           (lift-expr->stmt
             e2 (lambda (e2^)
                  (finish `(vector-ref ,t ,e1^ ,e2^)))))))
      ((make-vector ,t ,e)
       (lift-expr->stmt e (lambda (e^) (finish `(make-vector ,t ,e^)))))
      ((kernel ,t (((,x* ,t*) (,e* ,ts*)) ...) ,body)
       (let ((finish
               (lambda (e*^)
                 (let ((v (gensym 'v)))
                   `(let ((,v
                            (kernel ,t (((,x* ,t*) (,e*^ ,ts*)) ...)
                              ,(lift-expr->stmt body (lambda (b) b)))))
                      ,(finish `(var ,t ,v)))))))
         (let loop ((e* e*) (e*^ '()))
           (if (null? e*)
               (finish (reverse e*^))
               (lift-expr->stmt
                 (car e*)
                 (lambda (e^)
                   (loop (cdr e*) (cons e^ e*^))))))))
      ((vector ,t . ,e*)
       (let ((finish (lambda (e*^)
                       (let ((v (gensym 'v)))
                         `(let ((,v (vector ,t . ,e*^)))
                            ,(finish `(var ,t ,v)))))))
         (let loop ((e* e*) (e*^ '()))
           (if (null? e*)
               (finish (reverse e*^))
               (lift-expr->stmt
                 (car e*)
                 (lambda (e^)
                   (loop (cdr e*) (cons e^ e*^))))))))
      ((make-vector ,c)
       (finish `(make-vector ,c)))
      ((iota (int ,c))
       (let ((v (gensym 'iota)))
         `(let ((,v (iota (int ,c))))
            ,(finish `(var (vec int ,c) ,v)))))
      ((reduce ,t ,op ,e)
       (lift-expr->stmt
         e
         (lambda (e^)
           (let ((v (gensym 'v)))
             `(let ((,v (reduce ,t ,op ,e^)))
                ,(finish `(var ,t ,v)))))))
      ((length ,e) 
       (lift-expr->stmt
         e (lambda (e^)
             (finish `(length ,e^)))))
      ((,op ,e1 ,e2) (guard (or (binop? op) (relop? op)))
       (lift-expr->stmt
         e1 (lambda (e1^)
              (lift-expr->stmt
                e2 (lambda (e2^)
                     (finish `(,op ,e1^ ,e2^)))))))
      ((call ,rator . ,rand*)
       (let loop ((e* (cons rator rand*)) (e*^ '()))
         (if (null? e*)
             (finish `(call . ,(reverse e*^)))
             (lift-expr->stmt
               (car e*)
               (lambda (e^)
                 (loop (cdr e*) (cons e^ e*^))))))))))

(define-match lift-stmt
  ((begin ,[lift-stmt -> stmt*] ...)
   (make-begin stmt*))
  ((print ,expr)
   (lift-expr->stmt expr (lambda (e^) `(print ,e^))))
  ((assert ,expr)
   (lift-expr->stmt expr (lambda (e^) `(assert ,e^))))
  ((set! ,x ,e)
   (lift-expr->stmt e (lambda (e^) `(set! ,x ,e^))))
  ((let ((,x* ,e*) ...) ,[stmt])
   (let loop ((e* e*) (e*^ '()))
     (if (null? e*)
         `(let (,@(map list x* (reverse e*^))) ,stmt)
         (lift-expr->stmt
           (car e*)
           (lambda (e^) (loop (cdr e*) (cons e^ e*^)))))))
  ((if ,test ,conseq)
   (lift-expr->stmt test (lambda (t) `(if ,t ,conseq))))
  ((if ,test ,conseq ,alt)
   (lift-expr->stmt test (lambda (t) `(if ,t ,conseq ,alt))))
  ((vector-set! ,t ,x ,e1 ,e2)
   (lift-expr->stmt
     x
     (lambda (x^)
       (lift-expr->stmt
         e1
         (lambda (e1^)
           (lift-expr->stmt e2
             (lambda (e2^) `(vector-set! ,t ,x^ ,e1^ ,e2^))))))))             
  ((kernel ,iters ,body)
   ;; TODO: For now just pass the kernel through... this
   ;; won't let us declare vectors inside kernels though.
   `(kernel ,iters ,body))
  ((return ,expr)
   (lift-expr->stmt expr (lambda (e^) `(return ,e^))))
  ((for (,x ,start ,end) ,[stmt])
   (lift-expr->stmt
     start
     (lambda (start)
       (lift-expr->stmt
         end
         (lambda (end)
           `(for (,x ,start ,end) ,stmt))))))
  ((do ,e)
   (lift-expr->stmt e (lambda (e) `(do ,e))))
  ((while ,expr ,stmt)
   (lift-expr->stmt expr (lambda (expr) `(while ,expr ,stmt)))))

(define-match lift-decl
  ((fn ,name ,args ,t ,[lift-stmt -> stmt])
   `(fn ,name ,args ,t ,stmt))
  ((extern ,name ,args -> ,rtype)
   `(extern ,name ,args -> ,rtype)))

(define-match lift-complex
  ((module ,[lift-decl -> fn*] ...)
   `(module . ,fn*)))

)
