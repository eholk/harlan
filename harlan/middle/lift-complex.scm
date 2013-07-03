(library
  (harlan middle lift-complex)
  (export lift-complex
          lift-expr)
  (import
   (rnrs)
   (except (elegant-weapons helpers) ident?)
   (harlan helpers))
  
(define lift-expr
  (lambda (expr finish)
    (match expr
      ((void) (finish `(void)))
      ((,t ,n) (guard (scalar-type? t)) (finish `(,t ,n)))
      ((var ,t ,x) (finish `(var ,t ,x)))
      ((int->float ,e)
       (lift-expr e (lambda (e) (finish `(int->float ,e)))))
      ((not ,e)
       (lift-expr e (lambda (e) (finish `(not ,e)))))
      ((begin ,[lift-stmt -> stmt*] ... ,e)
       (lift-expr e (lambda (e) `(begin ,@stmt* ,(finish e)))))
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
           (let ((if-res (gensym 'if_res))
                 (type (type-of conseq)))
             (let ((c (lift-expr conseq (lambda (c) `(set! (var ,type ,if-res) ,c))))
                   (a (lift-expr alt (lambda (a) `(set! (var ,type ,if-res) ,a)))))
               `(let ((,if-res ,type))
                  (begin
                    (if ,t ,c ,a)
                    ,(finish `(var ,type ,if-res)))))))))
      ((vector-ref ,t ,e1 ,e2)
       (lift-expr
         e1
         (lambda (e1^)
           (lift-expr
             e2 (lambda (e2^)
                  (finish `(vector-ref ,t ,e1^ ,e2^)))))))
      ((make-vector ,t ,r ,e)
       (lift-expr e (lambda (e^) (finish `(make-vector ,t ,r ,e^)))))
      ((vector ,t ,r . ,e*)
       (lift-expr*
        e*
        (lambda (e*)
          (let ((v (gensym 'v)))
            `(let ((,v ,t (vector ,t ,r . ,e*)))
               ,(finish `(var ,t ,v)))))))
      ((length ,e) 
       (lift-expr
         e (lambda (e^)
             (finish `(length ,e^)))))
      ((box ,r ,t ,e)
       (let ((box (gensym 'box)))
         (lift-expr
          e
          (lambda (e)
            `(let ((,box region_ptr (box ,r ,t ,e)))
               ,(finish `(var region_ptr ,box)))))))
      ((unbox ,t ,r ,e) (lift-expr e (lambda (e) (finish `(unbox ,t ,r ,e)))))
      ((field ,e ,x)
       (lift-expr e (lambda (e) (finish `(field ,e ,x)))))
      ((empty-struct) (finish '(empty-struct)))
      ((c-expr ,t ,v) (finish `(c-expr ,t ,v)))
      ((,op ,e1 ,e2) (guard (or (binop? op) (relop? op)))
       (lift-expr
         e1 (lambda (e1^)
              (lift-expr
                e2 (lambda (e2^)
                     (finish `(,op ,e1^ ,e2^)))))))
      ((call ,rator . ,rand*)
       (lift-expr
        rator
        (lambda (rator)
          (lift-expr*
           rand*
           (lambda (rand*)
             (finish `(call ,rator . ,rand*)))))))
      (,else (error 'lift-expr "unmatched datum" else)))))

(define Expr
  (lambda (expr finish)
    (match expr
      ((make-vector ,t ,e)
       (lift-expr e
          (lambda (e)
            (finish `(make-vector ,t ,e)))))
      ((vector ,t ,r . ,e*)
       (lift-expr* e* (lambda (e*) (finish `(vector ,t ,r . ,e*)))))
      (,else (lift-expr else finish)))))

(define (lift-expr* e* finish)
  (let loop ((e* e*) (e^* `()))
    (cond
     ((null? e*) (finish (reverse e^*)))
     (else
      (lift-expr
       (car e*)
       (lambda (e^) (loop (cdr e*) (cons e^ e^*))))))))

(define-match lift-stmt
  ((kernel ,t ,dims (((,x* ,t*) (,e* ,ts*) ,dim*) ...) ,[body])
   (lift-expr*
    dims
    (lambda (dims)
      (lift-expr*
       e*
       (lambda (e*^)
         `(kernel ,t ,dims
                  (((,x* ,t*) (,e*^ ,ts*) ,dim*) ...)
                  ,body))))))
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
  ((let-region (,r ...) ,[body]) `(let-region (,r ...) ,body))
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
  ((typedef ,name ,t) `(typedef ,name ,t))
  ((extern ,name ,args -> ,rtype)
   `(extern ,name ,args -> ,rtype)))

(define-match lift-complex
  ((module ,[lift-decl -> fn*] ...)
   `(module . ,fn*)))

(define-match type-of
  ((,t ,v) (guard (scalar-type? t)) t)
  ((var ,t ,x) t)
  ((int->float ,t) `float)
  ((length ,t) `int)
  ((addressof ,[t]) `(ptr ,t))
  ((deref ,[t]) (cadr t))
  ((if ,t ,[c] ,a) c)
  ((call (var (,argt -> ,rt) ,fn) . ,arg*) rt)
  ((c-expr ,t ,v) t)
  ((vector-ref ,t ,v ,i) t)
  ((,op ,[lhs] ,rhs)
   (guard (binop? op))
   lhs)
  ((let ,b ,[e]) e)
  ((begin ,s* ... ,[e]) e)
  ((call ,[e] ,e* ...)
   (match e
     ((fn ,args -> ,t) t)))
  ((,op ,lhs ,rhs)
   (guard (relop? op))
   rhs))

)
