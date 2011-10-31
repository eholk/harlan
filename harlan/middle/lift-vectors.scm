(library
  (harlan middle lift-vectors)
  (export
    lift-vectors
    lift-expr->stmt)
  (import
    (only (chezscheme) format)
    (rnrs)
    (util match)
    (util verify-grammar)
    (util helpers)
    (harlan front parser))
  
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
      ((vector-ref ,t ,e1 ,e2)
       (lift-expr->stmt
         (if (symbol? e1)
             (error 'lift-expr->stmt
                    "This form is not legal"
                    e1)
             e1)
         (lambda (e1^)
           (lift-expr->stmt
             e2 (lambda (e2^)
                  (finish `(vector-ref ,t ,e1^ ,e2^)))))))
      ((make-vector ,t ,e)
       (lift-expr->stmt e (lambda (e^) (finish `(make-vector ,t ,e^)))))
      ((kernel ,t (((,x* ,t*) (,e* ,ts*)) ...) ,body* ... ,body)
       (let ((finish
              (lambda (e*^)
                (let ((v (gensym 'v)))
                  (cons `(let ,v ,t
                           (kernel ,t ,(map (lambda (x t xs ts)
                                              `((,x ,t) (,xs ,ts)))
                                            x* t* e*^ ts*)
                                   ,@(lift-stmt* body*)
                                   ,@(lift-expr->stmt
                                      body (lambda (body^) `(,body^)))))
                        (finish `(var ,t ,v)))))))
         (let loop ((e* e*) (e*^ '()))
           (if (null? e*)
               (finish (reverse e*^))
               (lift-expr->stmt
                (car e*)
                (lambda (e^)
                  (loop (cdr e*) (cons e^ e*^))))))))
      ((vector ,t ,e* ...)
       (let ((finish (lambda (e*^)
                       (let ((v (gensym 'v)))
                         (cons `(let ,v ,t (vector . ,e*^))
                               (finish `(var ,t ,v)))))))
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
         (cons `(let ,v (vector int ,c) (iota (int ,c)))
               (finish `(var (vector int ,c) ,v)))))
      ((reduce ,t ,op ,e)
       (lift-expr->stmt
         e
         (lambda (e^)
           (let ((v (gensym 'v)))
             (cons `(let ,v ,t (reduce ,t ,op ,e^))
               (finish `(var ,t ,v)))))))
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
      ((call ,t ,rator ,rand* ...)
       (guard (symbol? rator))
       (let loop ((e* rand*) (e*^ '()))
         (if (null? e*)
             (finish `(call ,t ,rator . ,(reverse e*^)))
             (lift-expr->stmt
               (car e*)
               (lambda (e^)
                 (loop (cdr e*) (cons e^ e*^)))))))
      (,else (error 'lift-expr->stmt "unknown expression" else)))))

(define lift-stmt*
  (lambda (stmt*)
    (match stmt*
      (() '())
      (((print ,expr) . ,[rest])
       (lift-expr->stmt expr (lambda (e^)
                               (cons `(print ,e^)
                                 rest))))
      (((print ,e1 ,e2) . ,[rest])
       (lift-expr->stmt
         e1 (lambda (e1^)
              (lift-expr->stmt
                e2 (lambda (e2^)
                     (cons `(print ,e1^ ,e2^)
                       rest))))))
      (((assert ,expr) . ,[rest])
       (lift-expr->stmt expr (lambda (e^)
                               (cons `(assert ,e^)
                                 rest))))
      (((set! ,x ,e) . ,[rest])
       ;; TODO: should x be any expression, or just a variable?
       (lift-expr->stmt e (lambda (e^)
                            (cons `(set! ,x ,e^)
                              rest))))
      (((vector-set! ,t ,x ,e1 ,e2) . ,[rest])
       ;; TODO: should x be any expression, or just a variable?
       ;; WEB: any expression
       (lift-expr->stmt
         x
         (lambda (x^)
           (lift-expr->stmt
             e1
             (lambda (e1^)
               (lift-expr->stmt e2
                 (lambda (e2^)
                   (cons `(vector-set! ,t ,x^ ,e1^ ,e2^)
                     rest))))))))             
      (((kernel ,iters ,body* ...) . ,[rest])
       ;; TODO: For now just pass the kernel through... this
       ;; won't let us declare vectors inside kernels though.
       (cons `(kernel ,iters ,body* ...) rest))
      (((let ,x ,t ,e) . ,[rest])
       (lift-expr->stmt e (lambda (e^)
                            (cons `(let ,x ,t ,e^)
                              rest))))
      (((return ,expr) . ,[rest])
       (lift-expr->stmt expr
         (lambda (e^)
           (cons `(return ,e^) rest))))
      (((for (,x ,start ,end) ,stmt* ...) . ,[rest])
       (lift-expr->stmt
         start
         (lambda (start)
           (lift-expr->stmt
             end
             (lambda (end)
               (cons `(for (,x ,start ,end) . ,(lift-stmt* stmt*))
                 rest))))))
      (((do ,e) . ,[rest])
       (lift-expr->stmt e (lambda (e) (cons `(do ,e) rest))))
      (((while ,expr ,stmt* ...) . ,[rest])
       (lift-expr->stmt
         expr
         (lambda (expr)
           (cons `(while ,expr . ,(lift-stmt* stmt*))
             rest))))
      (,else (error 'lift-stmt* "unknown statement" else)))))

(define (lift-decl fn)
  (match fn
    ((fn ,name ,args ,t . ,[lift-stmt* -> stmt*])
     `(fn ,name ,args ,t . ,stmt*))
    ((extern ,name ,args -> ,rtype)
     `(extern ,name ,args -> ,rtype))
    (,else (error 'lift-decl "bad function" else))))

(define lift-vectors
  (lambda (mod)
    (match mod
      ((module ,[lift-decl -> fn*] ...)
       `(module . ,fn*))
      (,else (error 'lift-vectors "malformed module" else)))))

)
