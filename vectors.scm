(library
 (vectors)
 (export lift-vectors lower-vectors uglify-vectors lift-expr->stmt)
 (import (only (chezscheme) format)
         (rnrs)
         (match)
         (print-c))

 (define gensym
   (let ((c 0))
     (lambda (x)
       (set! c (+ 1 c))
       (string->symbol
        (string-append (symbol->string x) "_" (number->string c))))))
        
 ;; Vector simplification code. Weirdly, this runs before
 ;; typechecking.

 
 (define lift-expr->stmt
   (lambda (expr finish)
     (match expr
       (,n (guard (number? n)) (finish n))
       ((var ,x) (finish `(var ,x)))
       ((time)
        (finish '(time)))
       ((vector-ref ,e1 ,e2)
        (lift-expr->stmt
         (if (symbol? e1) `(var ,e1) e1)
         (lambda (e1^)
              (lift-expr->stmt
               e2 (lambda (e2^)
                    (finish `(vector-ref ,e1^ ,e2^)))))))
       ((kernel ((,x* ,e*) ...) ,body)
        (let ((finish
               (lambda (e*^)
                 (let ((v (gensym 'v)))
                   (cons `(let ,v
                            (kernel ,(map list x* e*^)
                                    ,@(lift-expr->stmt
                                       body (lambda (body^) `(,body^)))))
                                (finish `(var ,v)))))))
          (let loop ((e* e*) (e*^ '()))
            (if (null? e*)
                (finish (reverse e*^))
                (lift-expr->stmt
                 (car e*)
                 (lambda (e^)
                   (loop (cdr e*) (cons e^ e*^))))))))
       ((vector ,e* ...)
        (let ((finish (lambda (e*^)
                        (let ((v (gensym 'v)))
                          (cons `(let ,v (vector . ,e*^))
                                (finish `(var ,v)))))))
          (let loop ((e* e*) (e*^ '()))
            (if (null? e*)
                (finish (reverse e*^))
                (lift-expr->stmt
                 (car e*)
                 (lambda (e^)
                   (loop (cdr e*) (cons e^ e*^))))))))
       ((make-vector ,e)
        (lift-expr->stmt
         e
         (lambda (e^)
           (let ((v (gensym 'v)))
             (cons `(let ,v ,e^)
                   (finish `(make-vector (var ,v))))))))
       ((iota ,e)
	(finish `(iota ,e)))
       ((reduce ,op ,e)
        (lift-expr->stmt
         e
         (lambda (e^)
           (let ((v (gensym 'v)))
             (cons `(let ,v (reduce ,op ,e^))
                   (finish `(var ,v)))))))
       ((,op ,e) (guard (unaryop? op))
        (lift-expr->stmt
         e (lambda (e^)
             (finish `(,op ,e^)))))
       ((,op ,e1 ,e2) (guard (binop? op))
        (lift-expr->stmt
         e1 (lambda (e1^)
              (lift-expr->stmt
               e2 (lambda (e2^)
                    (finish `(,op ,e1^ ,e2^)))))))
       (,else (error 'lift-expr->stmt "unknown expression" else)))))
 
 (define lift-stmt*
   (lambda (stmt*)
     (if (null? stmt*)
         '()
         (let ((rest (lift-stmt* (cdr stmt*))))
           (match (car stmt*)
             ((print ,expr)
              (lift-expr->stmt expr (lambda (e^)
                                      (cons `(print ,e^)
                                            rest))))
             ((print ,e1 ,e2)
              (lift-expr->stmt
               e1 (lambda (e1^)
                    (lift-expr->stmt
                     e2 (lambda (e2^)
                          (cons `(print ,e1^ ,e2^)
                                rest))))))
             ((assert ,expr)
              (lift-expr->stmt expr (lambda (e^)
                                      (cons `(assert ,e^)
                                            rest))))
             ((set! ,x ,e)
              ;; TODO: should x be any expression, or just a variable?
              (lift-expr->stmt e (lambda (e^)
                                   (cons `(set! ,x ,e^)
                                         rest))))
             ((vector-set! ,x ,e1 ,e2)
              ;; TODO: should x be any expression, or just a variable?
              ;; WEB: any expression
              (lift-expr->stmt
               x
               (lambda (x^)
                 (lift-expr->stmt
                  e1
                  (lambda (e1^)
                    (lift-expr->stmt e2 (lambda (e2^)
                                          (cons `(vector-set! ,x^ ,e1^ ,e2^)
                                                rest))))))))             
             ((kernel ,iters ,body* ...)
              ;; TODO: For now just pass the kernel through... this
              ;; won't let us declare vectors inside kernels though.
              (cons `(kernel ,iters ,body* ...) rest))
             ((let ,x ,e)
              (lift-expr->stmt e (lambda (e^)
                                   (cons `(let ,x ,e^)
                                         rest))))
             ((return ,expr)
              (lift-expr->stmt expr
                               (lambda (e^)
                                 (cons `(return ,e^) rest))))
             ((for (,x ,start ,end) ,stmt* ...)
              (lift-expr->stmt
               start
               (lambda (start)
                 (lift-expr->stmt
                  end
                  (lambda (end)
                    (cons `(for (,x ,start ,end) . ,(lift-stmt* stmt*))
                          rest))))))
             (,else (error 'lift-stmt* "unknown statement" else)))))))

 (define (lift-fn fn)
   (match fn
     ((fn ,name ,args ,stmt* ...)
      `(fn ,name ,args . ,(lift-stmt* stmt*)))
     (,else (error 'lift-fn "bad function" else))))

 (define lift-vectors
   (lambda (mod)
     (match mod
       ((module ,fn* ...)
        `(module . ,(map lift-fn fn*)))
       (,else (error 'lift-vectors "malformed module" else)))))

 ;; Moves to a lower-level allocate and set! representation for
 ;; vectors. This runs after typechecking.
 (define lower-vectors
   (lambda (mod)
     (match mod
       ((module ,fn* ...)
        `(module . ,(map lower-fn fn*))))))

 (define (lower-fn fn)
   (match fn
     ((fn ,name ,args ,t ,stmt* ...)
      `(fn ,name ,args ,t . ,(apply append (map lower-stmt stmt*))))
     (,else (error 'lower-fn "unknown fn" else))))

 (define lower-stmt
   (lambda (stmt)
     (match stmt
       ((let ,x ,t (vector ,e* ...))
        `((let ,x ,t (int ,(length e*)))
          . ,(let loop ((e* e*)
                        (i 0))
               (if (null? e*)
                   '()
                   `((vector-set! ,(cadr t) (var ,t ,x) (int ,i) ,(car e*)) .
                     ,(loop (cdr e*) (+ 1 i)))))))

       ((let ,x ,t (make-vector ,vt ,e))
        `((let ,x ,t ,e)))

       ((let ,x ,t (iota ,e))
        (let ((i (gensym 'i)))
          `((let ,x ,t ,e)
            (for (,i (int 0) ,e)
              (vector-set! int (var ,t ,x) (var int ,i) (var int ,i))))))
       
       ((let ,x ,t (reduce ,t2 + (var ,tv ,v)))
        (let ((i (gensym 'i)))
          `((let ,x ,t (vector-ref ,t (var ,tv ,v) (int 0)))
            (for (,i (int 1) (length (var ,tv ,v)))
                 (set! (var ,t ,x)
                       (+ (var ,t ,x)
                          (vector-ref ,t (var ,tv ,v) (var int ,i))))))))
       ((kernel ,t ,iter ,[body*] ...)
        `((kernel ,t ,iter . ,(apply append body*))))
       ((let ,x ,t ,e)
        `((let ,x ,t ,e)))
       ((print ,expr)
        `((print ,expr)))
       ((print ,e1 ,e2)
        `((print ,e1 ,e2)))      
       ((assert ,expr)
        `((assert ,expr)))
       ((set! ,x ,i) `((set! ,x ,i)))
       ((vector-set! ,t ,e1 ,i ,e2)
        `((vector-set! ,t ,e1 ,i ,e2)))
       ((for ((,x ,t) ,start ,end) ,[body*] ...)
        `((for (,x ,start ,end) . ,(apply append body*))))
       ((return ,expr)
        `((return ,expr)))
       (,else (error 'lower-stmt "unknown statment" else)))))

 ;; Uglify vectors takes our nice surface-level vector syntax and
 ;; converts it into an abomination of C function calls to the generic
 ;; vector representation library thing.
 ;;
 ;; It runs after lower-vectors but before compile-module.
 (define (uglify-vectors mod)
   (match mod
     ((module ,[uglify-fn -> fn*] ...)
      `(module ,fn* ...))))

 (define uglify-fn
   (lambda (fn)
     (match fn
       ((fn ,name ,args ,t ,[uglify-stmt -> stmt*] ...)
        `(fn ,name ,args ,t ,(apply append stmt*) ...)))))

 (define decode-vector-type
   (lambda (t)
     (match t
       ((vector ,[dim t])
        (values (+ 1 dim) t))
       (,t (values 0 t)))))

 (define extract-expr-type
   (lambda (e)
     (match e
       ((int ,n) 'int)
       ((var ,t ,x) t)
       ;; This next case is sort of a hack
       (,n (guard (integer? n)) 'int)
       (,else (error 'extract-expr-type
                     "cannot find type" else)))))

 (define uglify-let-vec
   (lambda (t e)
     (match e
       ((int ,y)
        (let-values (((dim t) (decode-vector-type `(vector ,t))))
          (unless (eq? dim 1)
            (error 'uglify-let-vec
                   "only 1D vectors are supported at the moment"))
          `(call hmk_vec (int ,dim) (int ,y) (sizeof ,t))))
       ((var int ,y)
        (let-values (((dim t) (decode-vector-type `(vector ,t))))
          `(call hmk_vec (int ,dim) (var int ,y) (sizeof ,t))))
       ((call hvec_length ,v ,sz)
        (let-values (((dim t) (decode-vector-type `(vector ,t))))
          `(call hmk_vec (int ,dim) (call hvec_length ,v ,sz) (sizeof ,t))))
       ((var ,tv ,y)
        `(var ,tv ,y))
       ;; Otherwise, just hope it works! We should use more type
       ;; information here.
       (,else else)
       (,else (error 'uglify-let-vec
                     "invalid vector initalizer" else)))))
 
 (define uglify-stmt
   (lambda (stmt)
     (match stmt
       ((let ,x (vector ,t) ,[uglify-expr -> init])
        (let ((vv (uglify-let-vec t init)))
          (cons
           `(let ,x (ptr void) ,vv)
           (match vv
             ((var int ,y) '())
             ((var ,tv ,y)
              `((let ,(gensym x) free_this (var (ptr void) ,x))))
             (,else '())))))
       ((let ,x ,t ,[uglify-expr -> e])
        `((let ,x ,t ,e)))
       ((for (,i ,[uglify-expr -> start] ,[uglify-expr -> end])
             ,[uglify-stmt -> stmt*] ...)
        `((for (,i ,start ,end) ,(apply append stmt*) ...)))
       ;; TODO: is set! really just a binop at this point?
       ((set! ,[uglify-expr -> lhs] ,[uglify-expr -> rhs])
        `((set! ,lhs ,rhs)))
       ((return ,[uglify-expr -> e])
        `((return ,e)))
       ((assert ,[uglify-expr -> e])
        `((assert ,e)))
       ;; TODO: vector-set! needs a type annotation. For now we'll
       ;; cheat and only allow integer literals
;;; WEB: not sure what to do here when adding type annotations to variables
       ((vector-set! ,t ,[uglify-expr -> x] ,[uglify-expr -> i]
                     ,[uglify-expr -> v])
        (uglify-vector-set! t x i v))
       ;; TODO: Hmm... we need more type information to correctly
       ;; generate code here.
       ((print ,[uglify-expr -> e])
        `((print ,e)))
       ((print ,[uglify-expr -> e1] ,[uglify-expr -> e2])
        `((print ,e1 ,e2)))       
       ((kernel ,t ,iters ,[stmt*] ...)
        ;; We erase kernel types here... It might be too soon.
        `((kernel ,iters ,(apply append stmt*) ...)))
       (,else
        (error 'uglify-stmt "unsupported stmt" else)))))

 (define vec-ref-1d
   (lambda (x i t)
     `(deref (cast (ptr ,t)
                   (call hvec_ref
                         (call hvec_ref ,x (int 0) (int 0))
                         ,i (sizeof ,t))))))
 
 (define uglify-vector-set!
   (lambda (t x i v)
     (let ((i (if (integer? i) `(int ,i) i)))
       (match t
         ((vector ,t)
          (error 'uglify-vector-set!
                 "Only 1D vectors are supported at the moment.")
          `((do (call vec_set_vec
                      (addressof ,x)
                      ,i ,v))))
         (,scalar
          (guard (symbol? scalar))
          `((set! ,(vec-ref-1d x i scalar) ,v)))
         (,else (error 'uglify-vector-set!
                       "unsupported vector type" else))))))

 (define uglify-expr
   (lambda (e)
     (match e
       ((time) '(time))
       ((int ,n) `(int ,n))
       ((u64 ,n) `(u64 ,n))
       ((var ,tx ,x) `(var ,tx ,x))
       ((,op ,[lhs] ,[rhs]) (guard (binop? op))
        `(,op ,lhs ,rhs))
       ((vector-ref ,t ,[e] ,[i])
        (uglify-vector-ref t e i))
       ((length (var (vector ,t) ,x))
        ;; TODO: this assumes a 1D vector
        `(call hvec_length (call hvec_ref (var (vector ,t) ,x)
                                 (int 0) (int 0))
               (sizeof ,t)))
       (,else
        (error 'uglify-expr (format "unsupported expr: ~s" else))))))

 (define uglify-vector-ref
   (lambda (t e i)
     (match t
       ((vector ,t)
        `(call hvec_ref ,e ,i (call hvec_byte_size (call hvec_ref ,e ,0 ,0))))
       (,scalar
        (guard (symbol? scalar))
        (vec-ref-1d e i t))
       (,else (error 'uglify-vector-ref "unsupported type" else)))))
 )
