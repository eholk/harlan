(library
  (harlan front parser)
  (export parse-harlan)
  (import
    (rnrs)
    (harlan helpers)
    (except (elegant-weapons helpers) ident?)
    (cKanren mk))

(define (check-idents x*)
  (or (null? x*)
      (begin
        (unless (symbol? (car x*))
          (error 'check-idents "invalid identifier" (car x*)))
        (check-idents (cdr x*)))))

(define-match parse-harlan
  ((module ,decl* ...)
   (let ((type-env (apply append
                          (map (lambda (d)
                                 (match d
                                   ((define-datatype ,name . ,_)
                                    (list name))
                                   (,else '())))
                               decl*))))
     `(module . ,(map (parse-decl type-env) decl*)))))

(define-match (parse-decl type-env)
  ((extern ,name . ,[(parse-type type-env) -> t])
   (begin
     (unless (symbol? name)
       (error 'parse-harlan "invalid extern name, expected symbol" name))
     `(extern ,name . ,t)))
  ((define-datatype ,name
     (,tag ,[(parse-type type-env) -> t] ...) ...)
   `(define-datatype ,name (,tag ,t ...) ...))
  ((define (,name . ,args) . ,stmt*)
   (begin
     (unless (symbol? name)
       (error 'parse-harlan "invalid function name, expected symbol" name))
     (let* ((args^ (map gensym args))
            (env (map cons args args^)))
       `(fn ,name ,args^ ,(make-begin (map (parse-stmt env) stmt*))))))
  ((fn ,name ,args . ,stmt*)
   (begin
     (unless (symbol? name)
       (error 'parse-harlan "invalid function name, expected symbol" name))
     (let* ((args^ (map gensym args))
            (env (map cons args args^)))
       `(fn ,name ,args^ ,(make-begin (map (parse-stmt env) stmt*)))))))

(define-match (parse-type type-env)
  (void 'void)
  (char 'char)
  (int 'int)
  (u64 'u64)
  (str 'str)
  (float 'float)
  (ofstream 'ofstream)
  (,t (guard (member t type-env)) `(adt ,t))
  ((ptr ,[t]) `(ptr ,t))
  ((vec ,n ,[t])
   (guard (integer? n))
   `(vec ,n ,t))
  ((closure (,[t*] ...) -> ,[t])
   `(closure ,t* -> ,t))
  (((,[t*] ...) -> ,[t])
   `(,t* -> ,t)))

(define-match (parse-stmt env)
  ((assert ,[(parse-expr env) -> e])
   `(assert ,e))
  ((print ,[(parse-expr env) -> e] ...)
   `(print . ,e))
  ((println ,[(parse-expr env) -> e] ...)
   `(println . ,e))
  ((return) `(return))
  ((return ,[(parse-expr env) -> e])
   `(return ,e))
  ((if ,[(parse-expr env) -> test]
       ,[(parse-stmt env) -> conseq])
   `(if ,test ,conseq))
  ((if ,[(parse-expr env) -> test]
       ,[(parse-stmt env) -> conseq]
       ,[(parse-stmt env) -> alt])
   `(if ,test ,conseq ,alt))
  ((begin ,[(parse-stmt env) -> stmt*] ...)
   `(begin . ,stmt*))
  ((while ,[(parse-expr env) -> test]
          ,[(parse-stmt env) -> stmt*] ...)
   `(while ,test ,(make-begin stmt*)))
  ((set! ,[(parse-expr env) -> x]
     ,[(parse-expr env) -> e])
   `(set! ,x ,e))
  ((let-region (,r) ,[e]) `(let-region (,r) ,e))
  ((let ((,x* ,[(parse-expr env) -> e*]) ...) . ,body)
   (begin
     (check-idents x*)
     (let* ((x*^ (map gensym x*))
            (env (append (map cons x* x*^) env))
            (body (map (parse-stmt env) body)))
       `(let ((,x*^ ,e*) ...) ,(make-begin body)))))
  (,[(parse-expr env) -> e] `(do ,e)))

(define-match (parse-expr env)
  (,c (guard (char? c)) `(char ,c))
  (,f (guard (float? f)) `(float ,f))
  (,n (guard (integer? n)) `(num ,n))
  (,b (guard (boolean? b)) `(bool ,b))
  (,x (guard (symbol? x))
    (let ((x^ (assq x env)))
      (unless x^ (error 'parse-expr "free variable" x))
      `(var ,(cdr x^))))
  (,str (guard (string? str)) `(str ,str))
  ((var ,x)
   (guard (symbol? x))
   (let ((x^ (assq x env)))
     (unless x^ (error 'parse-expr "free variable" x))
     `(var ,(cdr x^))))
  ((vector ,[e*] ...)
   `(vector . ,e*))
  ((vector-r ,r ,[e*] ...)
   `(vector-r ,r . ,e*))
  ((begin ,[(parse-stmt env) -> stmt*] ... ,[(parse-expr env) -> expr])
   `(begin ,@stmt* ,expr))
  ((make-vector ,[(parse-expr env) -> size] ,[expr])
   `(make-vector ,size ,expr))
  ((if ,[test] ,[conseq] ,[alt])
   `(if ,test ,conseq ,alt))
  ((iota ,[e])
   `(iota ,e))
  ((iota-r ,r ,[e])
   `(iota-r ,r ,e))
  ((vector-ref ,[v] ,[i])
   `(vector-ref ,v ,i))
  ((unsafe-vector-ref ,[v] ,[i])
   `(unsafe-vector-ref ,v ,i))
  ((unsafe-vec-ptr ,[v]) `(unsafe-vec-ptr ,v))
  ((length ,[e])
   `(length ,e))
  ((int->float ,[e]) `(int->float ,e))
  ((float->int ,[e]) `(float->int ,e))
  ((lambda (,x* ...) ,stmt* ... ,expr)
   (begin
     (check-idents x*)
     ;; We don't need to rename things anymore because the macro
     ;; expander renames things instead.
     (let* ((env (append (map cons x* x*) env))
            (stmt* (map (parse-stmt env) stmt*))
            (expr ((parse-expr env) expr)))
       `(lambda (,x* ...) ,(make-begin `(,@stmt* ,expr))))))
  ((let ((,x* ,[(parse-expr env) -> e*]) ...) ,stmt* ... ,expr)
   (begin
     (check-idents x*)
     (let* ((x*^ (map gensym x*))
            (env (append (map cons x* x*^) env))
            (stmt* (map (parse-stmt env) stmt*))
            (expr ((parse-expr env) expr)))
       `(let ((,x*^ ,e*) ...) ,(make-begin `(,@stmt* ,expr))))))
  ((kernel ((,x* ,[e*]) ...) ,stmt* ... ,e)
   (begin
     (check-idents x*)
     (let* ((x*^ (map gensym x*))
            (env (append (map cons x* x*^) env)))
       `(kernel ((,x*^ ,e*) ...)
          ,(make-begin
             `(,@(map (parse-stmt env) stmt*)
               ,((parse-expr env) e)))))))
  ((kernel-r ,r ((,x* ,[e*]) ...) ,stmt* ... ,e)
   (begin
     (check-idents x*)
     (let* ((x*^ (map gensym x*))
            (env (append (map cons x* x*^) env)))
       `(kernel-r ,r ((,x*^ ,e*) ...)
          ,(make-begin
             `(,@(map (parse-stmt env) stmt*)
               ,((parse-expr env) e)))))))
  ((match ,[e]
     ((,tag ,x* ...) ,s* ... ,e*) ...)
   (guard (and (andmap ident? tag)
               (andmap (lambda (x*) (andmap ident? x*)) x*)))
   (let-values (((x* e*)
                 (let ((x*^ (map (lambda (x) (map gensym x)) x*)))
                   (values
                    x*^
                    (map (lambda (x* x*^ s* e*)
                           (let ((env (append (map cons x* x*^) env)))
                             (make-begin
                              (append
                               (map (lambda (s) ((parse-stmt env) s)) s*)
                               (list ((parse-expr env) e*))))))
                         x* x*^ s* e*)))))
     `(match ,e
        ((,tag ,x* ...) ,e*) ...)))
  ((,op ,[lhs] ,[rhs])
   (guard (or (binop? op) (relop? op)))
   `(,op ,lhs ,rhs))
  ((,rator ,[rand*] ...)
   ;; We don't put top-level identifiers in the environment for some
   ;; reason, so here we take advantage of that fact to differentiate
   ;; between call and invoke instructions.
   (guard (and (symbol? rator) (not (assq rator env))))
   `(call ,rator . ,rand*))
  ((,[rator] ,[rand*] ...)
   `(invoke ,rator . ,rand*)))

;; end library
)
