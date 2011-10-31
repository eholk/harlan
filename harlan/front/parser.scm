(library
  (harlan front parser)
  (export parse-harlan)
  (import
    (rnrs)
    (only (chezscheme) format printf)
    (util verify-grammar)
    (util helpers)
    (util match))

;; parse-harlan takes a syntax tree that a user might actually want
;; to write and converts it into something that's more easily
;; analyzed by the type inferencer and the rest of the compiler.
;; This subsumes the functionality of the previous
;; simplify-literals mini-pass.

;; unnests lets, checks that all variables are in scope, and
;; renames variables to unique identifiers
  
(define-match parse-harlan
  ((module ,[parse-decl -> decl*] ...)
   `(module . ,decl*)))

(define-match parse-decl
  ((extern ,name . ,[parse-type -> t])
   (guard (symbol? name))
   `(extern ,name . ,t))
  ((fn ,name ,args . ,[(parse-stmt* '()) -> stmt*])
   `(fn ,name ,args . ,stmt*)))

(define-match parse-type
  (int 'int)
  (u64 'u64)
  (void 'void)
  (str 'str)
  (float 'float)
  ((vector ,[t] ,n)
   (guard (integer? n))
   `(vector ,t ,n))
  (((,[t*] ...) -> ,[t]) `(,t* -> ,t)))

(define-match (parse-stmt* env)
  (() '())
  ((,[(parse-stmt env) -> stmt env^] . ,stmt*)
   (append stmt ((parse-stmt* env^) stmt*))))

(define-match (parse-stmt env)
  ((assert ,[(parse-expr env) -> e])
   (values `((assert ,e)) env))
  ((print ,[(parse-expr env) -> e])
   (values `((print ,e)) env))
  ((return ,[(parse-expr env) -> e])
   (values `((return ,e)) env))
  ((for (,x ,start ,end) . ,stmt*)
   (guard (symbol? x))
   (let* ((x^ (gensym x))
          (env (cons `(,x . ,x^) env)))
     (let ((start ((parse-expr env) start))
           (end ((parse-expr env) end))
           (stmt* ((parse-stmt* env) stmt*)))
       (values `((for (,x^ ,start ,end) . ,stmt*)) env))))
  ((while ,[(parse-expr env) -> test] .
          ,[(parse-stmt* env) -> stmt*])
   (values `((while ,test . ,stmt*)) env))
  ((set! ,[(parse-expr env) -> x]
     ,[(parse-expr env) -> e])
   (values `((set! ,x ,e)) env))
  ((vector-set!
     ,[(parse-expr env) -> v]
     ,[(parse-expr env) -> i]
     ,[(parse-expr env) -> e])
   (values `((vector-set! ,v ,i ,e)) env))
  ((let ,x ,e)
   (guard (symbol? x))
   (let* ((x^ (gensym x))
          (env (cons `(,x . ,x^) env))
          (e ((parse-expr env) e)))
     (values `((let ,x^ ,e)) env)))
  ((let ((,x* ,[(parse-expr env) -> e*]) ...) . ,body)
   (let* ((x*^ (map gensym x*))
          (env (append (map cons x* x*^) env))
          (body ((parse-stmt* env) body)))
     (values `((let ((,x*^ ,e*) ...) . ,body)) env)))
  (,[(parse-expr env) -> e] (values `((do ,e)) env)))

(define-match (parse-expr env)
  (,f (guard (float? f)) `(float ,f))
  (,n (guard (integer? n)) `(num ,n))
  (,x (guard (symbol? x))
    (let ((x^ (assq x env)))
      (unless x^ (error 'parse-expr (format "Free variable ~s" x)))
      `(var ,(cdr x^))))
  (,str (guard (string? str)) `(str ,str))
  ((var ,x)
   (guard (symbol? x))
   (let ((x^ (assq x env)))
     (unless x^ (error 'parse-expr (format "Free variable ~s" x)))
     `(var ,(cdr x^))))
  ((vector ,[e*] ...)
   `(vector . ,e*))
  ((make-vector ,[e])
   `(make-vector ,e))
  ((iota ,[e])
   `(iota ,e))
  ((vector-ref ,[v] ,[i])
   `(vector-ref ,v ,i))
  ((length ,[e])
   `(length ,e))
  ((int->float ,[e]) `(int->float ,e))
  ((kernel ((,x* ,[e*]) ...) ,stmt* ... ,e)
   (let* ((x*^ (map gensym x*))
          (env (append (map cons x* x*^) env)))
     `(kernel ((,x*^ ,e*) ...)
        ,@
        (let loop ((stmt* stmt*) (env env))
          (cond
            ((null? stmt*) `(,((parse-expr env) e)))
            (else
              (let-values (((stmt^ env^)
                            ((parse-stmt env) (car stmt*))))
                `(,@stmt^ . ,(loop (cdr stmt*) env^)))))))))
  ((reduce ,op ,[e])
   (guard (reduceop? op))
   `(reduce ,op ,e))
  ((,op ,[lhs] ,[rhs])
   (guard (or (binop? op) (relop? op)))
   `(,op ,lhs ,rhs))
  ((,rator ,[rand*] ...)
   (guard (symbol? rator))
   `(call ,rator . ,rand*)))

;; end library
)
