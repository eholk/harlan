(library
  (harlan middle annotate-free-vars)
  (export annotate-free-vars)
  (import
    (only (chezscheme) format printf)
    (rnrs)
    (elegant-weapons print-c)
    (elegant-weapons match)
    (elegant-weapons helpers))

(define-match annotate-free-vars
  ((module ,[(annotate-decl '()) -> decl*] ...)
   `(module ,decl* ...)))

(define-match (annotate-decl gamma)
  ((fn ,name ,args ,type . ,stmt*)
   (let-values (((stmt* gamma) ((annotate-stmt* gamma) stmt*)))
     `(fn ,name ,args ,type . ,stmt*)))
  ((extern ,name ,arg-types -> ,type)
   `(extern ,name ,arg-types -> ,type)))

(define (annotate-stmt* gamma)
  (lambda (stmt*)
    (let loop ([stmt* stmt*] [gamma gamma] [new-stmt* '()])
      (cond
        [(null? stmt*)
         (values (reverse new-stmt*) gamma)]
        [else
          (let-values (((stmt gamma^)
                        ((annotate-stmt gamma) (car stmt*))))
            (loop (cdr stmt*) gamma (cons stmt new-stmt*)))]))))

(define-match (annotate-stmt gamma)
  ((begin . ,[(annotate-stmt* gamma) -> stmt* gamma^])
   (values `(begin . ,stmt*) gamma^))
  ((kernel (((,x* ,t*) (,xs* ,ts*)) ...) . ,stmt*)
   (let ((gamma (append (map cons x* t*) gamma)))
     (let-values (((stmt* gamma) ((annotate-stmt* gamma) stmt*)))
       (let* ((fv* (remove* x* (free-vars-stmt* stmt*))))
         (let ((fv* fv*))
           (values
             `(kernel (((,x* ,t*) (,xs* ,ts*)) ...)
                (free-vars ,@fv*) . ,stmt*)
             gamma))))))
  ((return ,[(annotate-expr gamma) -> e])
   (values `(return ,e) gamma))
  ((if ,[(annotate-expr gamma) -> test]
       ,[(annotate-stmt gamma) -> conseq gamma^])
   (values `(if ,test ,conseq) gamma))
  ((if ,[(annotate-expr gamma) -> test]
       ,[(annotate-stmt gamma) -> conseq gamma^]
       ,[(annotate-stmt gamma) -> alt gamma^^])
   (values `(if ,test ,conseq ,alt) gamma))
  ((do ,[(annotate-expr gamma) -> e*] ...)
   (values `(do . ,e*) gamma))
  ((let ((,x* ,[(annotate-expr gamma) -> e*]) ...) ,stmt)
   (let ((gamma (union x* gamma)))
     (let-values (((stmt gamma^) ((annotate-stmt gamma) stmt)))
       (values `(let ((,x* ,e*) ...) ,stmt) gamma))))
  ((while ,[(annotate-expr gamma) -> e] . ,s)
   (let-values (((s gamma) ((annotate-stmt* gamma) s)))
     (values `(while ,e ,s ...) gamma)))
  ((for (,x ,[(annotate-expr gamma) -> start]
          ,[(annotate-expr gamma) -> end])
     . ,s)
   (let ((gamma `((,x . int) . ,gamma)))
     (let-values (((s gamma) ((annotate-stmt* gamma) s)))
       (values `(for (,x ,start ,end) ,s ...) gamma))))
  ((set! ,x ,[(annotate-expr gamma) -> e])
   (values `(set! ,x ,e) gamma))
  ((print ,[(annotate-expr gamma) -> e])
   (values `(print ,e) gamma))
  ((assert ,[(annotate-expr gamma) -> e])
   (values `(assert ,e) gamma)))

(define-match (annotate-expr gamma)
  ((int ,n) (guard (integer? n)) `(int ,n))
  ((u64 ,n) (guard (integer? n)) `(u64 ,n))
  ((float ,f) `(float ,f))
  ((var ,t ,x) (guard (symbol? x)) `(var ,t ,x))
  ((str ,s) (guard (string? s)) `(str ,s))
  ((cast ,t ,[(annotate-expr gamma) -> e]) `(cast ,t ,e))
  ((call ,t ,rator ,[rand*] ...)
   (guard (symbol? rator))
   `(call ,t ,rator ,rand* ...))
  ((let ((,x* ,[(annotate-expr gamma) -> e*]) ...) ,expr)
   (let* ((gamma (union x* gamma))
          (expr ((annotate-expr gamma) expr)))
     `(let ((,x* ,e*) ...) ,expr)))
  ((begin ,stmt* ... ,expr)
   (let-values (((stmt* gamma^)
                 ((annotate-stmt* gamma) stmt*)))
     (let ((expr ((annotate-expr gamma^) expr)))
       (make-begin `(,@stmt* ,expr)))))
  ((if ,[(annotate-expr gamma) -> test]
       ,[(annotate-expr gamma) -> conseq]
       ,[(annotate-expr gamma) -> alt])
   `(if ,test ,conseq ,alt))
  ((sizeof ,t)
   `(sizeof ,t))
  ((addressof ,[e]) `(addressof ,e))
  ((,op ,[lhs] ,[rhs])
   (guard (or (binop? op) (relop? op)))
   `(,op ,lhs ,rhs))
  ((vector-ref ,t ,[v] ,[i])
   `(vector-ref ,t ,v ,i)))

 (define-match free-vars-stmt*
   [() '()]
   [(,[free-vars-stmt -> stmt] . ,[rest])
    (union stmt rest)])

 (define-match free-vars-stmt
   ((kernel (((,x* ,t*) (,xs* ,ts*)) ...) .
      ,[free-vars-stmt* -> stmt*])
    (remove* x* stmt*))
   ((begin . ,[free-vars-stmt* -> e]) e)
   ((return ,[free-vars-expr -> e]) e)
   ((do ,[free-vars-stmt -> s]) s)
   ((assert ,[free-vars-expr -> e]) e)
   ((if ,[free-vars-expr -> test] ,[conseq])
    (union test conseq))
   ((if ,[free-vars-expr -> test] ,[conseq] ,[alt])
    (union* test conseq alt))
   ((let ((,x ,[free-vars-expr -> fv**])) ,[fv*])
    (union (apply union* fv**) (remove x fv*)))
   ((for (,x ,[free-vars-expr -> start]
           ,[free-vars-expr -> end])
      ,[free-vars-stmt -> s])
    (remove x s))
   ((while ,[free-vars-expr -> test]
      . ,[free-vars-stmt* -> stmt*])
    (union test stmt*))
   ((set! (var ,t ,x) ,[free-vars-expr -> e]) (union `(,x) e))
   ((print ,[free-vars-expr -> e]) e))

(define-match free-vars-expr
  ((int ,n) (guard (number? n)) '())
  ((var ,t ,x) (guard (symbol? x)) `(,x))
  ((str ,s) (guard (string? s)) '())
  ((float ,f) '())
  ((,op ,[free-vars-expr -> e1] ,[free-vars-expr -> e2])
   (guard (or (binop? op) (relop? op)))
   (union e1 e2))
  ((begin ,[free-vars-stmt -> se] ... ,[e])
   (apply union* e se))
  ((if ,[test] ,[conseq] ,[alt])
   (union* test conseq alt))
  ((deref ,[free-vars-expr -> e]) e)
  ((cast ,t ,[free-vars-expr -> e]) e)
  ((addressof ,[free-vars-expr -> e]) e)
  ((vector-ref ,t ,[v] ,[i])
   (union v i))
  ((sizeof ,t) '()))

(define union
  (lambda (s1 s2)
    (cond
      [(null? s1) s2]
      [(member (car s1) s2) (union (cdr s1) s2)]
      [else (cons (car s1) (union (cdr s1) s2))])))

(define union*
  (lambda args
    (union*-aux args)))

(define union*-aux
  (lambda (args)
    (cond
      [(null? args) '()]
      [(null? (cdr args)) (car args)]
      [else (union (union (car args) (cadr args))
                   (union*-aux (cddr args)))])))

(define remove*
  (lambda (x* ls)
    (cond
      [(null? x*) ls]
      [else (remove* (cdr x*) (remove (car x*) ls))])))

(define lookup
  (lambda (x gamma)
    (cond
      [(assq x gamma) => cdr]
      [else (error 'lookup (format "annotate-free-vars--unbound variable: ~s in gamma: ~s\n" x gamma))])))

)
