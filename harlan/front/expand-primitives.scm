(library
  (harlan front expand-primitives)
  (export expand-primitives)
  (import
    (rnrs)
    (elegant-weapons helpers)
    (elegant-weapons compat)
    (only (vicare) printf trace-define))

  ;; This pass macro-expands primitives. It also inserts fresh region
  ;; variables.
  
  (define-match expand-primitives
    ((module ,[expand-prim-decl -> decl*] ...)
     `(module ,(apply append decl*) ...)))
  
  (define-match expand-prim-decl
    ((fn ,name ,args ,t ,[expand-prim-stmt -> stmt])
     `((fn ,name ,args ,t ,stmt)))
    ((define-datatype ,t (,c ,t* ...) ...)
     ;; For now we guard for simple enum types. The region calling
     ;; convention doesn't work very well with print the way it
     ;; currently works.
     (guard (symbol? t))
     (let ((adt (gensym 'adt))
           (out (gensym 'out))
           (type (match t
                   ((,n ,r) `(adt ,n ,r))
                   (,n `(adt ,n)))))
       `((define-datatype ,t (,c ,t* ...) ...)
         (fn print (,adt ,out) (fn (,type (ptr ofstream)) -> void)
             (begin
               (do (match int (var ,type ,adt)
                     ,@(map (lambda (c t*)
                              (let ((x* (map (lambda (_) (gensym 'x)) t*))
                                    )
                                `((,c ,x* ...)
                                  (begin
                                    (print (str ,(string-append
                                                  "("
                                                  (symbol->string c)))
                                           (var (ptr ofstream) ,out))
                                    ,@(let ((out (map (lambda (_)
                                                        `(var (ptr ofstream)
                                                              ,out))
                                                      t*)))
                                        `((begin (print (str " ")
                                                        ,out)
                                                 (print (var ,t* ,x*)
                                                        ,out)) ...))
                                    (print (str ")") (var (ptr ofstream) ,out))
                                    (int 0)))))
                            c t*)))
               (return)))
         (fn print (,adt) (fn (,type) -> void)
             (begin
               (do (match int (var ,type ,adt)
                     ,@(map (lambda (c t*)
                              (let ((x* (map (lambda (_) (gensym 'x)) t*))
                                    )
                                `((,c ,x* ...)
                                  (begin
                                    (print (str ,(string-append
                                                  "("
                                                  (symbol->string c))))
                                    (begin (print (str " "))
                                           (print (var ,t* ,x*))) ...
                                    (print (str ")"))
                                    (int 0)))))
                            c t*)))
               (return))))))
    ;; Fallback for datatypes we don't generate print for.
    ((define-datatype ,t (,c ,t* ...) ...)
     `((define-datatype ,t (,c ,t* ...) ...)))
    ((extern ,name ,args -> ,rtype)
     `((extern ,name ,args -> ,rtype))))

  (define-match expand-prim-stmt
    ((let ((,x* ,t* ,[expand-prim-expr -> e*]) ...) ,[body])
     `(let ((,x* ,t* ,e*) ...) ,body))
    ((let-region (,r ...) ,[body]) `(let-region (,r ...) ,body))
    ((set! ,[expand-prim-expr -> lhs] ,[expand-prim-expr -> rhs])
     `(set! ,lhs ,rhs))
    ((if ,[expand-prim-expr -> test] ,[conseq] ,[altern])
     `(if ,test ,conseq ,altern))
    ((if ,[expand-prim-expr -> test] ,[conseq])
     `(if ,test ,conseq))
    ((while ,[expand-prim-expr -> test] ,[body])
     `(while ,test ,body))
    ((begin ,[stmt*] ...)
     `(begin . ,stmt*))
    ((print (vec ,r ,t) ,[expand-prim-expr -> e]
            ,[expand-prim-expr -> stream])
     (expand-print r t e stream))
    ((print (vec ,r ,t) ,[expand-prim-expr -> e])
     (expand-print r t e))
    ((print ,t ,[expand-prim-expr -> e] ...)
     `(print . ,e))
    ((println ,t . ,expr)
     `(begin
        ,(expand-prim-stmt `(print ,t . ,expr))
        (print (str "\n"))))
    ((assert ,[expand-prim-expr -> e])
     `(assert ,e))
    ((return) `(return))
    ((return ,[expand-prim-expr -> e])
     `(return ,e))
    ((do ,[expand-prim-expr -> e])
     `(do ,e)))
  
  (define-match expand-prim-expr
    ((,t ,v) (guard (scalar-type? t)) `(,t ,v))
    ((var ,t ,x) `(var ,t ,x))
    ((int->float ,[e]) `(int->float ,e))
    ((float->int ,[e]) `(float->int ,e))
    ((iota ,[e])
     `(iota ,e))
    ((iota-r ,r ,[e])
     `(iota-r ,r ,e))
    ((vector (vec ,r ,t) ,[e*] ...)
     `(vector (vec ,r ,t) ,r . ,e*))
    ((vector-r (vec ,r ,t) ,r ,[e*] ...)
     `(vector (vec ,r ,t) ,r . ,e*))
    ((make-vector (vec ,r ,t) ,[size] ,[init])
     (let ((i (gensym 'i))
           (len (gensym 'len))
           (v (gensym 'v)))
       `(let ((,len int ,size))
          (let ((,v (vec ,r ,t) (make-vector ,t ,r (var int ,len))))
            (begin
              (let ((,i int (int 0)))
                (while (< (var int ,i) (var int ,len))
                  (begin
                    (set! (vector-ref ,t
                                      (var (vec ,r ,t) ,v)
                                      (var int ,i))
                          ,init)
                    (set! (var int ,i) (+ (var int ,i) (int 1))))))
              (var (vec ,r ,t) ,v))))))
    ((vector-ref ,t ,[v] ,[i])
     `(vector-ref ,t ,v ,i))
    ((unsafe-vector-ref ,t ,[v] ,[i])
     `(unsafe-vector-ref ,t ,v ,i))
    ((unsafe-vec-ptr ,t ,[v])
     `(unsafe-vec-ptr ,t ,v))
    ((length ,[e])
     `(length ,e))
    ((call ,[f] ,[args] ...)
     `(call ,f . ,args))
    ((invoke ,[f] ,[args] ...)
     `(invoke ,f . ,args))
    ((lambda ,t0 ((,x ,t) ...) ,[e])
     `(lambda ,t0 ((,x ,t) ...) ,e))
    ((if ,[test] ,[conseq] ,[altern])
     `(if ,test ,conseq ,altern))
    ((if ,[test] ,[conseq])
     `(if ,test ,conseq))
    ((kernel ,ktype (((,x ,t) (,[xs] ,ts)) ...) ,[body])
     `(kernel ,ktype ,(gensym 'region) (((,x ,t) (,xs ,ts)) ...) ,body))
    ((kernel-r ,ktype ,r (((,x ,t) (,[xs] ,ts)) ...) ,[body])
     `(kernel ,ktype ,r (((,x ,t) (,xs ,ts)) ...) ,body))
    ((let ((,x* ,t* ,[e*]) ...) ,[e])
     `(let ((,x* ,t* ,e*) ...) ,e))
    ((begin ,[expand-prim-stmt -> s*] ... ,[e])
     `(begin ,s* ... ,e))
    ((+ (vec ,t) ,[lhs] ,[rhs])
     (expand-vec-addition t lhs rhs))
    ((= (vec ,r ,t) ,[lhs] ,[rhs])
     (expand-vec-comparison t r lhs rhs))
    ((match ,t ,[e] (,p ,[e*]) ...)
     `(match ,t ,e (,p ,e*) ...))
    ((,op ,t ,[lhs] ,[rhs])
     (guard (or (relop? op) (binop? op)))
     `(,op ,lhs ,rhs)))

  (define (expand-print r t e . stream)
    (let ((v (gensym 'v)) 
          (i (gensym 'i))
          (len (gensym 'len)))
      `(let ((,v (vec ,r ,t) ,e))
         (begin
           (print (str "[") . ,stream)
           (let ((,i int (int 0))
                 (,len int (length (var (vec ,r ,t) ,v))))
             (while (< (var int ,i) (var int ,len))
               (begin
                 ,(if (scalar-type? t)
                      `(if (> (var int ,i) (int 0))
                           (print (str " ") . ,stream))
                      `(if (> (var int ,i) (int 0))
                           (print (str " \n ") . ,stream)))
                 ,(expand-prim-stmt
                   `(print ,t
                           (vector-ref ,t
                                       (var (vec ,r ,t) ,v) (var int ,i))
                            . ,stream))
                 (set! (var int ,i) (+ (var int ,i) (int 1))))))
           (print (str "]") . ,stream)))))

  (define (expand-vec-addition t lhs rhs)
    (let ((l (gensym 'lhs))
          (r (gensym 'rhs))
          (len (gensym 'len))
          (i (gensym 'i))
          (res (gensym 'res))
          (lhsi (gensym 'lhsi))
          (rhsi (gensym 'rhsi)))
      `(let ((,l (vec ,t) ,lhs)
             (,r (vec ,t) ,rhs))
         (let ((,len int (length (var (vec ,t) ,l))))
           (let ((,res (vec ,t) (make-vector ,t ,(gensym 'region) (var int ,len))))
             (begin
               (for (,i (int 0) (var int ,len) (int 1))
                    (let ((,lhsi
                           ,t
                           (vector-ref ,t (var (vec ,t) ,l)
                                       (var int ,i)))
                          (,rhsi
                           ,t
                           (vector-ref ,t (var (vec ,t) ,r)
                                       (var int ,i))))
                      (set! (vector-ref ,t
                                        (var (vec ,t) ,res)
                                        (var int ,i))
                            ,(expand-prim-expr
                              `(+ ,t (var ,t ,lhsi) (var ,t ,rhsi))))))
               (var (vec ,t) ,res)))))))

  (define (expand-vec-comparison t r lhs rhs)
    (let ((lv (gensym 'lhs))
          (rv (gensym 'rhs))
          (len (gensym 'len))
          (i (gensym 'i))
          (res (gensym 'res))
          (lhsi (gensym 'lhsi))
          (rhsi (gensym 'rhsi)))
      `(let ((,lv (vec ,r ,t) ,lhs)
             (,rv (vec ,r ,t) ,rhs))
         (let ((,len int (length (var (vec ,r ,t) ,lv)))
               (,res bool (bool #t)))
           (begin
             (if (= (var int ,len)
                    (length (var (vec ,r ,t) ,rv)))
                 (for (,i (int 0) (var int ,len) (int 1))
                      (let ((,lhsi ,t
                                   (vector-ref ,t (var (vec ,r ,t) ,lv)
                                               (var int ,i)))
                            (,rhsi ,t
                                   (vector-ref ,t (var (vec ,r ,t) ,rv)
                                               (var int ,i))))
                        (if (= ,(expand-prim-expr
                                 `(= ,t (var ,t ,lhsi) (var ,t ,rhsi)))
                               (bool #f))
                            (begin (set! (var bool ,res) (bool #f))
                                   (set! (var int ,i) (var int ,len))))))
                 (set! (var bool ,res) (bool #f)))
             (var bool ,res))))))

  ;; end library
  )
