(library
  (harlan front expand-primitives)
  (export expand-primitives)
  (import
    (rnrs)
    (elegant-weapons helpers)
    (elegant-weapons compat))

  (define externs (make-parameter '()))

  (define (add-externs prim)
    (case prim
      ((write-pgm)
       (externs
         (append
           `((extern open_outfile (str) -> (ptr ofstream))
             (extern close_outfile ((ptr ofstream)) -> void))
           (externs))))))

  (define-match expand-primitives
    ((module ,[expand-prim-decl -> decl*] ...)
     `(module ,(externs) ... ,decl* ...)))
  
  (define-match expand-prim-decl
    ((fn ,name ,args ,t ,[expand-prim-stmt -> stmt])
     `(fn ,name ,args ,t ,stmt))
    ((extern ,name ,args -> ,rtype)
     `(extern ,name ,args -> ,rtype)))

  (define-match expand-prim-stmt
    ((let ((,x* ,t* ,[expand-prim-expr -> e*]) ...) ,[body])
     `(let ((,x* ,t* ,e*) ...) ,body))
    ((set! ,[expand-prim-expr -> lhs] ,[expand-prim-expr -> rhs])
     `(set! ,lhs ,rhs))
    ((vector-set! ,t
       ,[expand-prim-expr -> v]
       ,[expand-prim-expr -> i]
       ,[expand-prim-expr -> e])
     `(vector-set! ,t ,v ,i ,e))
    ((if ,[expand-prim-expr -> test] ,[conseq] ,[altern])
     `(if ,test ,conseq ,altern))
    ((if ,[expand-prim-expr -> test] ,[conseq])
     `(if ,test ,conseq))
    ((while ,[expand-prim-expr -> test] ,[body])
     `(while ,test ,body))
    ((for (,x ,[expand-prim-expr -> start]
            ,[expand-prim-expr -> stop]) ,[body])
     `(for (,x ,start ,stop) ,body))
    ((begin ,[stmt*] ...)
     `(begin . ,stmt*))
    ((print (vec ,n ,t) ,[expand-prim-expr -> e]
       ,[expand-prim-expr -> stream])
     (expand-print n t e stream))
    ((print (vec ,n ,t) ,[expand-prim-expr -> e])
     (expand-print n t e))
    ((print ,t ,[expand-prim-expr -> e])
     `(print ,e))
    ((assert ,[expand-prim-expr -> e])
     `(assert ,e))
    ((return) `(return))
    ((return ,[expand-prim-expr -> e])
     `(return ,e))
    ((do ,[expand-prim-expr -> e])
     `(do ,e))
    ((write-pgm ,file ,data)
     (expand-write-pgm file data)))
  
  (define-match expand-prim-expr
    ((,t ,v) (guard (scalar-type? t)) `(,t ,v))
    ((var ,t ,x) `(var ,t ,x))
    ((int->float ,[e]) `(int->float ,e))
    ((iota ,[e]) `(iota ,e))
    ((vector ,t ,[e*] ...)
     `(vector ,t . ,e*))
    ((make-vector ,t ,[e])
     `(make-vector ,t ,e))
    ((vector-ref ,t ,[v] ,[i])
     `(vector-ref ,t ,v ,i))
    ((length ,[e])
     `(length ,e))
    ((call ,[f] ,[args] ...)
     `(call ,f . ,args))
    ((if ,[test] ,[conseq] ,[altern])
     `(if ,test ,conseq ,altern))
    ((if ,[test] ,[conseq])
     `(if ,test ,conseq))
    ((reduce ,t ,op ,[e])
     `(reduce ,t ,op ,e))
    ((kernel ,ktype (((,x ,t) (,[xs] ,ts)) ...) ,[body])
     `(kernel ,ktype (((,x ,t) (,xs ,ts)) ...) ,body))
    ((let ((,x* ,t* ,[e*]) ...) ,[e])
     `(let ((,x* ,t* ,e*) ...) ,e))
    ((begin ,[expand-prim-stmt -> s*] ... ,[e])
     `(begin ,s* ... ,e))
    ((+ (vec ,n ,t) ,[lhs] ,[rhs])
     (expand-vec-addition n t lhs rhs))
    ((= (vec ,n ,t) ,[lhs] ,[rhs])
     (expand-vec-comparison n t lhs rhs))
    ((,op ,t ,[lhs] ,[rhs])
     (guard (or (relop? op) (binop? op)))
     `(,op ,lhs ,rhs)))

  (define (expand-print n t e . stream)
    (let ((v (gensym 'v)) 
          (i (gensym 'i)))
      `(let ((,v (vec ,n ,t) ,e))
         (begin
           (print (str "[") . ,stream)
           (for (,i (int 0) (length (var (vec ,n ,t) ,v)))
             (begin
               ,(if (scalar-type? t)
                    `(if (> (var int ,i) (int 0))
                         (print (str " ") . ,stream))
                    `(if (> (var int ,i) (int 0))
                         (print (str " \n ") . ,stream)))
               ,(expand-prim-stmt
                  `(print ,t
                     (vector-ref ,t
                       (var (vec ,n ,t) ,v) (var int ,i))
                     . ,stream))))
           (print (str "]") . ,stream)))))

  (define (expand-write-pgm file data)
    (let ((p (gensym 'p))
          (f (gensym 'file))
          (i (gensym 'i))
          (stream (gensym 'stream)))
      (add-externs 'write-pgm)
      `(let ((,f str ,file))
         (let ((,stream (ptr ofstream)
                (call (var ((str) -> (ptr ofstream)) open_outfile)
                  (var str ,f))))
           (begin
             (print (str "P2\n") (var ofstream ,stream))
             (print (str "1024 1024\n") (var ofstream ,stream))
             (print (str "255\n") (var ofstream ,stream))
             (for (,i (int 0) (* (int 1024) (int 1024)))
               (let ((,p int (vector-ref int
                               (vector-ref (vec 1024 int)
                                 ,data
                                 (/ (var int ,i) (int 1024)))
                               (mod (var int ,i) (int 1024)))))
                 (begin
                   (if (< (var int ,p) (int 0))
                       (set! (var int ,p) (int 0))
                       (if (> (var int ,p) (int 255))
                           (set! (var int ,p) (int 255))))
                   (print (var int ,p)
                     (var ofstream ,stream)))))
             (print (str " ") (var ofstream ,stream))
             (do (call (var (((ptr ofstream)) -> void) close_outfile)
                       (var (ptr ofstream) ,stream))))))))

  (define (expand-vec-addition n t lhs rhs)
    (let ((l (gensym 'lhs))
          (r (gensym 'rhs))
          (len (gensym 'len))
          (i (gensym 'i))
          (res (gensym 'res))
          (lhsi (gensym 'lhsi))
          (rhsi (gensym 'rhsi)))
      `(let ((,l (vec ,n ,t) ,lhs)
             (,r (vec ,n ,t) ,rhs))
         (let ((,len int (length (var (vec ,n ,t) ,l))))
           (let ((,res (vec ,n ,t) (make-vector ,t (int ,n))))
             (begin
               (for (,i (int 0) (var int ,len))
                 (let ((,lhsi ,t
                         (vector-ref ,t (var (vec ,n ,t) ,l)
                           (var int ,i)))
                       (,rhsi ,t
                         (vector-ref ,t (var (vec ,n ,t) ,r)
                           (var int ,i))))
                   (vector-set! ,t (var (vec ,n ,t) ,res)
                     (var int ,i)
                     ,(expand-prim-expr
                        `(+ ,t (var ,t ,lhsi) (var ,t ,rhsi))))))
               (var (vec ,n ,t) ,res)))))))

  (define (expand-vec-comparison n t lhs rhs)
    (let ((l (gensym 'lhs))
          (r (gensym 'rhs))
          (len (gensym 'len))
          (i (gensym 'i))
          (res (gensym 'res))
          (lhsi (gensym 'lhsi))
          (rhsi (gensym 'rhsi)))
      `(let ((,l (vec ,n ,t) ,lhs)
             (,r (vec ,n ,t) ,rhs))
         (let ((,len int (length (var (vec ,n ,t) ,l)))
               (,res bool (bool #t)))
           (begin
             (if (= (var int ,len)
                   (length (var (vec ,n ,t) ,r)))
                 (for (,i (int 0) (var int ,len))
                   (let ((,lhsi ,t
                           (vector-ref ,t (var (vec ,n ,t) ,l)
                             (var int ,i)))
                         (,rhsi ,t
                           (vector-ref ,t (var (vec ,n ,t) ,r)
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
