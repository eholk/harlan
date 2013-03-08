(library
    (harlan middle desugar-match)

  (export desugar-match)
  (import
   (rnrs)
   (only (chezscheme) trace-define)
   (elegant-weapons match)
   (elegant-weapons helpers))

  (define (make-anonymous-struct types)
    (let ((types
           ;; NVidia OpenCL doesn't like empty structs, so create a
           ;; dummy field in that case.
           (if (null? types)
               '(int)
               types)))
      (cons 'struct
            (let loop ((i 0)
                       (types types))
              (if (null? types)
                  '()
                  (cons (list (string->symbol
                               (string-append "f" (number->string i)))
                              (car types))
                        (loop (+ 1 i) (cdr types))))))))
    
  (define (make-named-union names types)
    (cons 'union
          (map list names types)))

  (define (make-constructor typedef)
    (let ((name (car typedef)))
      (lambda (tag types)
        (let ((args (map (lambda (_) (gensym tag)) types))
              (tmp (gensym 'result)))
          `(fn ,tag ,args (,types -> ,name)
               (let ((,tmp ,name (empty-struct)))
                 (begin
                   ,@(let* ((id (tag-id tag typedef))
                            (t* (list-ref (cdr typedef) id)))
                       (match t*
                         ((,tag . ,t*)
                          (let loop ((j 0)
                                     (t* t*)
                                     (x* args))
                            (match `(,x* ,t*)
                              (((,x . ,x*) (,t . ,t*))
                               (cons
                                `(set! (field
                                        (field (field (var ,name ,tmp) data)
                                               ,tag)
                                        ,(string->symbol
                                          (string-append "f"
                                                         (number->string j))))
                                       (var ,t ,x))
                                (loop (+ 1 j) t* x*)))
                              ((() ())
                               `((set! (field (var ,name ,tmp) tag)
                                       (int ,id)))))))))
                   (return (var ,name ,tmp)))))))))
    
  (define-match desugar-match
    ((module . ,decls)
     (let ((typedefs (map cdr (filter (lambda (d)
                                        (eq? (car d) 'define-datatype))
                                      decls))))

       (define-match desugar-decl
         ((define-datatype ,name (,tag ,type ...) ...)
          `((typedef ,name
                     (struct
                      (tag cl_int)
                      (data ,(make-named-union
                              tag
                              (map make-anonymous-struct
                                   type)))))
            . ,(map (make-constructor `(,name (,tag ,type ...) ...))
                    tag type)))
         ((extern . ,whatever)
          `((extern . ,whatever)))
         ((fn ,name ,args ,type ,[desugar-stmt -> body])
          `((fn ,name ,args ,type ,body))))
       
       (define-match desugar-stmt
         ((let-region ,r ,[s])
          `(let-region ,r ,s))
         ((begin ,[s] ...) `(begin ,s ...))
         ((let ((,x ,t ,[desugar-expr -> e]) ...) ,[b])
          `(let ((,x ,t ,e) ...) ,b))
         ((do ,[desugar-expr -> e]) `(do ,e))
         ((print ,[desugar-expr -> e]) `(print ,e))
         ((print ,[desugar-expr -> e] ,[desugar-expr -> o])
          `(print ,e ,o))
         ((if ,[desugar-expr -> t] ,c ,a) `(if ,t ,c ,a))
         ((if ,[desugar-expr -> t] ,c) `(if ,t ,c))
         ((for (,i ,[desugar-expr -> start]
                   ,[desugar-expr -> stop]
                   ,[desugar-expr -> step])
            ,[s])
          `(for (,i ,start ,stop ,step) ,s))
         ((while ,[desugar-expr -> t] ,[s])
          `(while ,t ,s))
         ((set! ,[desugar-expr -> e] ,[desugar-expr -> v])
          `(set! ,e ,v))
         ((assert ,[desugar-expr -> e]) `(assert ,e))
         ((return) '(return))
         ((return ,[desugar-expr -> e])
          `(return ,e)))
       
       (define-match desugar-expr
         ((int ,n) `(int ,n))
         ((str ,s) `(str ,s))
         ((float ,f) `(float ,f))
         ((bool ,b) `(bool ,b))
         ((var ,t ,x) `(var ,t ,x))
         ((char ,c) `(char ,c))
         ((int->float ,[i]) `(int->float ,i))
         ((begin ,[desugar-stmt -> s] ... ,[e])
          `(begin ,s ... ,e))
         ((call ,[e*] ...) `(call ,e* ...))
         ((if ,[t] ,[c] ,[a]) `(if ,t ,c ,a))
         ((,op ,[a] ,[b]) (guard (or (binop? op) (relop? op)))
          `(,op ,a ,b))
         ((vector-ref ,t ,[e] ,[i])
          `(vector-ref ,t ,e ,i))
         ((length ,[e]) `(length ,e))
         ((iota-r ,r ,[e]) `(iota-r ,r ,e))
         ((vector ,t ,r ,[e] ...) `(vector ,t ,r ,e ...))
         ((kernel ,t ,r (((,x ,t*) (,[xs] ,ts)) ...) ,[e])
          `(kernel ,t ,r (((,x ,t*) (,xs ,ts)) ...) ,e))
         ((let ((,x ,t ,[e]) ...) ,[b])
          `(let ((,x ,t ,e) ...) ,b))
         ((make-vector ,t ,r ,[e]) `(make-vector ,t ,r ,e))
         
         ((match ,t ,[e]
                 ((,tag ,x ...) ,[e*]) ...)
          (let* ((tag-var (gensym 'tag))
                 (e-var (gensym 'm))
                 (tag-type (type-of e))
                 (typedef (assq (match tag-type
                                  ((adt ,t . ,_) t))
                                typedefs)))
            `(let ((,e-var ,tag-type ,e))
               (let ((,tag-var int (call (c-expr
                                          ((,tag-type) -> int) extract_tag)
                                         (var ,tag-type ,e-var))))
                 ,(let loop ((tag tag)
                             (x x)
                             (e e*))
                    (match `(,tag ,x ,e)
                      (((,tag) (,x) (,e))
                       `(let ,(bind-fields tag x
                                           `(var ,tag-type ,e-var)
                                           typedef)
                          ,e))
                      (((,tag . ,tag*)
                        (,x . ,x*)
                        (,e . ,e*))
                       `(if (= (var int ,tag-var) (int ,(tag-id tag typedef)))
                            (let ,(bind-fields tag x
                                               `(var ,tag-type ,e-var)
                                               typedef)
                              ,e)
                            ,(loop tag* x* e*)))
                      (,else (error 'match-loop "unrecognized" else)))))))))
       
       `(module . ,(apply append (map desugar-decl decls))))))
  
  (define (bind-fields tag x e typedef)
    (let* ((id (tag-id tag typedef))
           (t* (list-ref (cdr typedef) id)))
      (match t*
        ((,tag . ,t*)
         (let loop ((j 0)
                    (t* t*)
                    (x* x))
           (match `(,x* ,t*)
             (((,x . ,x*) (,t . ,t*))
              (cons `(,x ,t (field (field (field ,e data) ,tag)
                                   ,(string->symbol
                                     (string-append "f" (number->string j)))))
                    (loop (+ 1 j) t* x*)))
             ((() ()) '())))))))
                                                        
  
  (define (tag-id tag typedef)
    (let loop ((id 0)
               (typedef (cdr typedef)))
      (cond
        ((null? typedef) #f)
        ((eq? tag (caar typedef))
         id)
        (else (loop (+ id 1) (cdr typedef))))))

  (define-match type-of
    ((call ,[f] . ,_)
     (match f
       ((,_ -> ,t) t)
       (,else (error 'type-of "Illegal function type" else))))
    ((var ,t ,x) t))
  
  )
