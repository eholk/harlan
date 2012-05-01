(library
  (harlan middle optimize-fuse-kernels)
  (export optimize-fuse-kernels
    make-2d-kernel
    shares-dimension
    build-switch)
  (import
   (rnrs)
   (harlan helpers)
   (only (harlan verification-passes)
         verify-make-kernel-dimensions-explicit)
   (harlan compile-opts)
   (elegant-weapons helpers)
   (only (chezscheme) trace-define trace-lambda))

  (define-match optimize-fuse-kernels
    ((module ,[Decl -> decl*] ...)
     `(module ,decl* ...)))
    
  (define-match Decl
    ((fn ,name ,args ,t ,[Stmt -> stmt])
     `(fn ,name ,args ,t ,stmt))
    ((extern ,name ,args -> ,rtype)
     `(extern ,name ,args -> ,rtype)))

  (define-match Stmt
    ((let ((,x* ,t* ,[Expr -> e*]) ...) ,[body])
     `(let ((,x* ,t* ,e*) ...) ,body))
    ((set! ,[Expr -> lhs] ,[Expr -> rhs])
     `(set! ,lhs ,rhs))
    ((if ,[Expr -> test] ,[conseq] ,[altern])
     `(if ,test ,conseq ,altern))
    ((if ,[Expr -> test] ,[conseq])
     `(if ,test ,conseq))
    ((while ,[Expr -> test] ,[body])
     `(while ,test ,body))
    ((for (,x ,[Expr -> start]
              ,[Expr -> stop]
              ,[Expr -> step])
          ,[body])
     `(for (,x ,start ,stop ,step) ,body))
    ((begin ,[stmt*] ...)
     `(begin ,stmt* ...))
    ((print ,[Expr -> e] ...)
     `(print . ,e))
    ((assert ,[Expr -> e])
     `(assert ,e))
    ((return) `(return))
    ((return ,[Expr -> e])
     `(return ,e))
    ((error ,x) `(error ,x))
    ((do ,[Expr -> e])
     `(do ,e)))

  (define-match Expr
    ((,t ,v) (guard (scalar-type? t)) `(,t ,v))
    ((var ,t ,x) `(var ,t ,x))
    ((int->float ,[e]) `(int->float ,e))
    ((make-vector ,t ,[e])
     `(make-vector ,t ,e))
    ((c-expr ,t ,v)
     `(c-expr ,t ,v))
    ((vector-ref ,t ,[v] ,[i])
     `(vector-ref ,t ,v ,i))
    ((length ,[e])
     `(length ,e))
    ((call ,[f] ,[args] ...)
     `(call ,f ,args ...))
    ((if ,[test] ,[conseq] ,[altern])
     `(if ,test ,conseq ,altern))
    ((if ,[test] ,[conseq])
     `(if ,test ,conseq))
    ((kernel ,t ,dims ,iters ,[body])
     (make-2d-kernel t dims iters body))
    ((let ((,x* ,t* ,[e*]) ...) ,[e])
     `(let ((,x* ,t* ,e*) ...) ,e))
    ((begin ,[Stmt -> s*] ... ,[e])
     `(begin ,s* ... ,e))
    ((,op ,[lhs] ,[rhs])
     (guard (or (relop? op) (binop? op)))
     `(,op ,lhs ,rhs)))

  (define (inline-kernel t dims iters body)
    (match iters
      ((((,x ,xt)
         ((kernel ,t^ ,dims^ ,iters^ ,body^)
          ,et)
         ,i)
        . ,rest)
       ;; Super kernel!
       (begin
         (if (verbose)
             (begin
               (display "Harlan Compiler Message:")
               (display "optimize-fuse-kernels is inlining a kernel\n")))
         (Expr
          `(kernel ,t ,dims
                   (,@rest . ,iters^)
                   (let ((,x ,xt ,body^))
                     ,body)))))
      (,else `(kernel ,t ,dims ,iters ,body))))

  (define (make-2d-kernel t dims iters body)
    (let ((arg-vars (map caar iters))
          (fallback (lambda () (inline-kernel t dims iters body))))
      (match body
        ((kernel ,t^ ,dims^ ,iters^ ,body^)
         (guard (and ((not-in arg-vars) dims^)    ;; this should be (not (in ...))
                     ((not-in arg-vars) iters^)))
         (Expr
          `(kernel
            ,t
            (,@dims ,@dims^)
            (,@iters
             ,@(map incr-dimension iters^))
            ,(incr-dim-expr body^))))
        ((kernel ,t^ ,dims^ ,iters^ ,body^)
         (cond
          ((shares-dimension iters iters^)
           =>
           (lambda (varxs)
             (build-switch
              (map caddr varxs)
              varxs
              t
              dims dims^
              iters iters^
              body^
              (fallback))))
          (else (fallback))))
        (,else (fallback)))))

  (define (shares-dimension iters iters^)
    (let ((ls (map (lambda (iter) (cons (caar iter) (caadr iter))) iters)))
      (match iters^
        (() #f)
        ((((,x ,xt) ((var ,et ,e) ,et) ,d) . ,[rest])
         (cond
          ((assq e ls)
           => (lambda (p) (if rest (set-add/var rest (cdr p)) `(,(cdr p)))))
          (else rest)))
        ((((,x ,xt) (,e ,et) ,d) . ,[rest]) rest))))

  (define (build-switch
           xs varxs t dims dims^ iters iters^ body^ oldkernel)
    (let ((lenxs (map (lambda (x) (gensym (symbol-append 'len x))) xs))
          (lenrows (map (lambda (x) (gensym (symbol-append 'lenrow x))) xs))
          (is (map (lambda (_) (gensym 'i)) varxs))
          (res (gensym 'res)))
      `(let ((,res bool (bool #t))
             ,@(map
                (lambda (lenx varx) `(,lenx int (length ,varx)))
                lenxs varxs)
             ,@(map
                (lambda (lenrow varx)
                  `(,lenrow int (length (vector-ref ,(cadadr varx) ,varx (int 0)))))
                lenrows varxs))
         (begin
           ,@(map
              (lambda (i lenx lenrow varx)
                `(if (var bool ,res) 
                     (for (,i (int 1) (var int ,lenx) (int 1))
                          (if (= (= (var int ,lenrow)
                                    (length (vector-ref ,(cadadr varx) ,varx (var int ,i))))
                                 (bool #f))
                              (begin
                                (set! (var bool ,res) (bool #f))
                                (set! (var int ,i) (var int ,lenx)))))))
              is lenxs lenrows varxs)
           (if (var bool ,res)
               (begin
                 ,@(if (verbose)
                       `((print (str "Rectangular argument, took branch to 2D-erize a kernel\n")))
                       `())
                 (kernel
                  ,t
                  (,@dims
                   (length (vector-ref ,(cadadr (car varxs)) ,(car varxs) (int 0))))
                  (,@iters
                   ,@(map incr-dimension iters^))
                  ,(incr-dim-expr body^)))
               (begin
                 ,@(if (verbose)
                       `((print (str "Non-recangular argument, took branch to run nested kernel\n")))
                       `())
                 ,oldkernel))))))
  
  (define-match (not-in argv)
    ((var ,t ,x) (not (memq x argv)))
    ((,x* ...)
     (andmap (not-in argv) x*))
    (,x #t))

  (define (make-env iters)
    (match iters
      (() `())
      ((((,x ,xt) (,xs ,ts) ,d) . ,[env])
       (cons `(,x . ,xs) env))))

  (define (incr-dimension iter)
    (match iter
      ((,arg ,exp ,dim)
       `(,arg ,exp ,(+ dim 1)))))

  (define-match incr-dim-stmt
    ((let ((,x* ,t* ,[incr-dim-expr -> e*]) ...) ,[body])
     `(let ((,x* ,t* ,e*) ...) ,body))
    ((set! ,[incr-dim-expr -> lhs] ,[incr-dim-expr -> rhs])
     `(set! ,lhs ,rhs))
    ((if ,[incr-dim-expr -> test] ,[conseq] ,[altern])
     `(if ,test ,conseq ,altern))
    ((if ,[incr-dim-expr -> test] ,[conseq])
     `(if ,test ,conseq))
    ((while ,[incr-dim-expr -> test] ,[body])
     `(while ,test ,body))
    ((for (,x ,[incr-dim-expr -> start]
              ,[incr-dim-expr -> stop]
              ,[incr-dim-expr -> step])
          ,[body])
     `(for (,x ,start ,stop ,step) ,body))
    ((begin ,[stmt*] ...)
     `(begin ,stmt* ...))
    ((print ,[incr-dim-expr -> e] ...)
     `(print . ,e))
    ((assert ,[incr-dim-expr -> e])
     `(assert ,e))
    ((return) `(return))
    ((return ,[incr-dim-expr -> e])
     `(return ,e))
    ((error ,x) `(error ,x))
    ((do ,[incr-dim-expr -> e])
     `(do ,e)))

  (define-match incr-dim-expr
    ((,t ,v) (guard (scalar-type? t)) `(,t ,v))
    ((var ,t ,x) `(var ,t ,x))
    ((int->float ,[e]) `(int->float ,e))
    ((make-vector ,t ,[e])
     `(make-vector ,t ,e))
    ((c-expr ,t ,v)
     `(c-expr ,t ,v))
    ((vector-ref ,t ,[v] ,[i])
     `(vector-ref ,t ,v ,i))
    ((length ,[e])
     `(length ,e))
    ((call (c-expr ,t get_global_id) (int ,n))
     `(call (c-expr ,t get_global_id) (int ,(+ n 1))))
    ((call ,[f] ,[args] ...)
     `(call ,f ,args ...))
    ((if ,[test] ,[conseq] ,[altern])
     `(if ,test ,conseq ,altern))
    ((kernel ,t (,[dims] ...)
             (((,x ,xt) (,[e] ,et) ,d) ...)
             ,[body])
     `(kernel ,t ,dims
              (((,x ,xt) (,e ,et) ,d) ...)
              ,body))
    ((let ((,x* ,t* ,[e*]) ...) ,[e])
     `(let ((,x* ,t* ,e*) ...) ,e))
    ((begin ,[incr-dim-stmt -> s*] ... ,[e])
     `(begin ,s* ... ,e))
    ((,op ,[lhs] ,[rhs])
     (guard (or (relop? op) (binop? op)))
     `(,op ,lhs ,rhs)))


  ;; end library
  )
