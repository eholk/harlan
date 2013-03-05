(library
  (harlan middle remove-danger)
  (export remove-danger)
  (import
   (rnrs)
   (harlan helpers)
   (except (elegant-weapons helpers) ident?))

  ;; This pass inserts vector bounds checks. So far we only support
  ;; this and allocation failures. The checking code for allocation
  ;; failures is inserted later, in uglify-vectors.
  ;;
  ;; The way we handle errors in kernels is going to drive us
  ;; completely insane someday, so here is a note to my future self to
  ;; hopefully lessen the pain. We support two kinds of errors, bounds
  ;; checks and allocation failures. To handle these, we add a
  ;; parameter to the kernel called the danger vector. Every
  ;; successful kernel thread should set their entry in the danger
  ;; vector to 0. If an error is detected, they fill the danger vector
  ;; with the correct error code and then return. The host code will
  ;; then iterate over the danger vector to make sure all the kernels
  ;; completed successfully.
  ;;
  ;; Now, you're probably wondering where all this code gets
  ;; generated. Well, the pass your looking at now inserts the array
  ;; bounds checks. The returnify-kernels pass is responsible for
  ;; adding the danger vector (errors are just a specially kind of
  ;; return, if you squint right). It gets better though. The
  ;; returnify-kernels pass also walks the whole AST and replaces all
  ;; the calls to (error ...) with code to fill the danger vector and
  ;; return. In this case, we've only ever generated array bounds
  ;; checks, so we just assume all of the error forms are bounds check
  ;; errors. So far so good.
  ;;
  ;; Next, we need to deal with the allocation errors. The types and
  ;; grammars are tricky to do this early on, and plus, we have lots
  ;; of allocating forms. Instead, we add the checks in
  ;; uglify-vectors. Basically, if alloc returns 0, there was a
  ;; failure to allocate. This generates new error forms, which won't
  ;; get erased from kernels in returnify-kernels like the ones from
  ;; remove-danger because returnify-kernels has already
  ;; won. Furthermore, by the time we get to uglify-vectors, it's hard
  ;; to figure out where the danger vector is. Instead we just punt
  ;; and let the error forms go to the end. Sadly, the expand into a
  ;; call to harlan_error, which OpenCL has no idea what to do with.
  ;;
  ;; But now, gpu_only.h has a macro called harlan_error which sets
  ;; the danger vector and returns. Since we know the only error forms
  ;; that made it that far are allocation failures, we set the danger
  ;; vector with the allocation failure code. But wait, didn't we just
  ;; say it's hard to find the danger vector after returnify-kernels?
  ;; Yes, we did. Well, the dirty little secret is that we decided to
  ;; always call it `danger` so the macro can find it.
  ;;
  ;; This is software engineering at its finest.

  
  (define-match remove-danger
    ((module ,[Decl -> decl*] ...)
     `(module ,decl* ...)))
    
  (define-match Decl
    ((fn ,name ,args ,t ,[Stmt -> stmt])
     `(fn ,name ,args ,t ,stmt))
    ((typedef ,name ,t) `(typedef ,name ,t))
    ((extern ,name ,args -> ,rtype)
     `(extern ,name ,args -> ,rtype)))

  (define-match Stmt
    ((let ((,x* ,t* ,[Expr -> e*]) ...) ,[body])
     `(let ((,x* ,t* ,e*) ...) ,body))
    ((let-region (,r ...) ,[body]) `(let-region (,r ...) ,body))
    ((set! ,[Expr -> lhs] ,[Expr -> rhs])
     `(set! ,lhs ,rhs))
    ((if ,[Expr -> test] ,[conseq] ,[altern])
     `(if ,test ,conseq ,altern))
    ((if ,[Expr -> test] ,[conseq])
     `(if ,test ,conseq))
    ((while ,[Expr -> test] ,[body])
     `(while ,test ,body))
    ((for (,x ,[Expr -> start] ,[Expr -> stop] ,[Expr -> step]) ,[body])
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
    ((do ,[Expr -> e])
     `(do ,e)))

  (define-match Expr
    ((,t ,v) (guard (scalar-type? t)) `(,t ,v))
    ((var ,t ,x) `(var ,t ,x))
    ((int->float ,[e]) `(int->float ,e))
    ((make-vector ,t ,r ,[e])
     `(make-vector ,t ,r ,e))
    ((vector-ref ,t ,[v] ,[i])
     (let ((v-var (gensym 'vec))
           (i-var (gensym 'refindex))
           (vt    (type-of v)))
       `(let ((,v-var ,vt ,v)
              (,i-var int ,i))
          (begin
            (if (>= (var int ,i-var) (length (var ,vt ,v-var)))
                (error ,(gensym 'vector-length-error)))
            (vector-ref ,t (var ,vt ,v-var) (var int ,i-var))))))
    ((length ,[e])
     `(length ,e))
    ((vector ,t ,r ,[e*] ...)
     `(vector ,t ,r . ,e*))
    ((call ,[f] ,[args] ...)
     `(call ,f . ,args))
    ((if ,[test] ,[conseq] ,[altern])
     `(if ,test ,conseq ,altern))
    ((kernel
      (vec ,r ,inner-type)
      ,r
      (,[dim] ...)
      (((,x* ,t*) (,[xs*] ,ts*) ,d*) ...)
      ,[body])
     `(kernel
       (vec ,r ,inner-type)
       ,r
       ,dim
       (((,x* ,t*) (,xs* ,ts*) ,d*) ...) ,body))
    ((let ((,x* ,t* ,[e*]) ...) ,[e])
     `(let ((,x* ,t* ,e*) ...) ,e))
    ((begin ,[Stmt -> s*] ... ,[e])
     `(begin ,s* ... ,e))
    ((c-expr . ,whatever) `(c-expr . ,whatever))
    ((field ,[e] ,x) `(field ,e ,x))
    ((,op ,[lhs] ,[rhs])
     (guard (or (relop? op) (binop? op)))
     `(,op ,lhs ,rhs)))

  (define-match type-of
    ((var ,t ,_) t)
    ((vector-ref ,t ,v ,i) t)
    ((let ((,x ,t ,e) ...) ,[b]) b)
    ((begin ,e* ... ,[e]) e))
  
  
  ;; end library
  )
