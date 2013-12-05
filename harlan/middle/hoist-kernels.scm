(library
  (harlan middle hoist-kernels)
  (export hoist-kernels)
  (import
   (rnrs)
   (except (elegant-weapons helpers) ident?)
   (elegant-weapons sets)
   (harlan helpers))

(define-match hoist-kernels
  ((module ,[hoist-decl -> decl* kernel*] ...)
   (let* ((kernel* (apply append kernel*))
          (syms (gather-symbols (apply union (map decl-symbols kernel*))
                                decl*))
          (gpu-decls ((extract-symbols syms) decl*)))
     `(module
        (gpu-module ,@gpu-decls . ,kernel*)
        . ,decl*))))

(define-match type-symbols
  ((fn (,[t] ...) -> ,[t^]) (union t^ (apply union t)))
  ((adt ,t . ,_) (list t))
  ((vec ,[t]) t)
  ((ptr ,[t]) t)
  ((struct (,name ,[type-symbols -> t]) ...)
   (apply union t))
  ((union (,name ,[type-symbols -> t]) ...)
   (apply union t))
  ((box ,r ,[t]) t)
  (,x (guard (symbol? x)) (list x)))

(define-match stmt-symbols
  ((let ,x ,[type-symbols -> t] ,[expr-symbols -> e])
   (union t e))
  ((let ,x ,[type-symbols -> t]) t)
  ((if ,[expr-symbols -> t] ,[c] ,[a]) (union t c a))
  ((if ,[expr-symbols -> t] ,[c]) (union t c))
  ((set! ,[expr-symbols -> e] ,[expr-symbols -> v])
   (union e v))
  ((for (,i ,[expr-symbols -> start]
            ,[expr-symbols -> stop]
            ,[expr-symbols -> step])
     ,[s])
   (union start stop step s))
  ((while ,[expr-symbols -> t] ,[s])
   (union t s))
  ((return) '())
  ((error ,x) '())
  ((return ,[expr-symbols -> e]) e)
  ((print ,[expr-symbols -> e]) e)
  ((do ,[expr-symbols -> e]) e)
  ((assert ,[expr-symbols -> e]) e)
  ((begin ,[s] ...) (apply union s)))

(define-match expr-symbols
  ((int ,n) '())
  ((bool ,b) '())
  ((float ,f) '())
  ((str ,s) '())
  ((var ,[type-symbols -> t] ,x) (set-add t x))
  ((deref ,[e]) e)
  ((call ,[e] ...) (apply union e))
  ((c-expr ,[type-symbols -> t] ,x) (set-add t x))
  ((field ,[e] ,x) e)
  ((addressof ,[e]) e)
  ((vector-ref ,[type-symbols -> t] ,[v] ,[i])
   (union t v i))
  ((region-ref ,[type-symbols -> t] ,[r] ,[i])
   (union t r i))
  ((sizeof ,[type-symbols -> t]) t)
  ((cast ,[type-symbols -> t] ,[e]) (union t e))
  ((empty-struct) '())
  ((alloc ,[r] ,[s]) (union r s))
  ((not ,[e]) e)
  ((,op ,[a] ,[b]) (guard (or (binop? op) (relop? op)))
   (union a b)))

(define-match decl-symbols
  ((typedef ,name ,[type-symbols -> t]) t)
  ((fn ,name ,args ,[type-symbols -> t] ,[stmt-symbols -> b])
   (union t b))
  ((kernel ,name ,args ,[stmt-symbols -> stmt]) stmt))

(define (gather-symbols syms decls)
  ;; TODO: this really needs to iterate to a fix point
  (let loop ((syms syms))
    (let ((syms^
           ((filter-decls syms
                          (lambda (d rest)
                            (union (decl-symbols d) rest))
                          syms)
            decls)))
      (if (set-equal? syms syms^)
          syms
          (loop syms^)))))
  
;; Extracts the symbol definitions from a set of decls
(define-match (filter-decls syms f zero)
  (() zero)
  (((fn ,name . ,_) . ,[rest])
   (guard (member name syms))
   (f `(fn ,name . ,_) rest))
  (((typedef ,name . ,_) . ,[rest])
   (guard (member name syms))
   (f `(typedef ,name . ,_) rest))
  (((extern ,name . ,_) . ,[rest])
   (guard (member name syms))
   (f `(extern ,name . ,_) rest))
  ((,a . ,[d]) d))

(define (extract-symbols syms)
  (filter-decls syms cons '()))

(define-match hoist-decl
  ((fn ,name ,args ,type ,[hoist-stmt -> stmt kernel*])
   (values `(fn ,name ,args ,type ,stmt) kernel*))
  ((extern ,name ,arg-types -> ,t)
   (values `(extern ,name ,arg-types -> ,t) '()))
  ((typedef ,name ,t)
   (values `(typedef ,name ,t) '()))
  ((global ,type ,name ,e)
   (values `(global ,type ,name ,e) '())))

(define-match hoist-stmt
  ((kernel ,dims (free-vars (,fv* ,ft*) ...)
     ,[hoist-stmt -> stmt kernel*])
   (let ((name (gensym 'kernel)))
     (let-values (((fv*^ ft*^ casts)
                   (regionptr->voidptr fv* ft*)))
       (values
        `(apply-kernel ,name
                       ,dims
                       ,@(map (lambda (t x) `(var ,t ,x)) ft* fv*))
        `((kernel ,name
                  ,(map list fv*^ ft*^)
                  (begin ,@casts ,stmt))
          . ,kernel*)))))
  ((begin ,[hoist-stmt -> stmt* kernel*] ...)
   (values (make-begin stmt*) (apply append kernel*)))
  ((for (,i ,start ,end ,step) ,[hoist-stmt -> stmt kernel*])
   (values `(for (,i ,start ,end ,step) ,stmt) kernel*))
  ((while ,expr ,[hoist-stmt -> stmt kernel*])
   (values `(while ,expr ,stmt) kernel*))
  ((if ,test ,[hoist-stmt -> conseq ckernel*] ,[hoist-stmt -> alt akernel*])
   (values `(if ,test ,conseq ,alt) (append ckernel* akernel*)))
  (,else (values else '())))

(define (regionptr->voidptr fv* ft*)
  (match (map cons fv* ft*)
    (() (values `() `() `()))
    (((,x . (ptr region)) . ,[fv^ ft^ casts])
     (let ((void-region (gensym x)))
       (values
        (cons void-region fv^)
        (cons `(ptr void) ft^)
        (cons `(let ,x (ptr region)
                 (cast (ptr region)
                       (var (ptr void) ,void-region)))
              casts))))
    (((,x . ,t) . ,[fv^ ft^ casts])
     (values (cons x fv^) (cons t ft^) casts))
    (,else (error 'regionptr->voidptr "unmatched datum" else))))

;; end library
)
