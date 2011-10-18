(library
  (kernels)
  (export
    compile-kernels
    compile-kernel
    hoist-kernels
    move-gpu-data
    generate-kernel-calls
    verify-compile-kernels
    verify-hoist-kernels
    verify-move-gpu-data
    verify-generate-kernel-calls)
  (import
    (only (chezscheme) format)
    (rnrs)
    (util match)
    (print-c)
    (verify-grammar)
    (util helpers)
    (harlan generate-kernel-calls))
  
(generate-verify generate-kernel-calls
  (Module wildcard))

(generate-verify move-gpu-data
  (Module wildcard))
(define move-gpu-data (lambda (expr) expr))

(define format-kernel-arg
  (lambda (arg)
    (format-arg arg)))

(generate-verify compile-kernels
  (Module wildcard)
  (Kernel
    (kernel Var (Var *) Stmt *))
  (Var symbol)
  (Stmt wildcard))

(define compile-kernel
  (lambda (kernel)
    (match kernel
      ((kernel ,name (,[format-kernel-arg -> args*] ...)
         . ,stmts)
       (string-append
         "__kernel void " (format-ident name)
         "(" (join ", " args*) ") "
         (format-block `(block . ,stmts))))
      (,else
        (error 'compile-kernel (format "bad kernel expression: ~s" kernel))))))

(define (unpack-type x)
  (match x
    ((var (vector ,t ,n) ,x^) t)
    (,else (error 'unpack-type "invalid kernel argument" else))))

(define compile-kernels
  (lambda (mod)
    (map (lambda (decl)
           (match decl
             ((gpu-module ,[compile-kernel -> kernel*] ...)
              `(global cl::program g_prog
                 ((field g_ctx createProgramFromSource)
                  ,(join "\n" kernel*))))
             ((func ,t ,x ,args
                    ,stmt* ...)
              `(func ,t ,x ,args ,stmt* ...))
             (,else else)))
      mod)))

(generate-verify hoist-kernels
  (Module (module Decl *))
  (Kernel wildcard)
  (Decl
   (gpu-module Kernel *)
   (fn Var (Var *) Type Stmt * Ret-Stmt)
   (extern Var (Type *) -> Type))
  (Expr wildcard)
  (Stmt wildcard)
  (Ret-Stmt (return Expr))
  (Var symbol)
  (Type wildcard))

;; This pass is probably too big. It finds all the kernel
;; expressions, hoists them into a GPU module, replaces the old
;; expression with an apply-kernel block, and rewrites all the
;; iterator variables in the body.
(define hoist-kernels
  (lambda (mod)
    ;; (display "hoisting kernels\n")
    (match mod
      ((module ,[hoist-decl -> decl* kernel*] ...)
       `(module
          (gpu-module . ,(apply append kernel*))
          ,decl* ...))
      (,else (error 'hoist-kernels "What is this?" else)))))

(define hoist-decl
  (lambda (decl)
    (match decl
      ((fn ,name ,args ,type ,[hoist-stmt -> stmt* kernel*] ...)
       (values
        (if (and (eq? name 'main) (not (null? (apply append kernel*))))
            `(fn ,name ,args ,type (do (call void GC_INIT) 
                                       (call void
                                             (field (var cl::program g_prog)
                                                    build))) 
                 ,stmt* ...)
            `(fn ,name ,args ,type (do (call void GC_INIT)) ,stmt* ...))
        (apply append kernel*)))
      ((extern ,name ,arg-types -> ,t)
       (values `(extern ,name ,arg-types -> ,t) '()))
      (,else (error 'hoist-decl "Invalid declaration" else)))))

(define hoist-stmt
  (lambda (stmt)
    (match stmt
      ;; TODO: kernels are actually an expression form...
      ((kernel (((,x* ,t*) (,xs* ,ts*)) ...)
         ;; TODO: correctly handle free variables.
         (free-vars (,fv* ,ft*) ...)
         ;; TODO: What if this introduces free variables? What
         ;; about free variables in general?
         ,[hoist-stmt -> stmt* kernel*] ...)
       (let ((k-name (gensym 'kernel)))
         (values
           `(apply-kernel ,k-name ,xs* ...
              ,@(map (lambda (x t)
                       `(var ,t ,x))
                  fv* ft*))
           (cons (generate-kernel k-name x* t*
                   (map (lambda (xs) (gensym 'k_arg)) xs*)
                   ts* fv* ft* stmt*)
             (apply append kernel*)))))
      ((for (,i ,start ,end) ,[hoist-stmt -> stmt* kernel*] ...)
; WEB: have no idea if this is right.
       (values `(for (,i ,start ,end) ,stmt* ...) (apply append kernel*)))
      (,else (values else '())))))

(define generate-kernel
  (lambda (name x* t* xs* ts* fv* ft* stmt*)
    ;; Plan of attack: replace all vectors with renamed char *'s,
    ;; then immediate use vec_deserialize. Also, for some reason the
    ;; vector refs don't seem to be being lowered like they should
    ;; be.
    ;;
    ;; We can also let-bind vars to the cell we care about, then
    ;; replace everything with a deref. That'll be cleaner.
    (let ((i (gensym 'i)))
      ;; TODO: Correctly handle free vars
      `(kernel ,name ,(append (map (lambda (x t)
                                     `(,x (ptr ,t)))
                                   xs* t*)
                              (map list fv* ft*))
         ;; TODO: allow this to work on n-dimensional vectors.
         (let ,i int (call int get_global_id (int 0)))
         ,@(apply
             append
             (map
               (lambda (x t xs ts)
                 `((let ,x (ptr ,t)
                        (addressof (vector-ref
                                    ,t (var ,ts ,xs) (var int ,i))))))
               x* t* xs* ts*))
         . ,(replace-vec-refs stmt* i x* xs* ts*)))))

(define replace-vec-refs
  (lambda (stmt* i x* xs* ts*)
    (map (lambda (stmt)
           (fold-left 
             (lambda (stmt x xs ts)
               (let ((t (match ts
                          ((vector ,t ,n) t)
                          ;; TODO: ptrs shouldn't be in the language
                          ;; yet, but we're being a bit too aggressive
                          ;; about erasing types. Once we move
                          ;; convert-types below this pass, we should
                          ;; remove this clause too.
                          ((ptr ,t) t)
                          (,else (error 'replace-vec-refs
                                        "Unknown type"
                                        else)))))
                 (match stmt
                   ((var ,t ,y) (guard (eq? x y)) `(deref (var ,t ,x)))
                   ((,[x] ...) `(,x ...))
                   (,x x))))
             stmt x* xs* ts*))
      stmt*)))

;; end library
)
