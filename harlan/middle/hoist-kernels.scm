(library
  (harlan middle hoist-kernels)
  (export hoist-kernels)
  (import
    (only (chezscheme) format)
    (rnrs)
    (util match)
    (util helpers))

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
       (let ((kernel* (apply append kernel*)))
         (values
           (if (and (eq? name 'main)
                    (not (null? kernel*)))
               `(fn ,name ,args ,type
                  (do (call void GC_INIT) 
                      (call void
                        (field (var cl::program g_prog)
                          build))) 
                  ,stmt* ...)
               `(fn ,name ,args ,type
                  (do (call void GC_INIT)) ,stmt* ...))
           kernel*)))
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
       ;; WEB: have no idea if this is right.
       (values `(for (,i ,start ,end) ,stmt* ...)
         (apply append kernel*)))
      (,else (values else '())))))

(define generate-kernel
  (lambda (name x* t* xs* ts* fv* ft* stmt*)
    ;; Plan of attack: replace all vectors with renamed char *'s,
    ;; then immediate use vec_deserialize. Also, for some reason
    ;; the vector refs don't seem to be being lowered like they
    ;; should be.
    ;;
    ;; We can also let-bind vars to the cell we care about, then
    ;; replace everything with a deref. That'll be cleaner.
    (let ((i (gensym 'i)))
      ;; TODO: Correctly handle free vars
      `(kernel ,name ,(append (map list xs* ts*)
                        (map list fv* ft*))
         ;; TODO: allow this to work on n-dimensional vectors.
         (let ,i int (call int get_global_id (int 0)))
         ,@(apply
             append
             (map
               (lambda (x t xs ts)
                 `((let ,x (ptr ,t)
                        (addressof
                          (vector-ref
                            ,t (var ,ts ,xs) (var int ,i))))))
               x* t* xs* ts*))
         . ,(replace-vec-refs stmt* i x* xs* ts*)))))

(define replace-vec-refs
  (lambda (stmt* i x* xs* ts*)
    (map
      (lambda (stmt)
        (fold-left 
          (lambda (stmt x xs ts)
            (let ((t (match ts
                       ((vector ,t ,n) t)
                       (,else (error 'replace-vec-refs
                                "Unknown type"
                                else)))))
                 (match stmt
                   ((var ,t ,y) (guard (eq? x y))
                    `(deref (var ,t ,x)))
                   ((,[x*] ...) x*)
                   (,x x))))
             stmt x* xs* ts*))
      stmt*)))

;; end library
)
