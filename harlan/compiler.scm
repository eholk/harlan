(library
 (harlan compiler)
 (export compile-harlan test
         compile-harlan-middle verbose
         compile-harlan-frontend compile-module
         type-of)
 (import (rnrs)
         (only (chezscheme) pretty-print format)
         (util match)
         (util helpers)
         (verification-passes)
         (harlan parser)
         (typecheck)
         (lift-vectors)
         (lower-vectors)
         (uglify-vectors)
         (returnify)
         (returnify-kernels)
         (kernels)
         (harlan convert-types)
         (harlan move-gpu-data)
         (verify-compile-module)
         (annotate-free-vars)
         (print-c))

 (define verbose
   (let ((flag #f))
     (case-lambda
       (() flag)
       ((x) (set! flag x)))))

(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (let* ((expected expected-result)
            (produced tested-expression))
       (if (equal? expected produced)
           (printf "~s works!\n" title)
           (error
             'test
             (format "Failed ~s: ~a\nExpected: ~a\nComputed: ~a"
                     title 'tested-expression expected produced)))))))

(define trace-pass
  (lambda (m pass expr)
    (if (verbose)
        (begin
          (display m) (newline)
          (display "Output:") (newline)
          (let ((expr (pass expr)))
            (pretty-print expr) (newline)
            expr))
        (pass expr))))

(define-syntax passes
  (syntax-rules ()
    ((_ pass-name ...)
     (lambda (expr)
       (let* ((expr (trace-pass (symbol->string 'pass-name) pass-name expr))
              ...)
         expr)))))

(define compile-harlan
  (passes
   compile-harlan-frontend
   compile-harlan-middle))

;; The typical frontend of a compiler. Syntax expansion, typechecking,
;; etc.
(define compile-harlan-frontend
  (passes
    verify-harlan
    parse-harlan
    verify-parse-harlan
    returnify
    verify-returnify
    lift-vectors
    verify-lift-vectors
    typecheck
    verify-typecheck))

;; The "middle end" of a compiler. No one ever knows what's supposed
;; to go here. This goes from TFC to something we can give to print-c.
(define compile-harlan-middle
  (passes
    lower-vectors
    returnify-kernels
    uglify-vectors
    annotate-free-vars
    hoist-kernels
    verify-hoist-kernels
    move-gpu-data
    generate-kernel-calls
    verify-generate-kernel-calls
    compile-module
    verify-compile-module
    convert-types
    compile-kernels
    verify-compile-kernels))

(define compile-module
  (lambda (expr)
    (match expr
      [(module ,[compile-decl -> decl*] ...) decl*]
      [,else (error 'compile-module (format "unknown module type ~s" else))])))

(define compile-decl
  (lambda (decl)
    (match decl
      [(fn ,name ,args (,arg-types -> ,ret-type) ,[compile-stmt -> stmt*] ...)
       `(func ,ret-type ,name ,(map cons arg-types args) ,stmt* ...)]
      [(extern ,name ,arg-types -> ,rtype)
       `(extern ,rtype ,name ,arg-types)]
      [(gpu-module ,[compile-kernel^ -> kernel*] ...)
       `(gpu-module ,kernel* ...)]
      [,else (error 'compile-decl (format "unknown decl type ~s" else))])))

(define compile-stmt
  (lambda (stmt)
    (match stmt
      [(let ,x ,t ,[compile-expr -> e])
       `(let ,x ,t ,e)]
      [(let-gpu ,x ,t)
       `(let ,x (cl::buffer ,(scalar-type t))
             ((field g_ctx createBuffer ,(scalar-type t))
              ,(byte-size t)
              CL_MEM_READ_WRITE))]
      [(print ,[compile-expr -> expr]) `(print ,expr)]
      [(print ,[compile-expr -> e1] ,[compile-expr -> e2]) `(print ,e1 ,e2)]
      [(return ,[compile-expr -> expr]) `(return ,expr)]
      [(assert ,[compile-expr -> expr]) `(do (assert ,expr))]
      [(set! ,x ,e)
       (compile-set! x e)]
      [(vector-set! ,v ,i ,[compile-expr -> expr])
       `(vector-set! ,v ,i ,expr)]
      [(while (,relop ,[compile-expr -> e1] ,[compile-expr -> e2])
            ,[stmt*] ...)
       `(while (,relop ,e1 ,e2) ,stmt* ...)]
      [(for (,i ,[compile-expr -> start] ,[compile-expr -> end])
            ,[stmt*] ...)
       `(for (,i ,start ,end) ,stmt* ...)]
      [(do ,[compile-expr -> e] ...) `(do ,e ...)]
      [(map-gpu ((,x* ,e*) ...) ,[stmt*] ...)
       `(block
         ,@(map (lambda (x e)
                  `(let ,x (cl::buffer_map ,(scalar-type (type-of e)))
                        ((field g_queue mapBuffer ,(scalar-type (type-of e)))
                         ,(compile-expr e))))
                x* e*)
         ,stmt* ...)]
      [(block ,[stmt*] ...)
       `(block ,stmt* ...)]
      [,else (error 'compile-stmt (format "unknown stmt type ~s" else))])))

(define (compile-set! x e)
  (let ((lhs-t (type-of x)))
    (match lhs-t
      ((vector ,t ,n)
       `(do (memcpy ,(compile-expr x) ,(compile-expr e) ,(byte-size lhs-t))))
      (,scalar
       (guard (symbol? scalar))
       `(set! ,(compile-expr x) ,(compile-expr e)))
      (,e (error 'compile-set! "Unknown target type" e)))))

(define-match (scalar-type)
  ;; TODO: converting the types should happen after this, so hopefully
  ;; we won't need pointers.
  ((ptr ,[t]) t)
  ((vector ,[t] ,n) t)
  (int 'int))

(define-match (byte-size)
  (int `(sizeof int))
  ((vector ,[t] ,n) `(* ,n ,t)))

;; This compile kernel is used in the compile-module pass.
(define-match (compile-kernel^)
  ((kernel ,name ,args ,[compile-stmt -> stmt*] ...)
   `(kernel ,name ,args . ,stmt*)))

(define compile-expr
  (lambda (expr)
    (match expr
      [(int ,n) (guard (number? n)) n]
      [(u64 ,n) (guard (number? n)) n]
      [(var ,t ,x) (guard (symbol? x)) x]
      [(str ,s) (guard (string? s)) s]
      [(vector-ref ,t ,[v] ,[i]) `(vector-ref ,v ,i)]
      [(field ,[obj] ,x)
       (guard (ident? x))
       `(field ,obj ,x)]
      ((field ,[obj] ,x ,t)
       (guard (ident? x))
       `(field ,obj ,x ,t))
      [(sizeof ,t) `(sizeof ,t)]
      [(deref ,[e]) `(deref ,e)]
      [(addressof ,[e]) `(addressof ,e)]
      [(cast ,t ,[e]) `(cast ,t ,e)]
      [(,op ,[e1] ,[e2]) (guard (binop? op)) `(,op ,e1 ,e2)]
      [(time) '(nanotime)]
      [(call ,t ,f ,[a*] ...)
       (guard (symbol? f))
       `(,f ,a* ...)]
      [(call ,t ,[f] ,[a*] ...) `(,f ,a* ...)]
      [,else (error 'compile-expr (format "unknown expr type ~s" else))]))))

;; this program shouldn't typecheck,
;; since main must have type () -> int
;; (fn main ()) :: () -> ()

;; After annotate-types,
;; (module (fn main () (return 0)))
;; becomes
;; (module (fn main () (() -> int) (return (int 0))))

;; example
'(test 'compile-empty
  (compile-harlan
   '(module
       (fn main () (return 0))))
  '((func int main () (return 0))))

'(test 'compile-print
  (compile-harlan
    '(module
       (fn main () (print 42) (return 0))))
  '((func int main () (print 42) (return 0))))


;; need to add a flatten pass (flatten-vector) to
;; flatten the (print (vector ...)) [or anywhere else where a call to
;; vector appears in non-tail position] and introduce a new variable
;; declaration inside a block.
'(test 'compile-print
  (compile-harlan
    `(module
       (fn main ()
         (print (vector 1 2 3 4))
         (return 0)))
    `((func int main ()
            (block
             (let t_0 (vector int) 4)
             (vector-set! t_0 0 1)
             (vector-set! t_0 1 2)
             (vector-set! t_0 2 3)
             (vector-set! t_0 3 4)
             (print t_0))
            (return 0)))))
