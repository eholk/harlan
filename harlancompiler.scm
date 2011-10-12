(library
 (harlancompiler)
 (export compile-harlan lift-vectors annotate-types test
         compile-harlan-middle verbose
         compile-harlan-frontend compile-module)
 (import (chezscheme)
         (match)
         (typecheck)
         (vectors)
         (returnify)
         (returnify-kernels)
         (kernels)
         (convert-types)
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

(define annotate-types
  (lambda (expr)
    (let ((result (typecheck expr)))
      (if (null? result)
          (begin (display expr) (newline)
                 (error 'annotate-types "Could not infer types."))
          result))))

(define compile-harlan
  (lambda (expr)
    (let* ((expr (trace-pass "compile-harlan-frontend"
                             compile-harlan-frontend  expr))
           (expr (trace-pass "compile-harlan-middle"
                             compile-harlan-middle expr)))
      expr)))

;; The typical frontend of a compiler. Syntax expansion, typechecking,
;; etc.
(define compile-harlan-frontend
  (lambda (expr)
    (let* ((expr (trace-pass "returnify" returnify expr))
           (expr (trace-pass "lift-vectors" lift-vectors expr))
           (expr (trace-pass "annotate-types" annotate-types expr)))
      expr)))

;; The "middle end" of a compiler. No one ever knows what's supposed
;; to go here. This goes from TFC to something we can give to print-c.
(define compile-harlan-middle
  (lambda (expr)
    (let* ((expr (trace-pass "lower-vectors" lower-vectors expr))
           (expr (trace-pass "returnify-kernels" returnify-kernels expr))
           (expr (trace-pass "uglify-vectors" uglify-vectors expr))
           (expr (trace-pass "compile-module" compile-module expr))
           (expr (trace-pass "verify-compile-module" verify-compile-module expr))
           (expr (trace-pass "annotate-free-vars" annotate-free-vars expr))
           (expr (trace-pass "hoist-kernels" hoist-kernels expr))
           (expr (trace-pass "move-gpu-data" move-gpu-data expr))
           (expr (trace-pass "generate-kernel-calls" generate-kernel-calls expr))
           (expr (trace-pass "convert-types" convert-types expr))
           (expr (trace-pass "compile-kernels" compile-kernels expr)))
      expr)))

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
      [,else (error 'compile-decl (format "unknown decl type ~s" else))])))
 
(define compile-stmt
  (lambda (stmt)
    (match stmt
      [(let ,x ,t ,[compile-expr -> e])
       `(let ,x ,t ,e)]
      [(print ,[compile-expr -> expr]) `(print ,expr)]
      [(print ,[compile-expr -> e1] ,[compile-expr -> e2]) `(print ,e1 ,e2)]
      [(return ,[compile-expr -> expr]) `(return ,expr)]
      [(assert ,[compile-expr -> expr]) `(do (assert ,expr))]
      [(set! ,[compile-expr -> x] ,[compile-expr -> e]) `(set! ,x ,e)]
      [(vector-set! ,v ,i ,[compile-expr -> expr])
       `(vector-set! ,v ,i ,expr)]
      [(for (,i ,[compile-expr -> start] ,[compile-expr -> end])
            ,[stmt*] ...)
       `(for (,i ,start ,end) ,stmt* ...)]
      [(do ,[compile-expr -> e] ...) `(do ,e ...)]
      ;; TODO: these are expressions.
      [(kernel ,iter ,[stmt*] ...) `(kernel ,iter ,stmt* ...)]
      [,else (error 'compile-stmt (format "unknown stmt type ~s" else))])))

(define compile-expr
  (lambda (expr)
    (match expr
      [(int ,n) (guard (number? n)) n]
      [(u64 ,n) (guard (number? n)) n]
      [(var ,tx ,x) (guard (symbol? x)) x]
      [(str ,s) (guard (string? s)) s]
      [(vector-ref ,t ,[v] ,[i]) `(vector-ref ,v ,i)]
      [(length ,[v]) `((field ,v length))]
      [(sizeof ,t) `(sizeof ,t)]
      [(deref ,[e]) `(deref ,e)]
      [(addressof ,[e]) `(addressof ,e)]
      [(cast ,t ,[e]) `(cast ,t ,e)]
      [(,op ,[e1] ,[e2]) (guard (binop? op)) `(,op ,e1 ,e2)]
      [(time) '(nanotime)]
      [(call ,t ,f ,[a*] ...) `(,f ,a* ...)]
      [,else (error 'compile-expr (format "unknown expr type ~s" else))])))

)

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
