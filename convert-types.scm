(library
  (convert-types)
  (export convert-types)
  (import (only (chezscheme) format)
    (rnrs)
    (util helpers)
    (only (print-c) binop?))
  
;; This pass converts Harlan types into C types.
(define-match (convert-types)
  ((module ,[convert-decl -> decl*] ...)
   `(module . ,decl*)))

(define-match (convert-decl)
  ((fn ,name (,x* ...)
       ,[convert-type -> rtype]
       ,[convert-stmt -> stmt*] ...)
   (guard (andmap symbol? x*))
   `(fn ,name ,x* ,rtype . ,stmt*))
  ((extern ,name . ,[convert-type -> t])
   `(extern ,name . ,t)))

(define-match (convert-stmt)
  ((let ,x ,[convert-type -> type] ,[convert-expr -> e])
   (guard (symbol? x))
   `(let ,x ,type ,e))
  ((set! ,[convert-expr -> loc] ,[convert-expr -> val])
   `(set! ,loc ,val))
  ((print ,[convert-expr -> e]) `(print ,e))
  ((assert ,[convert-expr -> expr]) `(assert ,expr))
  ((for (,x ,[convert-expr -> begin] ,[convert-expr -> end])
        ,[convert-stmt -> stmt*] ...)
   (guard (symbol? x))
   `(for (,x ,begin ,end) . ,stmt*))
  ((kernel (((,x* ,[convert-type -> t*]) (,xs* ,[convert-type -> ts*])) ...)
           ,[body*] ...)
   `(kernel ,(map (lambda (x t xs ts)
                    `((,x ,t) (,xs ,ts)))
                  x* t* xs* ts*)
            . ,body*))
  ((do ,[convert-expr -> e*] ...)
   `(do . ,e*))
  ((return ,[convert-expr -> expr])
   `(return ,expr)))

(define-match (convert-expr)
  ((int ,n) (guard (number? n)) `(int ,n))
  ((str ,s) (guard (string? s)) `(str ,s))
  ((var ,[convert-type -> t] ,x) (guard (symbol? x)) `(var ,t ,x))
  ((,op ,[lhs] ,[rhs]) (guard (binop? op))
   `(,op ,lhs ,rhs))
  ;; sizeof might need some more work, since (sizeof (vector int 4))
  ;; != (sizeof (ptr int))
  ((sizeof ,[convert-type -> t]) `(sizeof ,t))
  ((vector-ref ,[convert-type -> t]
               ,[convert-expr -> v]
               ,[convert-expr -> i])
   `(vector-ref ,t ,v ,i))
  ((cast ,[convert-type -> t] ,[e]) `(cast ,t ,e))
  ((deref ,[e]) `(deref ,e))
  ((addressof ,[e]) `(addressof ,e))
  ((call ,[convert-type -> rtype] ,fn ,[convert-expr -> arg*] ...)
   `(call ,rtype ,fn . ,arg*)))

(define-match (convert-type)
  (int 'int)
  (u64 'uint64_t)
  (void 'void)
  ;; TODO: having (ptr void) is a little untidy, but we need it to
  ;; generate calls to malloc.
  ((ptr void) '(ptr void))
  ((vector ,[find-leaf-type -> t] ,size)
   `(ptr ,(convert-type t)))
  (((,[t*] ...) -> ,[t])
   `(,t* -> ,t)))

(define-match (find-leaf-type)
  ((vector ,[t] ,size) t)
  (,t (guard (harlan-scalar-type? t)) t))

(define harlan-scalar-type?
  (lambda (t)
    (case t
      ((int) #t)
      (else #f))))

;; end library
)