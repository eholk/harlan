(library
  (harlan front nest-lets)
  (export nest-lets)
  (import
    (rnrs)
    (elegant-weapons helpers)
    (elegant-weapons match))

;; parse-harlan takes a syntax tree that a user might actually want
;; to write and converts it into something that's more easily
;; analyzed by the type inferencer and the rest of the compiler.
;; This subsumes the functionality of the previous
;; simplify-literals mini-pass.

;; unnests lets, checks that all variables are in scope, and
;; renames variables to unique identifiers
  
(define-match nest-lets
  ((module ,[Decl -> decl*] ...)
   `(module . ,decl*)))

(define-match Decl
  ((fn ,name ,args . ,[(Expr* '()) -> expr*])
   `(fn ,name ,args . ,expr*))
  (,else else))

(define (unroll-lets def* expr*)
  (cond
    ((null? def*) expr*)
    (else
      `((let (,(car def*)) .
          ,(unroll-lets (cdr def*) expr*))))))

(define-match (Expr* def*)
  (((let ,x ,[Expr -> e]))
   (guard (symbol? x))
   (unroll-lets def* `(,e)))
  (((let ,x ,[Expr -> e]) . ,expr*)
   (guard (symbol? x))
   ((Expr* (append def* `((,x ,e)))) expr*))
  ((,expr) (unroll-lets def* `(,(Expr expr))))
  ((,[Expr -> expr] . ,expr*)
   (unroll-lets def*
     (cons expr ((Expr* '()) expr*)))))

(define-match Expr
  ((for (,x ,start ,end) . ,[(Expr* '()) -> expr*])
   `(for (,x ,start ,end) . ,expr*))
  ((while ,[Expr -> test] . ,[(Expr* '()) -> expr*])
   `(while ,test . ,expr*))
  ((kernel ((,x ,[Expr -> e]) ...) . ,[(Expr* '()) -> expr*])
   `(kernel ((,x ,e) ...) . ,expr*))
  ((let ((,x ,[Expr -> e]) ...) . ,[(Expr* '()) -> expr*])
   `(let ((,x ,e) ...) . ,expr*))
  ((print ,[Expr -> e]) `(print ,e))
  ((assert ,[Expr -> e]) `(assert ,e))
  ((set! ,[Expr -> x] ,[Expr -> v]) `(set! ,x ,v))
  ((vector-set! ,[Expr -> v] ,[Expr -> i] ,[Expr -> e])
   `(vector-set! ,v ,i ,e))
  ((begin . ,expr*)
   (make-begin ((Expr* '()) expr*)))
  ((if ,[Expr -> t] ,[Expr -> c])
   `(if ,t ,c))
  ((if ,[Expr -> t] ,[Expr -> c] ,[Expr -> a])
   `(if ,t ,c ,a))
  ((return ,[Expr -> e]) `(return ,e))
  ((vector ,[Expr -> e*] ...) `(vector . ,e*))
  ((vector-ref ,[Expr -> v] ,[Expr -> i])
   `(vector-ref ,v ,i))
  ((reduce ,op ,[Expr -> e]) `(reduce ,op ,e))
  ((length ,[Expr -> e]) `(length ,e))
  ((,op ,[Expr -> e1] ,[Expr -> e2])
   (guard (or (binop? op) (relop? op)))
   `(,op ,e1 ,e2))
  ((,v ,[Expr -> e*] ...) (guard (symbol? v))
   `(,v . ,e*))
  (,else else))

;; end library
)
