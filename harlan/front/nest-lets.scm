(library
  (harlan front nest-lets)
  (export nest-lets)
  (import
    (rnrs)
    (util helpers)
    (util match))

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
  ((fn ,name ,args . ,[(Stmt* '()) -> stmt*])
   `(fn ,name ,args . ,stmt*))
  (,else else))

(define (unroll-lets def* stmt*)
  (cond
    ((null? def*) stmt*)
    (else
      `((let (,(car def*)) .
          ,(unroll-lets (cdr def*) stmt*))))))

(define-match (Stmt* def*)
  ((,stmt) (unroll-lets def* (Stmt stmt)))
  (((let ,x ,e) . ,stmt*)
   ((Stmt* (append def* `((,x ,e)))) stmt*))
  ((,[Stmt -> stmt] . ,stmt*)
   (unroll-lets def*
     (append stmt ((Stmt* '()) stmt*)))))

(define-match Stmt
  ((for (,x ,start ,end) . ,[(Stmt* '()) -> stmt*])
   `((for (,x ,start ,end) . ,stmt*)))
  ((while ,test . ,[(Stmt* '()) -> stmt*])
   `((while ,test . ,stmt*)))
  (,else `(,else)))

;; end library
)
