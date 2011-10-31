(library
  (harlan front flatten-lets)
  (export flatten-lets)
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
  
(define-match flatten-lets
  ((module ,[Decl -> decl*] ...)
   `(module . ,decl*)))

(define-match Decl
  ((fn ,name ,args ,[Stmt -> stmt*] ...)
   `(fn ,name ,args . ,(apply append stmt*)))
  (,else else))

(define-match Stmt
  ((for (,x ,start ,end) ,[stmt*] ...)
   `((for (,x ,start ,end) . ,(apply append stmt*))))
  ((while ,test ,[stmt*] ...)
   `((while ,test . ,(apply append stmt*))))
  ((let ((,x* ,e*) ...) ,[stmt*] ...)
   `(,@(map (lambda (x e) `(let ,x ,e)) x* e*)
     . ,(apply append stmt*)))
  (,else `(,else)))

;; end library
)
