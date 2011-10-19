(library
  (lower-vectors)
  (export
    lower-vectors
    verify-lower-vectors)
  (import
    (only (chezscheme) format)
    (rnrs)
    (util match)
    (print-c)
    (util verify-grammar)
    (util helpers))

(generate-verify lower-vectors
  (Module (module Decl *))
  (Decl
    (fn Var (Var *) ((Type *) -> Type) Stmt * Ret-Stmt)
    (extern Var (Type *) -> Type))
  (Stmt 
    (print Expr)
    (print Expr Expr)
    (assert Expr)
    (set! (var Type Var) Expr)
    (vector-set! Type Expr Expr Expr)
    (kernel Type (((Var Type) (Expr Type)) *) Stmt * Expr)
    (let Var Type Expr)
    (for (Var Expr Expr) Stmt *)
    (while Expr Stmt *)
    (do Expr *)
    Ret-Stmt)
  (Ret-Stmt (return Expr))
  (Expr 
    (int Integer)
    (u64 Number)
    (str String)
    (var Type Var)
    (reduce int Binop Expr)
    (vector Expr *)
    (call Type Var Expr *)
    (vector-ref Type Expr Expr)
    (kernel Type (((Var Type) (Expr Type)) *) Stmt * Expr)
    (Unaryop Expr)
    (Binop Expr Expr))
  (Var symbol)
  (String string)
  (Number number)
  (Integer integer)
  (Type (vector Type Integer) wildcard)
  (Binop binop)
  (Unaryop unaryop))

;; Moves to a lower-level allocate and set! representation for
;; vectors. This runs after typechecking.
(define lower-vectors
  (lambda (mod)
    (match mod
      ((module ,[lower-decl -> fn*] ...)
       `(module . ,fn*)))))

(define (lower-decl fn)
  (match fn
    ((fn ,name ,args ,t ,[lower-stmt -> stmt*] ...)
     `(fn ,name ,args ,t . ,(apply append stmt*)))
    ((extern ,name ,args -> ,t)
     `(extern ,name ,args -> ,t))
    (,else (error 'lower-fn "unknown fn" else))))

(define lower-stmt
  (lambda (stmt)
    (match stmt
      ((let ,x ,t (vector ,e* ...))
       `((let ,x ,t (int ,(length e*)))
         . ,(let loop ((e* e*)
                       (i 0))
              (if (null? e*)
                  '()
                  `((vector-set! ,(cadr t) (var ,t ,x) (int ,i) ,(car e*)) .
                    ,(loop (cdr e*) (+ 1 i)))))))
      
      ((let ,x ,t (make-vector ,vt ,e))
       `((let ,x ,t ,e)))
      
      ((let ,x ,t (iota ,e))
       (let ((i (gensym 'i)))
         `((let ,x ,t ,e)
           (for (,i (int 0) ,e)
             (vector-set! int (var ,t ,x) (var int ,i) (var int ,i))))))
      
      ((let ,x ,t (reduce ,t2 + (var ,tv ,v)))
       (let ((i (gensym 'i)))
         `((let ,x ,t (vector-ref ,t (var ,tv ,v) (int 0)))
           (for (,i (int 1) (length (var ,tv ,v)))
             (set! (var ,t ,x)
               (+ (var ,t ,x)
                 (vector-ref ,t (var ,tv ,v) (var int ,i))))))))
      ((kernel ,t ,iter ,[body*] ...)
       `((kernel ,t ,iter . ,(apply append body*))))
      ((let ,x ,t ,e)
       `((let ,x ,t ,e)))
      ((print ,expr)
       `((print ,expr)))
      ((print ,e1 ,e2)
       `((print ,e1 ,e2)))      
      ((assert ,expr)
       `((assert ,expr)))
      ((set! ,x ,i) `((set! ,x ,i)))
      ((vector-set! ,t ,e1 ,i ,e2)
       `((vector-set! ,t ,e1 ,i ,e2)))
      ((while (,relop ,x ,y) ,[body*] ...)
       `((while (,relop ,x ,y) . ,(apply append body*))))
      ((for ((,x ,t) ,start ,end) ,[body*] ...)
       `((for (,x ,start ,end) . ,(apply append body*))))
      ((return ,expr)
       `((return ,expr)))
      ((do ,expr) `((do ,expr)))
      (,else (error 'lower-stmt "unknown statment" else)))))

)
