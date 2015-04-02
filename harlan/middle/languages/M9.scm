(library
    (harlan middle languages M9)
  (export M9 unparse-M9
          M9.1 unparse-M9.1
          M9.2 unparse-M9.2
          M9.2.1 unparse-M9.2.1
          M9.2.2 unparse-M9.2.2
          M9.3 unparse-M9.3)
  
    (import
     (rnrs)
     (nanopass)
     (only (elegant-weapons helpers) binop? scalar-type? relop?)
     (harlan middle languages1)
     (harlan middle languages2))

    (define-language M9
    (extends M8)
    (entry Module)

    (Stmt
     (stmt)
     (- (apply-kernel x (e1* ...) e* ...))))
  
  (define (any? x) (lambda _ #t))
  (define (address-space? x) (memq x '(local global private)))
    
  (define-language M9.1
    (extends M9)
    (entry Module)

    (terminals
     (+ (any (?))))
    
    (Decl
     (decl)
     (- (gpu-module k* ...))
     (+ (gpu-module cg k* ...)))
    
    (CallGraph
     (cg)
     (+ (call-graph ?0 ?1))))

  (define-language M9.2
    (extends M9.1)
    (entry Module)

    (terminals
     (- (any (?))))
    
    (Decl
     (decl)
     (+ (gpu-module k* ...))
     (- (gpu-module cg k* ...)))
    
    (Body
     (body)
     (+ (with-labels (lbl ...) stmt)))

    (CommonDecl
     (cdecl)
     (- (fn x (x* ...) t stmt))
     (+ (fn x (x* ...) t body)))

    (LabeledBlock
     (lbl)
     (+ (name ((x t) ...) stmt)))

    (CallGraph
     (cg)
     (- (call-graph ?0 ?1)))
    
    (Expr
     (e)
     (+ (call-label name t e* ...))))

  (define-language M9.2.1
    (extends M9.2)
    (entry Module)
    
    (Expr
     (e)
     (- (call-label name t e* ...))
     (+ (call-label name t ((x* t*) ...) e* ...))))

  (define-language M9.2.2
    (extends M9.2.1)
    (entry Module)

    (Rho-Type
     (t)
     (+ (fixed-array t i)))
    
    (Stmt
     (stmt)
     (+ (push! e0 e1 t e2) ;; push e2 with type t onto e0 + e1
        (pop! e0 e1 t e2)
        (goto name)
        (label name)))

    (Body
     (body)
     ;; like begin, bit doesn't conflict with the existing
     ;; (begin stmt* ... stmt)
     (+ (seq stmt* ... body)))
    
    (Expr
     (e)
     (+ (call-label name e* ...))
     (- (call-label name t ((x* t*) ...) e* ...))))
  
  (define-language M9.3
    (extends M9.2.2)
    (entry Module)

    (terminals
     (+ (address-space (space))))
    
    (Body
     (body)
     (- (with-labels (lbl ...) stmt)
        (seq stmt* ... body)))

    (Stmt
     (stmt)
     (- (push! e0 e1 t e2)
        (pop! e0 e1 t e2)))
    
    (Expr
     (e)
     (- (call-label name e* ...)))

    (Rho-Type
     (t)
     (+ (ptr space t)))

    (LabeledBlock
     (lbl)
     (- (name ((x t) ...) stmt)))))
