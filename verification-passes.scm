(library
  (verification-passes)
  (export
    verify-harlan
    verify-parse-harlan
    verify-returnify
    verify-lift-vectors
    verify-typecheck
    verify-lower-vectors
    verify-returnify-kernels
    verify-uglify-vectors)
  (import
    (rnrs)
    (only (print-c) binop? relop? unaryop?)
    (only (harlan parser) scalar-type? ident? reduceop?)
    (util verify-grammar))

  (grammar-transforms
    
    (harlan
      (Module (module Decl *))
      (Decl
        (extern Var (Type *) -> Type)
        (fn Var (Var *) Stmt *))
      (Type
        scalar-type
        (vector Type Integer)
        ((Type *) -> Type))
      (Stmt
        (let Var Expr)
        (print Expr)
        (assert Expr)
        (set! Expr Expr)
        (vector-set! Expr Expr Expr)
        (for (Var Expr Expr) Stmt *)
        (while Expr Stmt *)
        (return Expr)
        Expr)
      (Expr
        integer
        string
        ident
        (var Var)
        (vector Expr *)
        (vector-ref Expr Expr)
        (kernel ((Var Expr) *) Stmt * Expr)
        (reduce Reducer Expr)
        (iota Integer)
        (length Expr)
        (make-vector Integer)
        (Binop Expr Expr)
        (Relop Expr Expr)
        (Var Expr *))
      (Var ident)
      (Integer integer)
      (Reducer reduceop)
      (Binop binop)
      (Relop relop))

    (parse-harlan
      (%inherits Module Decl Type
        Var Integer Reducer Binop Relop)
      (Stmt
        (let Var Expr)
        (print Expr)
        (assert Expr)
        (set! Expr Expr)
        (vector-set! Expr Expr Expr)
        (do Expr)
        (for (Var Expr Expr) Stmt *)
        (while Expr Stmt *)
        (return Expr))
      (Expr
        (num Integer)
        (str String)
        (var Var)
        (vector Expr *)
        (vector-ref Expr Expr)
        (kernel ((Var Expr) *) Stmt * Expr)
        (reduce Reducer Expr)
        (iota (num Integer))
        (length Expr)
        (make-vector (num Integer))
        (Binop Expr Expr)
        (Relop Expr Expr)
        (call Var Expr *))
      (String string))

    (lift-vectors
      (%inherits Module Decl
        Var Integer Reducer Binop Relop String Type)
      (Stmt
        (print Expr)
        (print Expr Expr)
        (assert Expr)
        (set! (var Var) Expr)
        (vector-set! Expr Expr Expr)
        (kernel ((Var Expr) *) Stmt * Expr)
        (let Var Let-Expr)
        (do Expr)
        (for (Var Expr Expr) Stmt *)
        (while (Relop Expr Expr) Stmt *)
        (return Expr))
      (Let-Expr
        (reduce Reducer Let-Expr)
        (vector Let-Expr *)
        (make-vector (num Integer))
        (kernel ((Var Let-Expr) *) Stmt * Let-Expr)
        (iota (num Integer))
        Expr)
      (Expr
        (num Integer)
        (str String)
        (var Var)
        (length Expr)
        (call Var Expr *)
        (vector-ref Expr Expr)
        (Binop Expr Expr)))

    (returnify
      (%inherits Module
        Expr Var Integer Reducer Binop Relop String Type Let-Expr)
      (Decl
        (fn Var (Var *) Stmt * Ret-Stmt)
        (extern Var (Type *) -> Type))
      (Stmt
        (print Expr)
        (print Expr Expr)
        (assert Expr)
        (set! (var Var) Expr)
        (vector-set! Expr Expr Expr)
        (kernel ((Var Expr) *) Stmt * Expr)
        (let Var Let-Expr)
        (do Expr)
        (for (Var Expr Expr) Stmt *)
        (while (Relop Expr Expr) Stmt *)
        Ret-Stmt)
      (Ret-Stmt (return Expr)))

    (typecheck
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
        (let Var Type Lifted-Expr)
        (for ((Var Type) Expr Expr) Stmt *)
        (while (Relop Expr Expr) Stmt *)
        (do Expr)
        Ret-Stmt)
      (Ret-Stmt (return Expr))
      (Lifted-Expr
        Expr
        (reduce Type Binop Expr)
        (vector Expr *)
        (make-vector Type (int Integer))
        (iota (int Integer)))
      (Expr 
        (int Integer)
        (u64 Number)
        (str String)
        (var Type Var)
        (call Type Var Expr *)
        (vector-ref Type Expr Expr)
        (kernel Type (((Var Type) (Expr Type)) *) Stmt * Expr)
        (Unaryop Expr)
        (Relop Expr Expr)
        (Binop Expr Expr))
      (Var ident)
      (String string)
      (Number number)
      (Integer integer)
      (Type
        (vector Type Integer)
        scalar-type
        ((Type *) -> Type))
      (Binop binop)
      (Relop relop)
      (Unaryop unaryop))

    (lower-vectors
      (%inherits Module Decl
        Var String Number Integer Type Binop Relop Unaryop)
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
        (Binop Expr Expr)))

    (returnify-kernels
      (%inherits Module Decl Ret-Stmt
        Expr
        Var String Number Integer Type Binop Unaryop)
      (Stmt 
        (print Expr)
        (print Expr Expr)
        (assert Expr)
        (set! (var Type Var) Expr)
        (vector-set! Type Expr Expr Expr)
        (kernel Type (((Var Type) (Expr Type)) *) Stmt *)
        (let Var Type Expr)
        (for (Var Expr Expr) Stmt *)
        (while Expr Stmt *)
        (do Expr *)
        Ret-Stmt))

    (uglify-vectors
      (%inherits Module Decl Ret-Stmt
        Var String Number Integer Type Binop Unaryop)
      (Stmt 
        (print Expr)
        (print Expr Expr)
        (assert Expr)
        (set! (var Type Var) Expr)
        (set! (vector-ref Type Expr Expr) Expr)
        (kernel (((Var Type) (Expr Type)) *) Stmt *)
        (let Var Type Expr)
        (for (Var Expr Expr) Stmt *)
        (while Expr Stmt *)
        (do Expr *)
        Ret-Stmt)
      (Expr 
        (int Integer)
        (u64 Number)
        (str String)
        (var Type Var)
        (reduce int Binop Expr)
        (vector Expr *)
        (sizeof Type)
        (addressof Expr)
        (cast Type (call Type Var Expr *))
        (call Type Var Expr *)
        (vector-ref Type Expr Expr)
        (Unaryop Expr)
        (Binop Expr Expr))))

  )