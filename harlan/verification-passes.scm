(library
  (harlan verification-passes)
  (export
    verify-harlan
    verify-parse-harlan
    verify-returnify
    verify-lift-vectors
    verify-typecheck
    verify-lower-vectors
    verify-returnify-kernels
    verify-remove-nested-kernels
    verify-returnify-kernels
    verify-uglify-vectors
    verify-annotate-free-vars
    #|
    verify-hoist-kernels
    verify-move-gpu-data
    verify-generate-kernel-calls
    verify-compile-module
    verify-convert-types
    verify-compile-kernels
    |#)
  (import
    (rnrs)
    (util helpers)
    (util verify-grammar))

  (grammar-transforms

    (%static
      (Module (module Decl *))
      (Type
        scalar-type
        (vector Type Integer)
        (ptr Type)
        ((Type *) -> Type))
      (Var ident)
      (Integer integer)
      (Reduceop reduceop)
      (Binop binop)
      (Relop relop)
      (Float float)
      (String string)
      (Number number))
    
    (harlan
      (Start Module)
      (Decl
        (extern Var (Type *) -> Type)
        (fn Var (Var *) Stmt *))
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
        float
        string
        ident
        (var Var)
        (vector Expr *)
        (vector-ref Expr Expr)
        (kernel ((Var Expr) *) Stmt * Expr)
        (reduce Reduceop Expr)
        (iota Integer)
        (length Expr)
        (make-vector Integer)
        (Binop Expr Expr)
        (Relop Expr Expr)
        (Var Expr *)))

    (parse-harlan (%inherits Decl)
      (Start Module)
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
        (float Float)
        (str String)
        (var Var)
        (vector Expr *)
        (vector-ref Expr Expr)
        (kernel ((Var Expr) *) Stmt * Expr)
        (reduce Reduceop Expr)
        (iota (num Integer))
        (length Expr)
        (int->float Expr)
        (make-vector (num Integer))
        (Binop Expr Expr)
        (Relop Expr Expr)
        (call Var Expr *)))

    (typecheck
      (Start Module)
      (Decl
        (extern Var (Type *) -> Type)
        (fn Var (Var *) Type Stmt *))
      (Stmt
        (let Var Type Expr)
        (print Expr)
        (assert Expr)
        (set! Expr Expr)
        (vector-set! Type Expr Expr Expr)
        (do Expr)
        (for ((Var Type) Expr Expr) Stmt *)
        (while Expr Stmt *)
        (return Expr))
      (Expr
        (int Integer)
        (u64 Number)
        (float Float)
        (str String)
        (var Type Var)
        (vector Type Expr *)
        (vector-ref Type Expr Expr)
        (kernel Type (((Var Type) (Expr Type)) *) Stmt * Expr)
        (reduce Type Reduceop Expr)
        (iota (int Integer))
        (length Expr)
        (int->float Expr)
        (make-vector Type (int Integer))
        (Binop Expr Expr)
        (Relop Expr Expr)
        (call Type Var Expr *)))

    (returnify (%inherits Expr)
      (Start Module)
      (Decl
        (fn Var (Var *) Type Stmt * Ret-Stmt)
        (extern Var (Type *) -> Type))
      (Stmt
        (let Var Type Expr)
        (print Expr)
        (assert Expr)
        (set! Expr Expr)
        (vector-set! Type Expr Expr Expr)
        (do Expr)
        (for ((Var Type) Expr Expr) Stmt *)
        (while Expr Stmt *)
        Ret-Stmt)
      (Ret-Stmt (return Expr)))

    (lift-vectors (%inherits Decl Ret-Stmt)
      (Start Module)
      (Stmt
        (let Var Type Let-Expr)
        (print Expr)
        (assert Expr)
        (set! Expr Expr)
        (vector-set! Type Expr Expr Expr)
        (do Expr)
        (for ((Var Type) Expr Expr) Stmt *)
        (while Expr Stmt *)
        Ret-Stmt)
      (Let-Expr
        (kernel Type (((Var Type) (Let-Expr Type)) *) Stmt * Let-Expr)
        (vector Let-Expr *)
        (make-vector Type (int Integer))
        (iota (int Integer))
        Expr)
      (Expr
        (int Integer)
        (u64 Number)
        (float Float)
        (str String)
        (var Type Var)
        (int->float Expr)
        (length Expr)
        (call Type Var Expr *)
        (reduce Type Reduceop Let-Expr)
        (vector-ref Type Expr Expr)
        (Binop Expr Expr)
        (Relop Expr Expr)))

    (lower-vectors (%inherits Decl Ret-Stmt)
      (Start Module)
      (Stmt 
        (print Expr)
        (print Expr Expr)
        (assert Expr)
        (set! (var Type Var) Expr)
        (vector-set! Type Expr Expr Expr)
        (let Var Type Let-Expr)
        (for (Var Expr Expr) Stmt *)
        (while Expr Stmt *)
        (do Expr *)
        Ret-Stmt)
      (Let-Expr
        (kernel Type (((Var Type) (Expr Type)) *) Stmt * Expr)
        (reduce Type Reduceop Expr)
        (vector Expr *)
        (iota (int Integer))
        Expr)
      (Expr 
        (int Integer)
        (u64 Number)
        (float Float)
        (int->float Expr)
        (str String)
        (var Type Var)
        (call Type Var Expr *)
        (vector-ref Type Expr Expr)
        (length Expr)
        (Relop Expr Expr)
        (Binop Expr Expr)))

    (remove-nested-kernels (%inherits Decl Ret-Stmt Let-Expr Expr Stmt)
      (Start Module))

    (returnify-kernels (%inherits Decl Ret-Stmt Let-Expr Expr)
      (Start Module)
      (Stmt 
        (print Expr)
        (print Expr Expr)
        (assert Expr)
        (set! (var Type Var) Expr)
        (vector-set! Type Expr Expr Expr)
        (kernel Type (((Var Type) (Expr Type)) *) Stmt *)
        (let Var Type Let-Expr)
        (for (Var Expr Expr) Stmt *)
        (while Expr Stmt *)
        (do Expr *)
        Ret-Stmt))

    (uglify-vectors (%inherits Decl Ret-Stmt)
      (Start Module)
      (Stmt 
        (print Expr)
        (print Expr Expr)
        (assert Expr)
        (set! (var Type Var) Expr)
        (set! (vector-ref Type Expr Expr) Expr)
        (kernel (((Var Type) (Expr Type)) *) Stmt *)
        (let Var Type Let-Expr)
        (for (Var Expr Expr) Stmt *)
        (while Expr Stmt *)
        (do Expr *)
        Ret-Stmt)
      (Let-Expr
        (reduce Type Reduceop Expr)
        (vector Expr *)
        (iota (int Integer))
        Expr)
      (Expr 
        (int Integer)
        (u64 Number)
        (str String)
        (float Float)
        (var Type Var)
        (call Type Var Expr *)
        (cast Type Expr)
        (sizeof Type)
        (addressof Expr)
        (vector-ref Type Expr Expr)
        (length Expr)
        (Relop Expr Expr)
        (Binop Expr Expr)))

    (annotate-free-vars (%inherits Decl Let-Expr Expr Ret-Stmt)
      (Start Module)
      (Stmt 
        (print Expr)
        (print Expr Expr)
        (assert Expr)
        (set! (var Type Var) Expr)
        (set! (vector-ref Type Expr Expr) Expr)
        (kernel (((Var Type) (Expr Type)) *)  (free-vars (Var Type) *) Stmt *)
        (let Var Type Let-Expr)
        (for (Var Expr Expr) Stmt *)
        (while Expr Stmt *)
        (do Expr *)
        Ret-Stmt))

    #|
    (hoist-kernels
      (Start wildcard))

    (move-gpu-data
      (%inherits Start))

    (generate-kernel-calls
      (%inherits Start))

    (compile-module
      (%inherits Start))

    (convert-types
      (%inherits Start))

    (compile-kernels
      (%inherits Start))
    |#
    )
  )