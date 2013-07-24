(library
    (harlan middle remove-recursion)
  (export remove-recursion)
  (import
   (rnrs)
   (rnrs mutable-pairs)
   (only (chezscheme) pretty-print trace-define make-parameter parameterize)
   (harlan middle languages)
   (harlan helpers)
   (only (elegant-weapons helpers) gensym)
   (harlan compile-opts)
   (elegant-weapons sets)
   (elegant-weapons graphs)
   (elegant-weapons graphviz)
   (nanopass))

  ;; This is the set of passes that transforms recursive functions
  ;; called from kernels so that they are no longer recursive.

  (define-pass extract-callgraph : M9 (m) -> M9.1 ()
    (definitions

      (define current-node '())
      (define cgraph '())

      (define (new-node! name)
        (unless (null? current-node)
          (set! cgraph (cons current-node cgraph)))
        (set! current-node (list name)))

      (define (add-call! name)
        (set-cdr! current-node (set-add (cdr current-node) name))))
    
    (Stmt : Stmt (b) -> Stmt ())
    
    (Decl
     : Decl (decl) -> Decl ()

     ((gpu-module ,[k*] ...)
      (new-node! '_)
      (let ((sccs (strongly-connected-components cgraph)))
        (if (dump-call-graph)
            (begin
              (if (file-exists? "call-graph.dot")
                  (delete-file "call-graph.dot"))
              (write-dot cgraph
                         sccs
                         (open-output-file "call-graph.dot"))))
        `(gpu-module (call-graph ,cgraph ,sccs) ,k* ...)))
     ((fn ,name (,x ...) ,[t] ,stmt)
      (new-node! name)
      `(fn ,name (,x ...) ,t ,(Stmt stmt))))

    (Expr
     : Expr (e) -> Expr ()
     ((call (var ,[t] ,x) ,[e*] ...)
      (add-call! x)
      `(call (var ,t ,x) ,e* ...))))

  ;; Okay, now we have our call graph. We'll use this to build a
  ;; with-labels form, which is basically a letrec.
  (define-pass insert-labels : M9.1 (m) -> M9.2 ()
    (definitions
      (define call-graph (make-parameter '()))
      (define sccs (make-parameter '()))
      (define current-labels (make-parameter '()))
      (define gpu-module (make-parameter '()))
      (define in-kernel (make-parameter #f))

      (define (scc-code)
        (filter (lambda (f)
                  (nanopass-case
                   (M9.1 Kernel) f
                   ((fn ,name (,x* ...) ,t ,stmt)
                    (memq name (current-labels)))
                   (else #f)))
                (gpu-module)))
      
      (define (find-scc name)
        (let loop ((sccs (sccs)))
          (if (null? sccs)
              '() ;; This really shouldn't happen...
              (if (memq name (car sccs))
                  (car sccs)
                  (loop (cdr sccs))))))
      
      (define (recursive? name)
        (or (memq name (cdr (assq name (call-graph))))
            (> 1 (length (find-scc name))))))

    (Stmt
     : Stmt (stmt) -> Stmt ()
     ((return (call (var ,t ,x) ,[e*] ...))
      (guard (memq x (current-labels)))
      `(call-label ,x ,e* ...)))

    (Expr
     : Expr (e) -> Expr ()
     ((call (var ,[t] ,x) ,[e*] ...)
      (guard (memq x (current-labels)))
      `(call-label ,x ,e* ...)))
          
    
    (Fn->CodeBlock
     : CommonDecl (cdecl) -> LabeledBlock ()
     ((fn ,name (,x* ...) (fn (,[t*] ...) ,-> ,[t]) ,[stmt])
      `(,name ((,x* ,t*) ...) ,stmt)))
    
    (Kernel
     : Kernel (k) -> Kernel ()

     ((fn ,name (,x* ...) (fn (,[t*] ...) ,-> ,[t]) ,stmt)
      (guard (recursive? name))
      (let ((x** (map gensym x*)))
        (parameterize ((current-labels (find-scc name)))
          `(fn ,name (,x** ...) (fn (,t* ...) -> ,t)
               (with-labels (,(map Fn->CodeBlock (scc-code)) ...)
                            (call-label ,name (var ,t* ,x**) ...)))))))
    
    (Decl
     : Decl (decl) -> Decl ()
     ((gpu-module (call-graph ,?0 ,?1) ,k* ...)
      (parameterize ((call-graph ?0)
                     (sccs ?1)
                     (gpu-module k*)
                     (in-kernel #t))
        `(gpu-module ,(map Kernel k*) ...)))))

  ;; Transforms the lower-labels form into something that's pretty
  ;; close to C.
  (define-pass lower-labels : M9.2 (m) -> M9.3 ()
    (definitions
      (define (arg-types name)
        (let loop ((names (label-names))
                   (args  (label-args))
                   (types (label-types)))
          (if (null? names)
              (values '() '())
              (if (eq? (car names) name)
                  (values (car args) (car types))
                  (loop (cdr names) (cdr args) (cdr types))))))
      
      (define label-names (make-parameter '()))
      (define label-args  (make-parameter '()))
      (define label-types (make-parameter '())))
    
    (Stmt
     : Stmt (stmt) -> Stmt ()
     ((call-label ,name ,[e*] ...)
      (let-values (((x* t*) (arg-types name)))
        (let ((x*^ (map gensym x*)))
          (let ((temps (map (lambda (x t e)
                              `(let ,x ,t ,e))
                            x*^ t* e*))
                (set-args (map (lambda (x x^ t)
                                 `(set! (var ,t ,x) (var ,t ,x^)))
                               x* x*^ t*)))
            `(begin ,(append temps set-args (list `(goto ,name))) ...))))))

    (Body
     : Body (body) -> Body ()
     ((with-labels ((,name ((,x* ,[t*]) ...) ,stmt*) ...) ,stmt)
      (parameterize ((label-names name)
                     (label-args  x*)
                     (label-types t*))
        (let ((arg-defs
               (map (lambda (x t)
                      `(let ,x ,t))
                    (apply append x*) (apply append t*)))
              (labels
               (apply append
                      (map (lambda (name stmt)
                             (list `(label ,name)
                                   (Stmt stmt)))
                           name stmt*)))
              (start (Stmt stmt)))
          `(begin ,(append arg-defs (list start) labels) ...))))))
  
  (define (remove-recursion module)
    (>::> module
          extract-callgraph
          insert-labels
          lower-labels))
  
  )
