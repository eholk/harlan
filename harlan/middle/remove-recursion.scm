(library
    (harlan middle remove-recursion)
  (export remove-recursion)
  (import
   (rnrs)
   (rnrs mutable-pairs)
   (harlan middle languages)
   (harlan helpers)
   (only (elegant-weapons helpers) gensym)
   (util compat)
   (harlan compile-opts)
   (elegant-weapons sets)
   (elegant-weapons lists)
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
        (assert (not (null? current-node)))
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

    (Kernel
     : Kernel (k) -> Kernel ()

     ((kernel ,x ((,x* ,[t*]) ...) ,stmt)
      (new-node! x)
      `(kernel ,x ((,x* ,t*) ...) ,(Stmt stmt)))

     ((fn ,name (,x* ...) ,[t] ,stmt)
      (new-node! name)
      `(fn ,name (,x* ...) ,t ,(Stmt stmt))))
    
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
              (begin
                 ;; This really shouldn't happen...
                (display "Could not find label for ") (display name) (newline)
                '())
              (if (memq name (car sccs))
                  (car sccs)
                  (loop (cdr sccs))))))
      
      (define (recursive? name)
        (let ((node (assq name (call-graph))))
          (if (not node)
              (error 'recursive "could not find function in call graph"
                     name (call-graph)))
          (or (memq name (cdr node))
              (let ((scc (find-scc name)))
                ;;(display (length scc))
                (> (length scc) 1))))))

    (Expr
     : Expr (e) -> Expr ()
     ((call (var (fn (,t* ...) ,-> ,[t]) ,x) ,[e*] ...)
      (guard (memq x (current-labels)))
      `(call-label ,x ,t ,e* ...)))
          
    
    (Fn->CodeBlock
     : CommonDecl (cdecl) -> LabeledBlock ()
     ((fn ,name (,x* ...) (fn (,[t*] ...) ,-> ,[t]) ,[stmt])
      `(,name ((,x* ,t*) ...) ,stmt)))
    
    (Kernel
     : Kernel (k) -> Kernel ()

     ((fn ,name (,x* ...) (fn (,[t*] ...) ,-> ,[t]) ,stmt)
      (guard (parameterize ((current-labels (find-scc name)))
               (recursive? name)))
      (let ((x** (map gensym x*)))
        (parameterize ((current-labels (find-scc name)))
          `(fn ,name (,x** ...) (fn (,t* ...) -> ,t)
               (with-labels (,(map Fn->CodeBlock (scc-code)) ...)
                            (return (call-label ,name ,t
                                                (var ,t* ,x**) ...))))))))
    
    (Decl
     : Decl (decl) -> Decl ()
     ((gpu-module (call-graph ,?0 ,?1) ,k* ...)
      (parameterize ((call-graph ?0)
                     (sccs ?1)
                     (gpu-module k*)
                     (in-kernel #t))
        `(gpu-module ,(map Kernel k*) ...)))))

  (define-pass lift-call-label : M9.2 (m) -> M9.2 ()
    (definitions
      (define label-calls (make-parameter '()))

      (define (push-call! x t e)
        (label-calls
         (with-output-language
          (M9.2 Stmt)
          (cons*
           `(let ,x ,t)
           `(set! (var ,t ,x) ,e)
           (label-calls))))))

    (Body
     : Body (body) -> Body ()

     ((return ,e)
      (parameterize ((label-calls '()))
        (let* ((e (Expr e))
               (labels (label-calls)))
          (if (null? labels)
              `(return ,e)
              `(begin ,(append labels (list `(return ,e))) ...))))))

    (Stmt
     : Stmt (stmt) -> Stmt ()

     ((return ,e)
      (parameterize ((label-calls '()))
        (let* ((e (Expr e))
               (labels (label-calls)))
          (if (null? labels)
              `(return ,e)
              `(begin ,(append labels (list `(return ,e))) ...))))))
    
    (Expr
     : Expr (e) -> Expr ()
     ((call-label ,name ,[t] ,[e*] ...)
      (let ((x (gensym name)))
        (push-call! x t `(call-label ,name ,t ,e* ...))
        `(var ,t ,x)))))

  ;; By some weird accident, what we have now works for at least one
  ;; tail call. Unfortunately, it doesn't work anywhere close to in
  ;; general.
  ;;
  ;; At this point, we've lifted things into a pretty reasonable
  ;; place. Things being call-label expressions.
  ;;
  ;; In the next passes, we'll want to detect call-live variables,
  ;; create a stack, push these onto the stack before calls and pop
  ;; them off the stack afterwards.
  ;;
  ;; For the return point, we can insert a label (with an index so we
  ;; can dynamically jump to it) right after the call-label
  ;; expression. Then we'll create a dispatch function with a big
  ;; chain of ifs (or maybe a switch statement) that jumps to the
  ;; appropriate label upon return.
  ;;
  ;; It might be simpler to just assume all in-scope variables are
  ;; still live, but this is about as hard given that we've unnested
  ;; lets by this point.

  (define-pass detect-call-live : M9.2 (m) -> M9.2.1 ()
    ;; This is basically a fold-right over sequences of statments. We
    ;; have a couple of cases.
    ;;
    ;; 1. Variable references add things to the live set. We find this
    ;; in expressions.
    ;;
    ;; 2. set! kills things in the live set.
    ;;
    ;; 3. call-labels (also found in expressions), which are where we
    ;; add the live set annotations.
    ;;
    ;; A lot of the code is going to be threading live sets through
    ;; everything.

    (definitions

      (define (kill x x* t*)
        (fold-right-values
         ((x^ '()) (t^ '()) <- (x* x*) (t* t*))
         (if (eq? x* x)
             (values x^ t^)
             (values (cons x* x^)
                     (cons t* t^)))))
      
      (define (union-live x1 t1 x2 t2)
        (fold-right-values
         ((x* x2) (t* t2) <- (x x1) (t t1))
         (if (memq x x2)
             (values x* t*)
             (values (cons x x*) (cons t t*)))))

      (define (union-live* x* t*)
        (fold-right-values
         ((x^ '()) (t^ '()) <- (x x*) (t t*))
         (union-live x t x^ t^)))

      (define (union-live/e e x1 t1 x2 t2)
        (let-values (((x t) (union-live x1 t1 x2 t2)))
          (values e x t)))

      (define (union-live*/e e x t)
        (let-values (((x t) (union-live* x t)))
          (values e x t))))

    (LabeledBlock
     : LabeledBlock (lbl) -> LabeledBlock ()
     ((,name ((,x ,[t]) ...) ,[stmt '() '() -> stmt live-x live-t])
      `(,name ((,x ,t) ...) ,stmt)))
    
    (Expr
     : Expr (e [live-x '()] [live-t '()]) -> Expr ('() '())

     ((cast ,[t] ,[e x* t*])
      (values `(cast ,t ,e) x* t*))
     ((deref ,[e x t]) (values `(deref ,e) x t))
     ((addressof ,[e x t]) (values `(addressof ,e) x t))
     ((region-ref ,[t] ,[e1 x1 t1] ,[e2 x2 t2])
      (union-live/e `(region-ref ,t ,e1 ,e2)
                    x1 t1
                    x2 t2))
     ((vector-ref ,[t] ,[e1 x1 t1] ,[e2 x2 t2])
      (union-live/e `(vector-ref ,t ,e1 ,e2)
                    x1 t1
                    x2 t2))
     ((call ,[e x t] ,[e* x* t*] ...)
      (let-values (((x* t*) (union-live* x* t*)))
        (union-live/e `(call ,e ,e* ...)
                      x  t
                      x* t*)))
     ((call ,[e x t] ,[e* x* t*] ...)
      (let-values (((x* t*) (union-live* x* t*)))
        (union-live/e `(call ,e ,e* ...) x t x* t*)))
     ((call-label ,name ,[t] ,[e live-x^ live-t^] ...)
      (let-values (((live-x^ live-t^) (union-live* live-x^ live-t^)))
        (values `(call-label ,name ,t ((,live-x ,live-t) ...) ,e ...)
                live-x^ live-t^)))
     ((region-ref ,[t] ,[e0 live-x0 live-t0] ,[e1 live-x1 live-t1])
      (union-live/e `(region-ref ,t ,e0 ,e1)
                    live-x0 live-t0
                    live-x1 live-t1))
     ((alloc ,[e0 live-x0 live-t0] ,[e1 live-x1 live-t1])
      (union-live/e `(alloc ,e0 ,e1)
                    live-x0 live-t0
                    live-x1 live-t1))
     ((,op ,[e1 x1 t1] ,[e2 x2 t2])
      (union-live/e `(,op ,e1 ,e2)
                    x1 t1 x2 t2))
     ((field ,[e x t] ,name)
      (values `(field ,e ,name) x t))
     ((var ,[t] ,x)
      (values `(var ,t ,x) (cons x live-x) (cons t live-t))))

    (ExprTrap
     : Expr (e) -> Expr ()
     (else (error 'ExprTrap "We don't want to use this transformer"
                  (unparse-M9.2 e))))
    (StmtTrap
     : Stmt (stmt) -> Stmt ()
     (else (error 'StmtTrap "We don't want to use this transformer"
                  (unparse-M9.2 stmt))))
    
    (Type : Rho-Type (t) -> Rho-Type ())

    (Kernel
     : Kernel (k) -> Kernel ()
     ((kernel ,x ((,x* ,[t*]) ...) ,[stmt live-x live-t])
      `(kernel ,x ((,x* ,t*) ...) ,stmt)))

    (Stmt
     : Stmt (stmt [live-x '()] [live-t '()]) -> Stmt (live-x live-t)

     ((begin ,stmt ...)
      (let-values (((stmt* live-x live-t)
                    (fold-right-values
                     ((stmt* '()) (live-x live-x) (live-t live-t)
                      <- (stmt stmt))
                     (begin
                       (let-values (((stmt live-x live-t)
                                     (Stmt stmt live-x live-t)))
                         (values (cons stmt stmt*) live-x live-t))))))
        (values `(begin ,stmt* ...) live-x live-t)))
     ((do ,[e x t])
      (union-live/e `(do ,e) x t live-x live-t))
     ((let ,x ,[t] ,e)
      (let-values (((live-x live-t) (kill x live-x live-t)))
        (let-values (((e x* t*) (Expr e live-x live-t)))
          (values `(let ,x ,t ,e)
                  x* t*))))
     ((assert ,[e x t])
      (union-live/e `(assert ,e)
                    x t
                    live-x live-t))

     ((set! (var ,[t] ,x) ,e)
      (let-values (((live-x live-t) (kill x live-x live-t)))
        (let-values (([e x* t*] (Expr e live-x live-t)))
          (let-values (((x* t*) (union-live x* t* live-x live-t)))
            (values `(set! (var ,t ,x) ,e)
                    x* t*)))))
     
     ((set! ,[e1 x1 t1] ,[e x* t*])
      (let-values (((x* t*) (union-live x* t* live-x live-t)))
        (let-values (((x* t*) (union-live x* t* x1 t1)))
          (values `(set! ,e1 ,e)
                  x* t*))))

     ;; This is wrong.
     ((if ,[e x1 t1]
          ,[stmt1 live-x live-t -> stmt1 x2 t2]
          ,[stmt2 live-x live-t -> stmt2 x3 t3])
        (let-values (((x* t*) (union-live x2 t2 x1 t1)))
          (let-values (((x* t*) (union-live x3 t3 x* t*)))
            (values `(if ,e ,stmt1 ,stmt2) x* t*))))
     ((if ,[e x1 t1]
          ,[stmt1 live-x live-t -> stmt1 x2 t2])
        (let-values (((x* t*) (union-live x2 t2 x1 t1)))
            (values `(if ,e ,stmt1) x* t*)))
     ((for (,x ,[e1 x1 t1] ,[e2 x2 t2] ,[e3 x3 t3])
        ,[stmt live-x live-t -> stmt x* t*])
      (let-values (((x* t*) (kill x x* t*)))
        (let-values (((x* t*) (union-live x* t* x1 t1)))
          (let-values (((x* t*) (union-live x* t* x2 t2)))
            (let-values (((x* t*) (union-live x* t* x2 t2)))
              (values `(for (,x ,e1 ,e2 ,e3) ,stmt)
                      x* t*))))))
     ((error ,x) (values `(error ,x) live-x live-t))
     ((return)
      (values `(return) '() '()))
     ((return ,[e x* t*])
      ;; we ignore the passed-in live-x and live-t, since return
      ;; obviously kills anything that comes after it.
      (values `(return ,e)
              x* t*))
     ((let ,x ,[t])
      (let-values (((live-x live-t) (kill x live-x live-t)))
        (values `(let ,x ,t) live-x live-t)))
     ((print ,[e x t])
      (union-live/e `(print ,e) x t live-x live-t))
     ((print ,[e0 x0 t0] ,[e1 x1 t1])
      (let-values (((live-x live-t) (union-live x0 t0 live-x live-t)))
        (union-live/e `(print ,e0 ,e1) x1 t1 live-x live-t)))
     ((while ,[e x t] ,[stmt live-x live-t -> stmt live-x live-t])
      (union-live/e `(while ,e ,stmt) x t live-x live-t))
     
     (else (error 'Stmt "What is this?" (unparse-M9.2 stmt))))

    (Body
     : Body (body) -> Body ()
     ((with-labels (,[lbl] ...) ,[stmt '() '() -> stmt x t])
      `(with-labels (,lbl ...) ,stmt))
     (,stmt
      (let-values (((stmt x t)
                    (Stmt stmt '() '())))
        stmt))))

  ;; for non-tail labels, push all the call-lives to the stack, then
  ;; the return address. Upon return, pop off the return value and the
  ;; call-lives.
  (define-pass apply-calling-conventions : M9.2.1 (m) -> M9.2.2 ()
    (definitions

      (define (type-of e)
        (nanopass-case
         (M9.2.2 Expr) e
         ((int ,i) 'int)
         ((bool ,b) 'bool)
         ((var ,t ,x) t)
         ((call (var (fn (,x* ...) ,-> ,t) ,x) ,e* ...) t)
         ((,op ,e1 ,e2) (type-of e1))
         (else (error 'type-of "I don't know how to find this type"
                      (unparse-M9.2.2 e)))))
      
      (define next-index (let ((i -1))
                           (lambda ()
                             (set! i (+ 1 i))
                             i)))
      (define preserve-return (make-parameter #f))
      (define dispatch-name (make-parameter #f))
      (define stack-base (make-parameter #f))
      (define stack-pointer (make-parameter #f))
      (define new-labels (make-parameter '()))

      (define (build-dispatch)
        (with-output-language
         (M9.2.2 LabeledBlock)
         (let ((x (gensym 'x)))
           `(,(dispatch-name) ((,x int))
             ,(let loop ((lbl (new-labels)))
                (if (null? lbl)
                    `(do (int 42)) ;; badness ensues.
                    `(if (= (var int ,x) (int ,(caar lbl)))
                         (do (call-label ,(cdar lbl)))
                         ,(loop (cdr lbl))))))))))

    (Stmt
     : Stmt (stmt) -> Stmt ()
     ((return ,[e]) (guard (and (stack-base) (not (preserve-return))))
      (let ((ra (gensym 'return_address)))
      `(begin
         (let ,ra int)
         (pop! ,(stack-base) ,(stack-pointer) int (var int ,ra))
         (push! ,(stack-base) ,(stack-pointer) ,(type-of e) ,e)
         (do (call-label ,(dispatch-name) (var int ,ra))))))
     ((return) (guard (and (stack-base) (not (preserve-return))))
      (let ((ra (gensym 'return_address)))
      `(begin
         (let ,ra int)
         (pop! ,(stack-base) ,(stack-pointer) int (var int ,ra))
         (do (call-label ,(dispatch-name) (var int ,ra))))))
     ((set! ,[e] (call-label ,name ,[t] ((,x* ,[t*]) ...) ,[e*] ...))
      `(begin
         ,(let loop ((x* x*)
                     (t* t*)
                     (k '()))
            (if (null? x*)
                (let ((i (next-index))
                      (lbl (gensym 'return)))
                  (new-labels (cons (cons i lbl)
                                    (new-labels)))
                  (cons*
                   ;; push the return location
                   `(push! ,(stack-base) ,(stack-pointer) int (int ,i))
                   ;; call the label
                   `(do (call-label ,name ,e* ...))
                   ;; The return location
                   `(label ,lbl)
                   ;; pop the return value
                   `(pop! ,(stack-base) ,(stack-pointer)
                          ,t
                          ,e)
                   k))
                (let ((x (car x*))
                      (t (car t*)))
                  (cons `(push! ,(stack-base) ,(stack-pointer)
                                ,t (var ,t ,x))
                        (loop (cdr x*)
                              (cdr t*)
                              (cons `(pop! ,(stack-base)
                                           ,(stack-pointer)
                                           ,t
                                           (var ,t ,x))
                                    k)))))) ...)))

    (Body
     : Body (body) -> Body ()
     ((with-labels ((,name ((,x* ,[t*]) ...) ,stmt*) ...) ,stmt)
      (let ((stack (gensym 'stack))
            (sp (gensym 'sp))
            (stack-size 512))
        (parameterize ((new-labels '())
                       (dispatch-name (gensym 'dispatch))
                       (stack-base (with-output-language
                                    (M9.2.2 Expr)
                                    ;; TODO: make the stack size
                                    ;; controllable.
                                    `(var (fixed-array char ,stack-size)
                                          ,stack)))
                       (stack-pointer (with-output-language
                                       (M9.2.2 Expr)
                                       `(var int ,sp))))
          (let* ((stmt* (map Stmt stmt*))
                 (lbls (with-output-language
                        (M9.2.2 Body)
                        (parameterize ((preserve-return #t))
                          `(with-labels (,(build-dispatch)
                                         (,name ((,x* ,t*) ...) ,stmt*) ...)
                                        ,(Stmt stmt)))))
                 (stuff (list
                         `(let ,stack (fixed-array char ,stack-size)
                               (empty-struct))
                         `(let ,sp int (int 0)))))
            `(seq
              ,stuff ...
              ,lbls)))))))
  
  ;; Transforms the with-labels form into something that's pretty
  ;; close to C.
  (define-pass lower-labels : M9.2.2 (m) -> M9.3 ()
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
     ((push! ,[e0] ,[e1] ,[t] ,[e2])
      `(begin
         (set! (deref (cast (ptr private ,t) (+ ,e0 ,e1))) ,e2)
         (set! ,e1 (+ ,e1 (sizeof ,t)))))
     ((pop! ,[e0] ,[e1] ,[t] ,[e2])
      `(begin
         (set! ,e1 (- ,e1 (sizeof ,t)))
         (set! ,e2 (deref (cast (ptr private ,t) (+ ,e0 ,e1))))))
      
     ((do (call-label ,name ,[e*] ...))
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
     ((seq ,[stmt] ... ,[body])
      `(begin ,stmt ... ,body))
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
          lift-call-label
          detect-call-live
          apply-calling-conventions
          lower-labels))
  
  )
