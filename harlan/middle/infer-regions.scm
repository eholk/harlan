(library (harlan middle infer-regions)
  (export infer-regions)
  (import
    (rnrs)
    (cKanren mk)
    (cKanren ck)
    (cKanren tree-unify)
    (cKanren neq)
    (cKanren tracing)
    (harlan compile-opts))

;; Most important observation:
;; If x is on regions r1 and r2,
;; x is associated with the region list `(r1 r2)
;; had type (vec (vec <type>))
;; now has type (vec r1 (vec r2 <type>))
  
;; Switch this to expand to trace-define-mk when debugging.
(define-syntax define-mk
  (syntax-rules ()
    ((_ . rest) (define . rest))))

;; For a variable x with type t, fill all the "unassigned" regions
;; in its output type to be in the fallback region, r.  This extends
;; the environment.  The specialized constraint "fallback-region"
;; allows region variables to stay fresh until enforce-constraints,
;; where the correct region annotations will be added to any
;; still-fresh regions.
(define-mk (extend-with-region x t r env to r* envo)
  (fresh ()
    (== envo `((,x ,r* ,to) . ,env))
    (fill-type-with-backup-region t r to r*)))

(define-mk (fill-type-with-backup-region t r to r*)
  (conde
    ((scalar-type t)
     (== to t)
     (== r* `()))
    ((memory-type t)
     (fresh (r^ type type^ rest)
       (== t `(vec ,type))
       (== to `(vec ,r^ ,type^))
       (== r* `(,r^ . ,rest))
       (fallback-region r^ r)
       (fill-type-with-backup-region type r type^ rest)))
    ((fresh (t^ to^)
       (== t `(ptr ,t^))
       (== to `(ptr ,to^))
       (fill-type-with-backup-region t^ r to^ r*)))))

(define-mk (fill-type-with-region t r* to)
  (conde
    ((scalar-type t)
     (== to t)
     (== r* `()))
    ((memory-type t)
     (fresh (r type type^ rest)
       (== t `(vec ,type))
       (== to `(vec ,r ,type^))
       (== r* `(,r . ,rest))
       (fill-type-with-region type rest type^)))
    ((fresh (t^ to^)
       (== t `(ptr ,t^))
       (== to `(ptr ,to^))
       (fill-type-with-region t^ r* to^)))))

(define-mk (fallback-region x r)
  (goal-construct (fallback-region-c x r)))

(define (fallback-region-c x r)
  (lambdam@ (a : s c)
            (let ((x (walk x s))
                  (r (walk r s)))
              (cond
               ((var? x)
                ((update-c (build-oc fallback-region-c x r)) a))
               (else a)))))

(define (fallback-region-oc? oc)
  (eq? (oc->rator oc) 'fallback-region-c))

(define (gensym-suffix g)
  (let loop ((g (string->list (symbol->string g))))
    (cond
     ((member #\_ g) => (lambda (g^) (loop (cdr g^))))
     (else (string->number (list->string g))))))

(define (compare-fallback oc1 oc2)
  (let ((r1 (cadr (oc->rands oc1)))
        (r2 (cadr (oc->rands oc2))))
    (cond
     ((var? r1) #f)
     ((var? r2) #t)
     (else
      (<= (gensym-suffix r1)
          (gensym-suffix r2))))))

(define (sort-and-fallback c)
  (lambdag@ (a)
            (let ((c (list-sort compare-fallback c)))
              (let ((oc (car c)))
                (let ((rands (oc->rands oc)))
                  ((== (car rands) (cadr rands)) a))))))

(define-mk (enforce-filled-regions x)
  (let loop ()
    (lambdag@ (a : s c)
      (let-values (((c c^) (partition fallback-region-oc? c)))
        (cond
         ((null? c) (unitg a))
         (else
          ((fresh ()
             (sort-and-fallback c)
             (loop))
           (make-a s (append c c^)))))))))

;; helpers
(define-mk lookupo
  (lambda (x env r t)
    (fresh (env^)
      (conde
        ((== `((,x ,r ,t) . ,env^) env))
        ((fresh (y r^ t^)
           (== `((,y ,r^ ,t^) . ,env^) env)
           (=/= x y)
           (lookupo x env^ r t)))))))

(define-mk (memory-type t)
  (conde
    ((fresh (type)
       (== t `(vec ,type))))))

(define-mk (scalar-type t)
  (conde
    ((== t `ofstream))
    ((== t `int))
    ((== t `void))
    ((== t `bool))
    ((== t `u64))
    ((== t `float))
    ((== t `char))
    ((== t `str))))

(define-mk (membero x ls)
  (conde
    ((fresh (d)
       (== ls `(,x . ,d))))
    ((fresh (a d)
       (== ls `(,a . ,d))
       (=/= a x)
       (membero x d)))))

(define-mk (not-membero x ls)
  (conde
    ((== ls `()))
    ((fresh (a d)
       (== ls `(,a . ,d))
       (=/= a x)
       (not-membero x d)))))

;; grammar procedures
(define (infer-regions mod)
  (begin
    (extend-enforce-fns
     'regions enforce-filled-regions)
    (let ((result
           (run 2 (q) (infer-module mod q))))
      (case (length result)
        ((0)
         (error 'infer-regions
                "could not infer regions for program"
                mod))
        ((1) (car result))
        ((2)
         (error 'infer-regions
                "could not infer unique region assignment for program"
                mod))))))

(define-mk (infer-module mod modo)
  (fresh (decls declso rso)
    (== mod `(module . ,decls))
    (== modo `(module . ,declso))
    (infer-decls decls declso)))

(define-mk (infer-decls decls declso)
  (conde
    ((== decls `()) (== declso `()))
    ((fresh (decl declo rest resto)
       (== decls `(,decl . ,rest))
       (== declso `(,declo . ,resto))
       (infer-decl decl declo)
       (infer-decls rest resto)))))

(define-mk (infer-decl decl declo)
  (conde
    ((fresh (name stmt stmto args env
            in-r out-r arg-t rt)
       (== decl
           `(fn ,name ,args (,arg-t -> ,rt)
                (input-regions ,in-r)
                (output-regions ,out-r)
                ,stmt))
       (== declo
           `(fn ,name ,args (,arg-t -> ,rt)
                (input-regions ,in-r)
                (output-regions ,out-r)
                ,stmto))
       (infer-fn-args args arg-t in-r env)
       (infer-stmt stmt env out-r `dummy stmto)))
    ((fresh (body)
       (== decl `(extern . ,body))
       (== declo decl)))))

(define-mk (infer-fn-args args arg-t in-r env)
  (conde
    ((== args `())
     (== arg-t `())
     (== env `())
     (== in-r `()))
    ((fresh (a at res rest env^ r* r*^ ato)
       (== args `(,a . ,res))
       (== arg-t `(,at . ,rest))
       (== in-r `(,r* . ,r*^))
       (== env `((,a ,r* ,ato) . ,env^))
       (fill-type-with-region at r* ato)
       (infer-fn-args res rest r*^ env^)))))

(define-mk (infer-stmts stmts env outr* letr stmtso)
  (conde
    ((== stmts `())
     (== stmtso stmts))
    ((fresh (s s* so s*o v*)
       (== stmts `(,s . ,s*))
       (== stmtso `(,so . ,s*o))
       (infer-stmt s env outr* letr so)
       (infer-stmts s* env outr* letr s*o)))))

(define-mk (infer-stmt stmt env outr* letr stmto)
  (conde
    ((fresh (r^ s so)
       (== stmt `(let-region (,r^) ,s))
       (== stmto `(let-region (,r^) ,so))
       (=/= letr r^)
       (infer-stmt s env outr* r^ so)))
    ((fresh (msg)
       (== stmt `(error . ,msg))
       (== stmto `(error . ,msg))))
    ((fresh (triv trivo)
       (== stmt `(print ,triv))
       (== stmto `(print ,trivo))
       (infer-triv triv env `() trivo)))
    ((fresh (triv op trivo opo)
       (== stmt `(print ,triv ,op))
       (== stmto `(print ,trivo ,opo))
       (infer-triv triv env `()  trivo)
       (infer-triv op env `()  opo)))
    ((fresh (triv trivo)
       (== stmt `(assert ,triv))
       (== stmto `(assert ,trivo))
       (infer-triv triv env `() trivo)))
    ;; This implicitly enforces that every vector
    ;; effect must be from the same region
    ((fresh (x triv xo trivo r*)
       (== stmt `(set! ,x ,triv))
       (== stmto `(set! ,xo ,trivo))
       (infer-triv triv env r* trivo)
       (infer-triv x env r* xo)))
    ((fresh (d fv s so do fvo t)
       (== stmt `(kernel ,t ,d (free-vars . ,fv) ,s))
       (== stmto `(kernel ,do (free-vars . ,fvo) ,so))
       (infer-kernel-free-vars fv env fvo)
       (infer-kernel-dims d env do)
       (infer-stmt s env outr* letr so)))
    ((fresh (b s bo so envo)
       (== stmt `(let ,b ,s))
       (== stmto `(let ,bo ,so))
       (infer-let-bindings b env letr bo envo)
       (infer-stmt s envo outr* letr so)))
    ((fresh (stmt* stmt*o)
       (== stmt `(begin . ,stmt*))
       (== stmto `(begin . ,stmt*o))
       (infer-stmts stmt* env outr* letr stmt*o)))
    ((fresh (t c to co)
       (== stmt `(if ,t ,c))
       (== stmto `(if ,to ,co))
       (infer-triv t env `() to)
       (infer-stmt c env outr* letr co)))
    ((fresh (t c a to co ao)
       (== stmt `(if ,t ,c ,a))
       (== stmto `(if ,to ,co ,ao))
       (infer-triv t env `() to)
       (infer-stmt c env outr* letr co)
       (infer-stmt a env outr* letr ao)))
    ((fresh (triv trivo)
       (conde
         ((== stmt `(return ,triv))
          (== stmto `(return ,trivo))
          (infer-triv triv env outr* trivo))
         ((== stmt `(return))
          (== stmto stmt)))))
    ((fresh (triv trivo r*)
       (== stmt `(do ,triv))
       (== stmto `(do ,trivo))
       (infer-triv triv env r* trivo)))
    ((fresh (x s so start end step
               envo starto endo stepo)
       (== stmt `(for (,x ,start ,end ,step) ,s))
       (== stmto `(for (,x ,starto ,endo ,step) ,so))
       (== envo `((,x () int) . ,env))
       (infer-triv start env `() starto)
       (infer-triv end env `() endo)
       (infer-triv step env `() stepo)
       (infer-stmt s envo outr* letr so)))
    ((fresh (triv s trivo so)
       (== stmt `(while ,triv ,s))
       (== stmto `(while ,trivo ,so))
       (infer-triv triv env `() trivo)
       (infer-stmt s env outr* letr so)))))

(define-mk (infer-lifted-expr expr env r* expro)
  (conde
    ((fresh (t triv trivo r to rest)
       (== expr `(make-vector ,t ,triv))
       (== expro `(make-vector ,to ,trivo))
       (== r* `(,r . ,rest))
       (fill-type-with-region t rest to)
       (infer-triv triv env `() trivo)))
    ((infer-triv expr env r* expro))))

(define-mk (infer-triv triv env r* trivo)
  (conde
    ((fresh (c i u f b s)
       (conde
         ((== triv `(char ,c)))
         ((== triv `(int ,i)))
         ((== triv `(u64 ,u)))
         ((== triv `(float ,f)))
         ((== triv `(bool ,b))) 
         ((== triv `(str ,s))))
       (== r* `())
       (== trivo triv)))
    ((fresh (x t t^)
       (== triv `(var ,t ,x))
       (== trivo `(var ,t^ ,x))
       (lookupo x env r* t^)))
    ((fresh (t to)
       (== triv `(int->float ,t))
       (== trivo `(int->float ,to))
       (== r* `())
       (infer-triv t env `() to)))
    ((fresh (t c a to co ao)
       (== triv `(if ,t ,c ,a))
       (== trivo `(if ,to ,co ,ao))
       (infer-triv t env `() to)
       (infer-triv c env r* co)
       (infer-triv a env r* ao)))
    ((fresh (rto rt argt argto fn-call
                 t x fn arg* arg*o argr*)
       ;; (call (var ((int) -> (vec int)) foo) (int 5))
       ;; asked to _be on_ regions `(r), would be
       ;; (call (var ((int) -> (vec r int)) foo) (int 5))
       ;; without binding that region to other instances of foo
       (== triv `(call ,fn-call . ,arg*))
       (conde
         ((== fn-call `(var (,argt -> ,rt) ,fn))
          (== trivo `(call (var (,argto -> ,rto) ,fn) . ,arg*o))
          (infer-call-fn-args
           arg* argt argr* env argto arg*o)
          (fill-type-with-region rt r* rto))
         ((== fn-call `(c-expr ,t ,x))
          (== trivo `(call ,fn-call . ,arg*))))))
    ((fresh (t x)
       (== triv `(c-expr ,t ,x))
       (== trivo `(c-expr ,t ,x))
       ;; This enforces that regions can't be sent off to external/C
       ;; functions that might not know what's up
       (== r* `())))
    ((fresh (t v i to vo io r^)
       (== triv `(vector-ref ,t ,v ,i))
       (== trivo `(vector-ref ,to ,vo ,io))
       (fill-type-with-region t r* to)
       (infer-triv v env `(,r^ . ,r*) vo)
       (infer-triv i env `() io)))
    ((fresh (t to r*^)
       (== triv `(length ,t))
       (== trivo `(length ,to))
       (== r* `())
       (infer-triv t env r*^ to)))
    ((fresh (t to)
       (== triv `(addressof ,t))
       (== trivo `(addressof ,to))
       (infer-triv t env r* to)))
    ((fresh (t to)
       (== triv `(deref ,t))
       (== trivo `(deref ,to))
       (infer-triv t env r* to)))
    ((fresh (op t1 t2 t1o t2o)
       (== triv `(,op ,t1 ,t2))
       (== trivo `(,op ,t1o ,t2o))
       (conde
         ((binop op))
         ((relop op)))
       (== r* `())
       (infer-triv t1 env `() t1o)
       (infer-triv t2 env `() t2o)))))

(define-mk (infer-call-fn-args
            arg* argt argr* env argto arg*o) 
  (conde
    ((== arg* `())
     (== argt `())
     (== argr* `())
     (== argto `())
     (== arg*o `()))
    ((fresh (a d t dt r* ro to dto ao do)
       (== arg* `(,a . ,d))
       (== argt `(,t . ,dt))
       (== argr* `(,r* . ,ro))
       (== argto `(,to . ,dto))
       (== arg*o `(,ao . ,do))
       (fill-type-with-region t r* to)
       (infer-triv a env r* ao)
       (infer-call-fn-args d dt ro env dto do)))))

(define-mk (binop op)
  (conde
    ((== op '+))
    ((== op '-))
    ((== op '*))
    ((== op 'mod))
    ((== op '/))))

(define-mk (relop op)
  (conde
    ((== op '<))
    ((== op '<=))
    ((== op '=))
    ((== op '>))
    ((== op '>=))))

(define-mk (infer-let-bindings b env letr bo envo)
  (conde
    ((== b `())
     (== bo b)
     (== envo env))
    ((fresh (x t e rest env^ resto eo to r*)
       (== b `((,x ,t ,e) . ,rest))
       (== bo `((,x ,to ,eo) . ,resto))
       (extend-with-region x t letr env to r* env^)
       (infer-lifted-expr e env r* eo)
       (infer-let-bindings
        rest env^ letr resto envo)))))

(define-mk (infer-kernel-dims d env do)
  (conde
    ((== d `()) (== do `()))
    ((fresh (dim d^ dimo d^o)
       (== d `(,dim . ,d^))
       (== do `(,dimo . ,d^o))
       (infer-triv dim env `() dimo)
       (infer-kernel-dims d^ env d^o)))))

(define-mk (infer-kernel-free-vars fv env fvo)
  (conde
    ((== fv `()) (== fvo `()))
    ((fresh (x t rest to resto r*)
       (== fv `((,x ,t) . ,rest))
       (== fvo `((,x ,to) . ,resto))
       (lookupo x env r* to)
       (infer-kernel-free-vars rest env resto)))))

)
