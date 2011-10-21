(library
  (typecheck)
  (export
    typecheck
    infer-kernel
    infer-stmt
    infer-expr
    lookup)
  (import
   (rnrs)
   (util mk))

;;; ********************************* FIX ME !!!! ****
  
;;; need to add examples with multiple fn's which call each other.
  
;;; add tests with recursive and mutually recursive calls
  
(define pairo
  (lambda (x)
    (fresh (a d)
      (== `(,a . ,d) x))))

(define lookup
  (lambda (env x type)
    (fresh (y t env^)
      ;; use conda instead of conde
      ;; to handle shadowing
      (conda
        ((== `((,x . ,type) . ,env^) env))
        ((== `((,y . ,t) . ,env^) env)
         (lookup env^ x type))))))

;; ensures that all expressions have the same type
(define infer-exprs
  (lambda (exprs env type exprso)
    (fresh (expr expro expr* expro*)
      (conde
        ((== exprs '())
         (== exprso '())
         (== type type))
        ((== exprs `(,expr . ,expr*))
         (== exprso `(,expro . ,expro*))
         (infer-expr expr env type expro)
         (infer-exprs expr* env type expro*))))))

(define infer-expr
  (lambda (expr env type expro)
    (conde
      ((fresh ()
         (== expr '(time))
         (== expro '(time))
         (== type 'u64)))
      ((fresh (n)
         (== expr `(num ,n))
         (conda
          ((== expro `(int ,n))
           (== type 'int))
          ((== expro `(u64 ,n))
           (== type 'u64))
          ((== expro `(float ,n))
           (== type 'float)))))
      ((fresh (n)
         (== expr `(float ,n))
         (== expro `(float ,n))
         (== type 'float)))
      ((fresh (str)
         (== expr `(str ,str))
         (== expro `(str ,str))
         (== type 'str)))
      ((fresh (x)
         (== expr `(var ,x))
         (== expro `(var ,type ,x))    
         (lookup env x type)))
      ((fresh (e* e^* t n)
         (== expr `(vector . ,e*))
         (== expro `(vector . ,e^*))
         (infer-exprs e* env t e^*)
         (project (e*)
                  (== (length e*) n))
         (== type `(vector ,t ,n))))
      ((fresh (c t)
         (== expr `(make-vector (num ,c)))
         (== expro `(make-vector ,t (int ,c)))
         (== type `(vector ,t ,c))))
      ((fresh (op e e^ t n)
         (== expr `(reduce ,op ,e))
         (== expro `(reduce ,type ,op ,e^))
         (infer-expr e env t e^)
         (conde
           ((== '+ op)
            (== 'int type)
            (== `(vector ,type ,n) t)))))
      ((fresh (e e^ t n)
         (== expr `(length ,e))
         (== expro `(length ,e^))
         (== type 'int)
         (infer-expr e env `(vector ,t ,n) e^)))
      ((fresh (c t)
         (== expr `(iota (num ,c)))
         (== expro `(iota (int ,c)))
         (== type `(vector int ,c))))
      ((fresh (e1 e2 e1^ e2^ t)
         (== expr `(= ,e1 ,e2))
         (== expro `(= ,e1^ ,e2^))
         (== type 'bool)
         (infer-expr e1 env t e1^)
         (infer-expr e2 env t e2^)))
      ((fresh (e1 e2 e1^ e2^ op)
         (== expr `(,op ,e1 ,e2))
         (== expro `(,op ,e1^ ,e2^))
         (conde
           ((== op '+))
           ((== op '-))
           ((== op '*))
           ((== op 'mod))
           ((== op '/)))
         (conde
           ((== type 'int))
           ((== type 'u64))
           ((== type 'float)))
         (infer-expr e1 env type e1^)
         (infer-expr e2 env type e2^)))
      ((fresh (e e^)
         (== expr `(int->float ,e))
         (== expro `(int->float ,e^))
         (== type 'float)
         (infer-expr e env 'int e^)))
      ((fresh (ve ie ve^ ie^ n)
         (== expr `(vector-ref ,ve ,ie))
         (== expro `(vector-ref ,type ,ve^ ,ie^))          
         (infer-expr ie env 'int ie^)
         (infer-expr ve env `(vector ,type ,n) ve^)))
      ((fresh (fn args arg-types rtype argso)
         (== expr `(call ,fn . ,args))
         (lookup env fn `(,arg-types -> ,rtype))
         (infer-args env args arg-types argso)
         (== expro `(call ,rtype ,fn . ,argso))))
      ((fresh (b* body* b^* body*^ env^ n)
         (== expr `(kernel ,b* . ,body*))
         (== expro `(kernel ,type ,b^* . ,body*^))
         (infer-kernel b* b^* body* body*^ env type n))))))

(define infer-args
  (lambda (env args arg-types argso)
    (conde
      ((== args '()) (== argso '()))
      ((fresh (e e* t t* e^ e^*)
         (== args `(,e . ,e*))
         (== arg-types `(,t . ,t*))
         (== argso `(,e^ . ,e^*))
         (infer-expr e env t e^)
         (infer-args env e* t* e^*))))))

(define infer-kernel
  (lambda (b* b^* body* body*^ env type n)
    (conde
      ((== '() b*)
       (== '() b^*)
       (conde
         ((fresh (body body^ type^)
            (== `(,body) body*)
            (== `(,body^) body*^)
            (infer-expr body env type^ body^)
            (== `(vector ,type^ ,n) type)))
         ((fresh (body body^ rest rest^ t env^)
            (== `(,body . ,rest) body*)
            (pairo rest)
            (== `(,body^ . ,rest^) body*^)
            ;; using infer-stmt instead of infer-expr
            ;; (a little weird)
            (infer-stmt body env env^ t body^)
            (infer-kernel b* b^* rest rest^ env^ type n)))))
      ((fresh (x e e^ tx te rest rest^)
         (== `((,x ,e) . ,rest) b*)
         (infer-expr e env te e^)
         (== `(vector ,tx ,n) te)
         (== `(((,x ,tx) (,e^ ,te)) . ,rest^) b^*)
         (infer-kernel rest rest^ body* body*^ `((,x . ,tx) . ,env) type n))))))

(define infer-stmt
  (lambda (stmt env envo rtype stmto)
    (fresh (e e^)
      (conde
        ((fresh (x t)
           (== stmt `(let ,x ,e))
           (== stmto `(let ,x ,t ,e^))
           (== envo `((,x . ,t) . ,env))
           (infer-expr e env t e^)))
        ((fresh (e1 e2 e1^ e2^ t1 t2)
           (== stmt `(set! ,e1 ,e2))
           (== stmto `(set! ,e1^ ,e2^))
           (== rtype 'void)
           (== env envo)            
           (infer-expr e1 env t1 e1^)
           (infer-expr e2 env t2 e2^)))
        ((fresh (e1 e2 e3 e1^ e2^ e3^ t n)
           (== stmt `(vector-set! ,e1 ,e2 ,e3))
           (== stmto `(vector-set! ,t ,e1^ ,e2^ ,e3^))
           (== rtype 'void)
           (== env envo)
           (infer-expr e1 env `(vector ,t ,n) e1^)
           (infer-expr e2 env 'int e2^)
           (infer-expr e3 env t e3^)))
        ((== stmt `(print ,e))
         (== stmto `(print ,e^))
         (== rtype 'void)
         (== env envo)
         (fresh (rtype^)
           (infer-expr e env rtype^ e^)))
        ((fresh (e1 e2 e1^ e2^)
           (== stmt `(print ,e1 ,e2))
           (== stmto `(print ,e1^ ,e2^))
           (== rtype 'void)
           (== env envo)
           (fresh (rtype1^ rtype2^)
             (infer-expr e1 env rtype1^ e1^)
             (infer-expr e2 env rtype2^ e2^))))
        ((== stmt `(assert ,e))
         (== stmto `(assert ,e^))
         (== rtype 'void)
         (== env envo)
         (infer-expr e env 'bool e^))
        ((== stmt `(return ,e))
         (== stmto `(return ,e^))
         (== env envo)
         (infer-expr e env rtype e^))
        ((fresh (e e^ t)
           (== stmt `(do ,e))
           (== stmto `(do ,e^))
           (== env envo)
           (infer-expr e env t e^)))
        ((fresh (x start start^ end end^ stmt* stmt*^ te)
           (== stmt `(for (,x ,start ,end) . ,stmt*))
           (== stmto `(for ((,x int) ,start^ ,end^) . ,stmt*^))
           (== env envo)
           (== te 'int)
           (infer-expr start env te start^)
           (infer-expr end env te end^)
           (infer-stmts stmt* `((,x . ,te) . ,env) rtype stmt*^)))
        ((fresh (x y x^  y^ stmt* stmt*^ te relop)
           (== stmt `(while (,relop ,x ,y) . ,stmt*))
           (== stmto `(while (,relop  ,x^ ,y^) . ,stmt*^))
           (== env envo)
           (== te 'int)
           (infer-expr x env te x^)
           (infer-expr y env te y^)
           (infer-stmts stmt* env rtype stmt*^)))
        ))))

(define infer-stmts
  (lambda (stmts env rtype stmtso)
    (conde
      ((== stmts '()) (== stmts stmtso))
      ((fresh (stmt env^ stmto)
         (== stmts `(,stmt))
         (infer-stmt stmt env env^ rtype stmto)
         (== stmtso `(,stmto))))
      ((fresh (stmt stmt* env^ stype stmto stmtso^ e)
         (== stmts `(,stmt . ,stmt*))
         (pairo stmt*)
         (conda
           ((== stmt `(return ,e))
            (infer-stmt stmt env env^ rtype stmto)
            (infer-stmts stmt* env^ stype stmtso^)
            (== stmtso `(,stmto . ,stmtso^)))
           ((infer-stmt stmt env env^ stype stmto)
            (infer-stmts stmt* env^ rtype stmtso^)
            (== stmtso `(,stmto . ,stmtso^)))))))))

(define infer-fn
  (lambda (fn env arg-types rtype fno)
    (fresh (name stmts stmtso)
      (== fn `(fn ,name () . ,stmts))
      (infer-stmts stmts env rtype stmtso)
      (== arg-types '())
      (== fno `(fn ,name () (,arg-types -> ,rtype) . ,stmtso))
      (lookup env name `(,arg-types -> ,rtype))
      (conda
        ((== 'main name) (== 'int rtype))
        ((== #f #f))))))

(define infer-decl*
  (lambda (fn* fn^* env)
    (conde
      ((== '() fn*) (== '() fn^*))
      ((fresh (name stmts rest arg-types rtype fno fno*)
         (== fn* `((fn ,name () . ,stmts) . ,rest))
         (infer-fn `(fn ,name () . ,stmts) env arg-types rtype fno)
         (infer-decl* rest fno* `((,name . ,arg-types) . ,env))
         (== fn^* `(,fno . ,fno*))))
      ((fresh (name type stmts stmtso)
         (== fn* `((extern ,name . ,type) . ,stmts))
         (lookup env name type)
         (infer-decl* stmts stmtso env)
         (== fn^* `((extern ,name . ,type) . ,stmtso)))))))

(define infer-module
  (lambda (mod typed-mod)
    (fresh (decl* decl*^ env arg-types rtype)
      (== mod `(module . ,decl*))
      ;; same-length is here to make sure the environment doesn't
      ;; have any logic variables. Without this, the type inferencer
      ;; will infer an environment like ((foo . int) . _0), and then
      ;; lookup will happily add whatever it needs to the environment
      ;; later on.
      (same-length decl* env)
      (infer-decl* decl* decl*^ env)
      (== typed-mod
        `(module . ,decl*^)))))

(define same-length
  (lambda (a b)
    (conde
      ((== a '())
       (== b '()))
      ((fresh (aa ad ba bd)
         (== a `(,aa . ,ad))
         (== b `(,ba . ,bd))
         (same-length ad bd))))))

(define typecheck
  (lambda (mod)
    (let ((result (run 2 (q) 
                    (infer-module mod q))))
      (case (length result)
        ((0) (error 'typecheck "Could not infer type for program." mod))
        ((1) (car result))
        (else
         (display result)
         (error 'typecheck 
                "Could not infer a unique type for program"
                result))))))
)
