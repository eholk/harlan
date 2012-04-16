(library
  (harlan middle infer-regions)
  (export infer-regions
    infer-module
    infer-decls
    infer-fn-args
    infer-stmts
    infer-stmt
    infer-let-bindings
    memory-type
    scalar-type
    region
    assign-to-region)
  (import
    (rnrs)
    (cKanren ck)
    (cKanren fd)
    (cKanren tree-unify)
    (cKanren neq)
    (cKanren tracing)
    (harlan compile-opts)
    (util color))

(define number-of-regions 1)

(define pairo
  (lambda (x)
    (fresh (a d)
      (== `(,a . ,d) x))))

(define lookup
  (lambda (env x type)
    (fresh (env^)
      (conde
        ((== `((,x . ,type) . ,env^) env))
        ((fresh (y t)
           (=/= x y)
           (== `((,y . ,t) . ,env^) env)
           (lookup env^ x type)))))))

(define (infer-regions mod)
  (let ((result (run 2 (q) (infer-module mod q))))
    (case (length result)
      ((0)
       (error 'infer-regions
         "could not infer regions for program" mod))
      ((1) (car result))
      ((2)
       (error 'infer-regions
         "could not infer unique region assignment for program" mod)))))

(define (infer-module mod modo)
  (fresh (decls declso rso)
    (== mod `(module . ,decls))
    (infer-decls decls `() declso rso)
    (== modo `(module . ,decls))))

(define (infer-decls decls rs declso rso)
  (conde
    ((== decls `()) (== declso `()) (== rs rso))
    ((fresh (decl declo rest resto name t rs^^)
       (== decls `(,decl . ,rest))
       (== declso `(,declo . ,resto))
       (conde
         ((fresh (stmt stmto args arg-t rt rs^ regs)
            (== decl `(fn ,name ,args ,t ,stmt))
            (== declo `(fn ,name ,args ,regs ,t ,stmt))
            (== t `(,arg-t -> ,rt))
            (infer-fn-args args arg-t rs regs rs^)
            (infer-stmt stmt rs^ rs^^)))
         ((== decl `(extern ,name . ,t))
          (== declo decl)
          (== rs rs^^)))
       (infer-decls rest rs^^ resto rso)))))

(define (infer-fn-args args arg-t rs regs rso)
  (conde
    ((== args `()) (== arg-t `())
     (== rs rso)
     (== regs `()))
    ((fresh (a at res rest rs^ regs^)
       (== args `(,a . ,res))
       (== arg-t `(,at . ,rest))
       (conde
         ((memory-type at)
          (fresh (r)
            (region r)
            (== regs `(,a . ,regs^))
            (assign-to-region a r rs rs^)))
         ((scalar-type at)
          (== regs regs^)
          (== rs rs^)))
       (infer-fn-args res rest rs^ regs^ rso)))))

(define (infer-stmts stmts rs rso)
  (conde
    ((== stmts `()) (== rs rso))
    ((fresh (s res rs^)
       (== stmts `(,s . ,res))
       (infer-stmt s rs rs^)
       (infer-stmts res rs^ rso)))))

(define (infer-stmt stmt rs rso)
  (conde
    ((fresh (triv)
       (== stmt `(print ,triv))
       (== rs rso)))
    ((fresh (triv op)
       (== stmt `(print ,triv ,op))
       (== rs rso)))
    ((fresh (triv)
       (== stmt `(assert ,triv))
       (== rs rso)))
    ((fresh (x triv)
       (== stmt `(set! ,x ,triv))
       (== rs rso)))
    ((fresh (t x i v)
       (== stmt `(vector-set! ,t ,x ,i ,v))
       (== rs rso)))
    ((fresh (d b fv s)
       (== stmt `(kernel ,d ,b ,fv ,s))
       ;; This is missing some stuff.
       (infer-stmt s rs rso)))
    ((fresh (b s rs^)
       (== stmt `(let ,b ,s))
       (infer-let-bindings b rs rs^)
       (infer-stmt s rs^ rso)))
    ((fresh (stmt* stmt*^)
       (== stmt `(begin . ,stmt*))
       (infer-stmts stmt* rs rso)))
    ((fresh (t c)
       (== stmt `(if ,t ,c))
       (infer-stmt c rs rso)))
    ((fresh (t c a rs^)
       (== stmt `(if ,t ,c ,a))
       (infer-stmt c rs rs^)
       (infer-stmt a rs^ rso)))
    ((fresh (triv)
       (conde
         ((== stmt `(return ,triv)))
         ((== stmt `(return))))
       (== rs rso)))
    ((fresh (triv)
       (== stmt `(do ,triv))
       (== rs rso)))
    ((fresh (x start end s)
       (== stmt `(for (,x ,start ,end) ,s))
       (infer-stmt s rs rso)))
    ((fresh (e s)
       (== stmt `(while ,e ,s))
       (infer-stmt s rs rso)))))

(define (infer-let-bindings b rs rso)
  (conde
    ((== b `()) (== rs rso))
    ((fresh (x t e rest rs^)
       (== b `((,x ,t ,e) . ,rest))
       (conde
         ((memory-type t)
          (fresh (r)
            (region r)
            (assign-to-region x r rs rs^)))
         ((scalar-type t)
          (== rs rs^)))
       (infer-let-bindings rest rs^ rso)))))

(define (memory-type t)
  (conde
    ((fresh (type)
       (== t `(vec ,type))))))

(define (scalar-type t)
  (conde
    ((== t `int))
    ((== t `u64))
    ((== t `float))
    ((== t `char))
    ((== t `str))))

(define (region r)
  (infd r (range 1 number-of-regions)))

(define (assign-to-region x r rs rso)
  (conde
    ((== rs `())
     (== rso `((,r . (,x)))))
    ((fresh (ls rest)
       (== rs `((,r . ,ls) . ,rest))
       (conde
         ((membero x ls)
          (== rs rso))
         ((not-membero x ls)
          (== rso `((,r . (,x . ,ls)) . ,rest))))))
    ((fresh (s ls rest rs^)
       (== rs `((,s . ,ls) . ,rest))
       (=/= s r)
       (== rso `((,s . ,ls) . ,rs^))
       (assign-to-region x r rest rs^)))))

(define (not-membero x ls)
  (conde
    ((== ls `()))
    ((fresh (a d)
       (== ls `(,a . ,d))
       (=/= a x)
       (not-membero x d)))))

(define (membero x ls)
  (conde
    ((fresh (d)
       (== ls `(,x . ,d))))
    ((fresh (a d)
       (== ls `(,a . ,d))
       (=/= a x)
       (membero x d)))))


#|

(define infer-kernel-bindings
  (lambda (b* b^* env envo n)
    (conde
      ((== '() b*) (== '() b^*) (== env envo))
      ((fresh (x e e^ tx te rest rest^ env^)
         (== `((,x ,e) . ,rest) b*)
         (report-backtrack e env)
         (== `(((,x ,tx) (,e^ ,te)) . ,rest^) b^*)
         (== `(vec ,tx) te)
         (== envo `((,x . ,tx) . ,env^))
         (infer-expr e env te e^)
         (infer-kernel-bindings rest rest^ env env^ n))))))

|#
)

