(library
  (harlan middle returnify-kernels)
  (export returnify-kernels)
  (import
   (rnrs)
   (except (elegant-weapons helpers) ident?)
   (harlan helpers)
   (cKanren mk))
  
(define-match returnify-kernels
  ((module ,[returnify-kernel-decl -> fn*] ...)
   `(module . ,fn*)))

(define-match returnify-kernel-decl
  ((fn ,name ,args ,type ,[returnify-kernel-stmt -> stmt])
   `(fn ,name ,args ,type ,stmt))
  ((extern ,name ,args -> ,type)
   `(extern ,name ,args -> ,type)))

(define-match returnify-kernel-stmt
  ((print ,[returnify-kernel-expr -> expr*] ...)
   `(print . ,expr*))
  ((assert ,[returnify-kernel-expr -> expr])
   `(assert ,expr))
  ((set! ,[returnify-kernel-expr -> x]
         ,[returnify-kernel-expr -> e])
   `(set! ,x ,e))
  ((error ,x) `(error ,x))
  ((begin ,[stmt*] ...)
   `(begin . ,stmt*))
  ((if ,[returnify-kernel-expr -> test] ,[conseq])
   `(if ,test ,conseq))
  ((if ,[returnify-kernel-expr -> test] ,[conseq] ,[alt])
   `(if ,test ,conseq ,alt))
  ((return) `(return))
  ((return ,[returnify-kernel-expr -> expr])
   `(return ,expr))
  ((while ,[returnify-kernel-expr -> expr] ,[body])
   `(while ,expr ,body))
  ((for (,i ,[returnify-kernel-expr -> start]
            ,[returnify-kernel-expr -> stop]
            ,[returnify-kernel-expr -> step])
        ,[body])
   `(for (,i ,start ,stop ,step) ,body))
  ((let ((,x ,t ,[returnify-kernel-expr -> e]) ...) ,[stmt])
   `(let ((,x ,t ,e) ...) ,stmt))
  ((let-region (,r) ,[body]) `(let-region (,r) ,body))
  ((do ,[returnify-kernel-expr -> expr]) `(do ,expr)))

(define-match returnify-kernel-expr
  ((begin ,[returnify-kernel-stmt -> stmt*] ... ,[expr])
   `(begin ,@stmt* ,expr))
  ((let ((,x ,t ,[e]) ...) ,[expr])
   `(let ((,x ,t ,e) ...) ,expr))
  ((,t ,x) (guard (scalar-type? t)) `(,t ,x))
  ((var ,t ,x) `(var ,t ,x))
  ((if ,[t] ,[c] ,[a])
   `(if ,t ,c ,a))
  ((vector-ref ,t ,[v] ,[i])
   `(vector-ref ,t ,v ,i))
  ((length ,[e]) `(length ,e))
  ((int->float ,[e]) `(int->float ,e))
  ((make-vector ,t ,r ,[e]) `(make-vector ,t ,r ,e))
  ((vector ,t ,r ,[e*] ...) `(vector ,t ,r . ,e*))
  ((,op ,[lhs] ,[rhs])
   (guard (or (binop? op) (relop? op)))
   `(,op ,lhs ,rhs))
  ((c-expr ,t ,x) `(c-expr ,t ,x))
  ((call ,[fn] ,[arg*] ...) `(call ,fn . ,arg*))
  ((kernel . ,body*)
   (returnify-kernel `(kernel . ,body*))))

(define-match returnify-kernel
  ((kernel (vec ,t)
     ,r
     ,dims
     (((,x* ,tx*) (,[returnify-kernel-expr -> xe*] ,xet*) ,dim) ...)
     ,body)
   (let ((retvars (map (lambda (_) (gensym 'retval)) dims))
         ;; FIXME: we actually need N-dimensional danger vectors for
         ;; N-dimensional kernels.
         (danger-vector (gensym 'danger_vector))
         (danger (gensym 'danger))
         (i (gensym 'i))
         (vv (gensym 'vv))
         (id (gensym 'kern)))
     `(let ((,id (vec ,t) (make-vector ,t ,r ,(car dims)))
            (,danger-vector
             (vec bool)
             (make-vector bool ,(var 'danger-region) ,(car dims))))
        (begin
          ,@(if (null? (cdr dims))
                `()
                `((for (,i (int 0) ,(car dims) (int 1))
                       (let ((,vv ,t (make-vector ,(cadr t) ,(var 'region)
                                                  ,(cadr dims))))
                         (set! (vector-ref ,t (var (vec ,t) ,id) (var int ,i))
                               (var ,t ,vv))))))
          (kernel
           (vec ,t)
           ,dims
           ,(insert-retvars retvars (cons id retvars) 0 t
                            ;; Insert the danger vector as an argument
                            `(((,danger bool)
                               ((var (vec bool) ,danger-vector) (vec bool)) 0)
                              ((,x* ,tx*) (,xe* ,xet*) ,dim) ...))
           ,((set-retval (shave-type (length dims) `(vec ,t))
                         (car (reverse retvars))
                         danger)
             body))
          ,(check-danger-vector danger-vector (car dims))
          (var (vec ,t) ,id))))))

(define (check-danger-vector danger-vector len)
  (let ((i (gensym 'danger_i))
        (found-danger (gensym 'no_found_danger)))
    `(let ((,found-danger bool (bool #t)))
       (begin
         (for (,i (int 0) ,len (int 1))
              (if (vector-ref bool (var (vec bool) ,danger-vector) (var int ,i))
                  (begin
                    (do (call (c-expr ((void str int) -> void) fprintf)
                              (c-expr void stderr)
                              (str "Kernel lane %d encountered danger!\n")
                              (var int ,i)))
                    (set! (var bool ,found-danger) (bool #f)))))
         (assert (var bool ,found-danger))))))
                
             

;; This is stupid
(define (shave-type dim t)
  (if (zero? dim) t (shave-type (- dim 1) (cadr t))))

;; This is the stupidest procedure I've ever written
(define (insert-retvars retvars sources dim t arg*)
  (match arg*
    (() (guard (null? retvars)) `())
    (()
     (cons
      `((,(car retvars) ,t)
        ((var (vec ,t) ,(car sources))
         (vec ,t))
        ,dim)
      (if (null? (cdr retvars))
          `()
          (insert-retvars (cdr retvars)
                          (cdr sources)
                          (+ dim 1)
                          (cadr t)
                          arg*))))
    ((((,x ,tx) (,xs ,ts) ,d) . ,rest)
     (if (<= dim d)
         (cons
          `((,(car retvars) ,t)
            ((var (vec ,t) ,(car sources))
             (vec ,t))
            ,dim)
          (if (null? (cdr retvars))
              arg*
              (insert-retvars (cdr retvars)
                              (cdr sources)
                              (+ dim 1)
                              (cadr t)
                              arg*)))
         (cons (car arg*)
               (insert-retvars retvars
                               sources
                               dim
                               t
                               (cdr arg*)))))))

;; Replaces calls to (error) with set!'s on the error variable and a
;; return.
(define-match (rewrite-errors-stmt danger)
  ((error ,e)
   `(begin
      (set! (var bool ,danger) (bool #t))
      (return)))
  ((for (,x ,[(rewrite-errors-expr danger) -> start]
            ,[(rewrite-errors-expr danger) -> stop]
            ,[(rewrite-errors-expr danger) -> step])
        ,[body])
   `(for (,x ,start ,stop ,step) ,body))
  ((while ,[(rewrite-errors-expr danger) -> test] ,[body])
   `(while ,test ,body))
  ((set! ,[(rewrite-errors-expr danger) -> lhs]
         ,[(rewrite-errors-expr danger) -> rhs])
   `(set! ,lhs ,rhs))
  ((let ((,x ,t ,[(rewrite-errors-expr danger) -> e]) ...) ,[body])
   `(let ((,x ,t ,e) ...) ,body))
  ((begin ,[s] ...) `(begin ,s ...))
  ((return) `(return))
  ((if ,[(rewrite-errors-expr danger) -> t] ,[c] ,[a])
   `(if ,t ,c ,a))
  ((if ,[(rewrite-errors-expr danger) -> t] ,[c])
   `(if ,t ,c)))

(define-match (rewrite-errors-expr danger)
  ((int ,i) `(int ,i))
  ((float ,f) `(float ,f))
  ((bool ,b) `(bool ,b))
  ((var ,t ,x) `(var ,t ,x))
  ((int->float ,[e]) `(int->float ,e))
  ((vector-ref ,t ,[v] ,[i])
   `(vector-ref ,t ,v ,i))
  ((error ,e)
   `(begin
      (set! (var bool ,danger) (bool #t))
      (return)))
  ((let ((,x ,t ,[e]) ...) ,[body])
   `(let ((,x ,t ,e) ...) ,body))
  ((begin ,[(rewrite-errors-stmt danger) -> stmt*] ... ,[e])
   `(begin ,stmt* ... ,e))
  ((if ,[t] ,[c] ,[a]) `(if ,t ,c ,a))
  ((length ,[e]) `(length ,e))
  ((make-vector ,t ,r ,[e]) `(make-vector ,t ,r ,e))
  ((vector ,t ,r ,[e] ...) `(vector ,t ,r ,e ...))
  ((call ,[e] ...) `(call ,e ...))
  ((c-expr . ,c) `(c-expr . ,c))
  ((,op ,[lhs] ,[rhs]) (guard (or (relop? op) (binop? op)))
   `(,op ,lhs ,rhs)))

;; Changes expressions in tail position to set!'s on the return value.
(define-match (set-retval t retvar danger)
  ((begin ,[(rewrite-errors-stmt danger) -> stmt*] ...
          ,[expr])
   `(begin ,@stmt* ,((rewrite-errors-stmt danger) expr)))
  ((let ((,x ,t ,[(rewrite-errors-expr danger) -> e]) ...)
     ,[expr])
   `(let ((,x ,t ,e) ...) ,((rewrite-errors-stmt danger) expr)))
  (,[(rewrite-errors-expr danger) -> else]
   `(begin
      (set! (var bool ,danger) (bool #f))
      (set! (var ,t ,retvar) ,else))))

;; end library
)

