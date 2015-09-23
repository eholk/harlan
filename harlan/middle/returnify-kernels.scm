(library
  (harlan middle returnify-kernels)
  (export returnify-kernels
          danger-type
          bounds-check
          allocation-failure
          num-danger)
  (import
   (rnrs)
   (except (elegant-weapons helpers) ident?)
   (harlan helpers))

  ;; Variables related to danger
  ;;
  ;; Be sure to update DANGER_TABLE in harlan.cpp
  (define danger-type 'bool)
  (define bounds-check '(int 0))
  (define allocation-failure '(int 1))
  (define num-danger '(int 2))
  
(define-match returnify-kernels
  ((module ,[returnify-kernel-decl -> fn*] ...)
   `(module . ,fn*)))

(define-match returnify-kernel-decl
  ((fn ,name ,args ,type ,[returnify-kernel-stmt -> stmt])
   `(fn ,name ,args ,type ,stmt))
  ((typedef ,name ,t) `(typedef ,name ,t))
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
  ((let-region (,r ...) ,[body]) `(let-region (,r ...) ,body))
  ((do ,[returnify-kernel-expr -> expr]) `(do ,expr)))

(define-match returnify-kernel-expr
  ((box ,r ,t ,[e]) `(box ,r ,t ,e))
  ((unbox ,t ,r ,[e]) `(unbox ,t ,r ,e))
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
  ((unsafe-vec-ptr ,t ,[v])
   `(unsafe-vec-ptr ,t ,v))
  ((length ,[e]) `(length ,e))
  ((cast ,t ,[e]) `(cast ,t ,e))
  ((make-vector ,t ,r ,[e]) `(make-vector ,t ,r ,e))
  ((vector ,t ,r ,[e*] ...) `(vector ,t ,r . ,e*))
  ((,op ,[lhs] ,[rhs])
   (guard (or (binop? op) (relop? op)))
   `(,op ,lhs ,rhs))
  ((c-expr ,t ,x) `(c-expr ,t ,x))
  ((call ,[fn] ,[arg*] ...) `(call ,fn . ,arg*))
  ((field ,[e] ,x) `(field ,e ,x))
  ((empty-struct) '(empty-struct))
  ((kernel . ,body*)
   (returnify-kernel `(kernel . ,body*))))

(define-match returnify-kernel
  ((kernel (vec ,r ,t)
     ,r
     ,dims
     (((,x* ,tx*) (,[returnify-kernel-expr -> xe*] ,xet*) ,dim) ...)
     ,body)
   (let ((retvars (map (lambda (_) (gensym 'retval)) dims))
         (danger-vector (gensym 'danger_vector))
         (danger-vec-t `(vec ,r ,danger-type))
         (i (gensym 'i))
         (vv (gensym 'vv))
         (id (gensym 'kern)))
     `(let ((,id (vec ,r ,t) (make-vector ,t ,r ,(car dims)))
            (,danger-vector
             ;; TODO: the danger vector should probably get its own region.
             ,danger-vec-t
             (make-vector ,danger-type ,r ,num-danger)))
        (begin
          (for (,i (int 0) ,num-danger (int 1))
            (set! (vector-ref
                   ,danger-type
                   (var ,danger-vec-t ,danger-vector)
                   (var int ,i))
                  (bool #f)))
          ,@(if (null? (cdr dims))
                `()
                (match t
                  ((vec ,r^ ,t^)
                   `((for (,i (int 0) ,(car dims) (int 1))
                          (let ((,vv (vec ,r^ ,t^)
                                     (make-vector ,t^ ,r^
                                                  ,(cadr dims))))
                            (set! (vector-ref (vec ,r^ ,t^)
                                              (var (vec ,r ,t) ,id)
                                              (var int ,i))
                                  (var (vec ,r^ ,t^) ,vv))))))))
          (kernel
           (vec ,r ,t)
           ,dims
           (danger: ,danger-vector ,danger-vec-t)
           ,(insert-retvars r retvars (cons id retvars) 0 t
                            `(((,x* ,tx*) (,xe* ,xet*) ,dim) ...))
           ,((set-retval (shave-type (length dims) `(vec ,r ,t))
                         (car (reverse retvars))
                         (lambda (d) `(vector-ref ,danger-type
                                             (var ,danger-vec-t ,danger-vector)
                                             ,d)))
              body))
          ,(check-danger-vector danger-vector r num-danger)
          (var (vec ,r ,t) ,id))))))

(define (check-danger-vector danger-vector r len)
  (let ((i (gensym 'danger_i))
        (found-danger (gensym 'no_found_danger))
        ;; FIXME: this is using a dummy region variable...
        (danger-vec-t `(vec ,r ,danger-type))
        (di (gensym 'di)))
    `(let ((,found-danger bool (bool #t)))
       (begin
         (for (,i (int 0) ,len (int 1))
           (if (vector-ref ,danger-type
                           (var ,danger-vec-t ,danger-vector)
                           (var int ,i))
               (begin
                 (do (call (c-expr (fn (void str int) -> void) fprintf)
                           (c-expr void stderr)
                           (str "Kernel encounter danger type %d (%s)!\n")
                           (var int ,i)
                           (call (c-expr (fn (int) -> str) danger_name)
                                 (var int ,i))))
                 (set! (var bool ,found-danger) (bool #f)))))
         (assert (var bool ,found-danger))))))

;; This is stupid
(define (shave-type dim t)
  (if (zero? dim) t (shave-type (- dim 1) (caddr t))))

;; This is the stupidest procedure I've ever written
(define (insert-retvars r retvars sources dim t arg*)
  (match arg*
    (() (guard (null? retvars)) `())
    (()
     (cons
      `((,(car retvars) ,t)
        ((var (vec ,r ,t) ,(car sources))
         (vec ,r ,t))
        ,dim)
      (if (null? (cdr retvars))
          `()
          (match t
            ((vec ,r ,t)
             (insert-retvars r
                             (cdr retvars)
                             (cdr sources)
                             (+ dim 1)
                             t
                             arg*))))))
    ((((,x ,tx) (,xs ,ts) ,d) . ,rest)
     (if (<= dim d)
         (cons
          `((,(car retvars) ,t)
            ((var (vec ,r ,t) ,(car sources))
             (vec ,r ,t))
            ,dim)
          (if (null? (cdr retvars))
              arg*
              (match t
                ((vec ,r ,t)
                 (insert-retvars r
                                 (cdr retvars)
                                 (cdr sources)
                                 (+ dim 1)
                                 t
                                 arg*)))))
         (cons (car arg*)
               (insert-retvars r
                               retvars
                               sources
                               dim
                               t
                               (cdr arg*)))))))

;; Replaces calls to (error) with set!'s on the error variable and a
;; return.
(define-match (rewrite-errors-stmt danger)
  ((error ,e)
   `(begin
      (set! ,(danger bounds-check) (bool #t))
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
   `(if ,t ,c))
  ((do ,[(rewrite-errors-expr danger) -> e])
   `(do , e)))

(define-match (rewrite-errors-expr^ danger)
  ((int ,i) `(int ,i))
  ((float ,f) `(float ,f))
  ((bool ,b) `(bool ,b))
  ((var ,t ,x) `(var ,t ,x))
  ((char ,c) `(char ,c))
  ((str ,s) `(str ,s))
  ((cast ,t ,[e]) `(cast ,t ,e))
  ((vector-ref ,t ,[v] ,[i])
   `(vector-ref ,t ,v ,i))
  ((unsafe-vec-ptr ,t ,[v])
   `(unsafe-vec-ptr ,t ,v))
  ((error ,e)
   `(begin
      (set! ,(danger bounds-check) (bool #t))
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
  ((field ,[e] ,x) `(field ,e ,x))
  ((unbox ,t ,r ,[e]) `(unbox ,t ,r ,e))
  ((box ,r ,t ,[e]) `(box ,r ,t ,e))
  ((,op ,[lhs] ,[rhs]) (guard (or (relop? op) (binop? op)))
   `(,op ,lhs ,rhs)))

;;(define (rewrite-errors-expr danger)
;;  (trace-lambda rewrite-errors-expr (e)
;;                ((rewrite-errors-expr^ danger) e)))
(define rewrite-errors-expr rewrite-errors-expr^)

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
      (set! (var ,t ,retvar) ,else))))

;; end library
)

