(library
  (harlan middle returnify-kernels)
  (export returnify-kernels)
  (import (rnrs)
    (util match)
    (util verify-grammar)
    (util helpers))
  
  (define-match returnify-kernels
    ((module ,[returnify-kernel-decl -> fn*] ...)
     `(module . ,fn*)))

  (define-match returnify-kernel-decl
    ((fn ,name ,args ,type ,stmt* ...)
     `(fn ,name ,args ,type . ,(returnify-kernel-stmt* stmt*)))
    ((extern ,name ,args -> ,type)
     `(extern ,name ,args -> ,type)))

  (define-match returnify-kernel-stmt*
    (() '())
    ((,[returnify-kernel-stmt -> stmt] . ,[rest])
     (append stmt rest)))

  (define-match returnify-kernel-stmt
    ((print ,expr) `((print ,expr)))
    ((assert ,expr) `((assert ,expr)))
    ((set! ,x ,e) `((set! ,x ,e)))
    ((vector-set! ,t ,x ,e1 ,e2) `((vector-set! ,t ,x ,e1 ,e2)))
    ((return ,expr) `((return ,expr)))
    ((while ,expr . ,[returnify-kernel-stmt* -> body*])
     `((while ,expr . ,body*)))
    ((for (,x ,e1 ,e2) . ,[returnify-kernel-stmt* -> body*])
     `((for (,x ,e1 ,e2) . ,body*)))
    ((let ,id ,type ,expr)
     (returnify-kernel-let `(let ,id ,type ,expr)))
    ((do ,expr) `((do ,expr))))

  (define-match type-dim
    ((vector ,[t] ,n) (+ 1 t))
    (,x 0))
  
  (define-match returnify-kernel-let
    ((let ,id ,t1 (kernel void ,arg* . ,body*))
     ;; TODO: we still need to traverse the body*
     `(let ,id ,t1 (kernel void ,arg* . ,body*)))
    ((let ,id ,t1 (kernel ,t2 ,arg* . ,body*))
     (if (= 1 (type-dim t1))
         (match arg*
           ((((,x* ,tx*) (,xe* ,xet*)) ...)
            (let ((g (gensym 'retval)))
              (let ((body*
                      (let loop ((body* body*)) ;; why is this a loop
                        (match body*
                          ((,[returnify-kernel-stmt -> body^] ... ,body)
                           (append (apply append body^)
                             `((set! (var ,(cadr t2) ,g) ,body))))
                          (,else (error 'returnify-kernel-let
                                   "malformed stuffs" else))))))
                `((let ,id ,t1 (length ,(car xe*)))
                  (kernel ,t2 (((,g ,(cadr t2)) ((var ,t1 ,id) ,t1))
                               . ,arg*)
                    . ,body*)))))
           (,else (error 'returnify-kernel-let
                    "could not match kernel arguments" arg*)))
         (error 'returnify-kernel-let
           "Only 1-dimensional return values are allowed.")))
    ((let ,id ,type ,expr) `((let ,id ,type ,expr))))

  )
