(library
  (harlan middle insert-let-regions)
  (export insert-let-regions)
  (import (rnrs)
    (harlan helpers)
    (elegant-weapons helpers))
  
  ;; insert-let-regions inserts let-regions (no kidding) in an
  ;; ascending order from top level inwards.  otherwise, the region
  ;; inferencer will have no clue which regions take precedence, have
  ;; what scope, etc.  there's a better way to do this, but lazy.

  (define (new-region) (gensym 'g_region))
  
  ;; put recursive calls inside calls to letr for great success
  (define-syntax letr
    (syntax-rules ()
      ((_ s)
       (let ((r (new-region)))
         `(let-region (,r) ,s)))))

  (define-match type->regions
    ((vec ,r*)
     (let ((r (new-region)))
       (cons r (type->regions r*))))
    (,t `()))

  (define-match insert-let-regions
    ((module ,[insert-decl -> decl*] ...)
     `(module . ,decl*)))

  (define-match insert-decl
    ((fn ,name ,args (,argt -> ,rt) ,s)
     (let ((region (gensym (symbol-append name 'region))))
       (let ((in-r (map type->regions argt))
             (out-r (type->regions rt))
             (r (new-region)))
         `(fn ,name ,args (,argt -> ,rt)
              (input-regions ,in-r)
              (output-regions ,out-r)
              (let-region (,region)
                          ,(insert-stmt s))))))
    ((extern ,name ,args -> ,t)
     `(extern ,name ,args -> ,t)))

  (define-match insert-stmt
    ((error ,x) `(error ,x))
    ((print ,t ...) `(print . ,t))
    ((assert ,t) `(assert ,t))
    ((set! ,x ,v) `(set! ,x ,v))
    ((kernel ,args ... ,s)
     `(kernel ,args ... ,s))
    ((let ,b ,[s]) `(let ,b ,s))
    ((if ,t ,[c]) `(if ,t ,c))
    ((if ,t ,[c] ,[a]) `(if ,t ,c ,a))
    ((for ,b ,s)
     `(for ,b ,(letr (insert-stmt s))))
    ((while ,t ,s)
     `(while ,t ,(letr (insert-stmt s))))
    ((do ,t) `(do ,t))
    ((begin ,[s*] ...) `(begin . ,s*))
    ((return) `(return))
    ((return ,t) `(return ,t)))

)
