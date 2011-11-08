(library
  (harlan middle lower-vectors)
  (export
    lower-vectors)
  (import
    (only (chezscheme) format)
    (rnrs)
    (elegant-weapons match)
    (elegant-weapons print-c)
    (util verify-grammar)
    (elegant-weapons helpers))

;; Moves to a lower-level allocate and set! representation for
;; vectors. This runs after typechecking.

(define-match lower-vectors
  ((module ,[lower-decl -> fn*] ...)
   `(module . ,fn*)))

(define-match lower-decl
  ((fn ,name ,args ,t ,[lower-stmt -> stmt*])
   `(fn ,name ,args ,t ,(make-begin stmt*)))
  (,else else))

(define-match lower-stmt

  ((let ,x ,t (vector . ,e*))
   `((let ,x ,t (int ,(length e*)))
     . ,(let loop ((e* e*)
                   (i 0))
          (if (null? e*)
              '()
              `((vector-set!
                  ,(cadr t) (var ,t ,x) (int ,i) ,(car e*))
                . ,(loop (cdr e*) (+ 1 i)))))))
  
  ((let ,x ,t (make-vector ,vt ,e))
   `((let ,x ,t ,e)))
  
  ((let ,x ,t (iota ,e))
   (let ((i (gensym 'i)))
     `((let ,x ,t ,e)
       (for (,i (int 0) ,e)
         (vector-set! int (var ,t ,x) (var int ,i) (var int ,i))))))
  
  ((let ,x ,t (reduce ,t2 + (var ,tv ,v)))
   (let ((i (gensym 'i)))
     `((let ,x ,t (vector-ref ,t (var ,tv ,v) (int 0)))
       (for (,i (int 1) (length (var ,tv ,v)))
         (set! (var ,t ,x)
           (+ (var ,t ,x)
             (vector-ref ,t (var ,tv ,v) (var int ,i))))))))

  ((let ,x ,t ,e)
   `((let ,x ,t ,e)))
  
  ((begin ,[stmt*] ...)
   `(,(make-begin (apply append stmt*))))
  ((kernel ,t ,b ,[stmt])
   `((kernel ,t ,b (begin ,@stmt))))
  ((print ,expr)
   `((print ,expr)))      
  ((assert ,expr)
   `((assert ,expr)))
  ((set! ,x ,i) `((set! ,x ,i)))
  ((if ,test ,[conseq])
   `((if ,test ,(make-begin conseq))))
  ((if ,test ,[conseq] ,[alt])
   `((if ,test ,(make-begin conseq) ,(make-begin alt))))
  ((vector-set! ,t ,e1 ,i ,e2)
   `((vector-set! ,t ,e1 ,i ,e2)))
  ((while ,expr ,[body])
   `((while ,expr ,(make-begin body))))
  ((for (,x ,start ,end) ,[body])
   `((for (,x ,start ,end) ,(make-begin body))))
  ((return ,expr)
   `((return ,expr)))
  ((do . ,expr*) `((do . ,expr*))))

(define-match lower-expr
  ((begin ,[lower-stmt -> stmt*] ... ,[expr])
   `(begin ,@(apply append stmt*) ,expr))
  (,else else))
)
