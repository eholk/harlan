(library
 (annotate-free-vars)
 (export annotate-free-vars)
 (import (only (chezscheme) format printf)
         (rnrs)
         (print-c)
         (match))

;; Is there anything else I should work on tomorrow, once I get this pass working?
;; either the for loop pass, or the pass that uses the free variables, or keep working on returnify kernels
;; my short term goal is to get dot-product.kfc all the way through the compiler, and generating C++ that compiles and runs
;; it currently gets through the compiler (at least last I checked), but it doesn't generate valid C++
;; okay
;; the pass that uses the free variables
;; seems like a good next step
;; since that's what I've got on my mind
;; alright
;; do you know what to do there?
;; not exactly
;; ok
;; so, in generate-kernel, in kernels.scm
;; it takes a list of all the iterator parameters and generates a new top level kernel
;; basically, for all the vectors, these get passed into the kernel as a void *
;; for each of these, it generates a call to vec_deserialize, which converts the void * into a vec struct
;; this represents the big X
;; it also looks up the kernel's global id
;; and then let-binds small x to be (vec_ref_1d X id)
;; this is a pointer to some T, so it goes through and replaces all the uses of x with a deref
;; for the free variables, we need to do the exact same thing, except that you don't implicitly unpack a small x
;; so at the end there won't be a distinction between free variables and iterator variables, there will just be variables
;; and some of them are implicitly vector-ref'd, and others aren't
 
 (define gensym
   (let ((c 0))
     (lambda (x)
       (set! c (+ 1 c))
       (string->symbol
        (string-append (symbol->string x) "_" (number->string c))))))

 (define annotate-free-vars
   (lambda (mod)
     (match mod
       ((,[(annotate-decl '()) -> decl*] ...)
        decl*))))

 (define annotate-decl
   (lambda (gamma)
     (lambda (decl)
       (match decl
         ((func ,type ,name ,args ,stmt* ...)
          (let-values (((stmt* gamma) ((annotate-stmt* gamma) stmt*)))
            `(func ,type ,name ,args . ,stmt*)))))))

(define annotate-stmt*
  (lambda (gamma)
    (lambda (stmt*)
      (let loop ([stmt* stmt*][gamma gamma][new-stmt* '()])
        (cond
          [(null? stmt*)
           (values (reverse new-stmt*) gamma)]
          [else
           (let-values (((stmt gamma) ((annotate-stmt gamma) (car stmt*))))
             (loop (cdr stmt*) gamma (cons stmt new-stmt*)))])))))

 (define annotate-stmt
   (lambda (gamma)
     (lambda (stmt)
       (match stmt
         ((kernel (((,x* ,t*) (,xs* ,ts*)) ...) ,stmt* ...)
          (let ((gamma (append (map cons x* t*) gamma)))
            (let-values (((stmt* gamma) ((annotate-stmt* gamma) stmt*)))
              (let* ((fv* (remove* x* (free-vars-stmt* stmt*)))
                     (fvt* (map (lambda (x) (lookup x gamma)) fv*)))
                (let ((kernel-args (map (lambda (x t xs ts) (list (list x t) (list xs ts))) x* t* xs* ts*)))
                  (let ((fv* (map list fv* fvt*)))
                    (values
                     `(kernel ,kernel-args (free-vars ,@fv*) . ,stmt*)
                     gamma)))))))
         ((return ,[(annotate-expr gamma) -> e])
          (values `(return ,e) gamma))
         ((do ,[(annotate-expr gamma) -> e*] ...)
          (values `(do ,e* ...) gamma))
         ((let ,x ,t ,e)
          (let* ((gamma `((,x . ,t) . ,gamma))
                 (e ((annotate-expr gamma) e)))
            (values `(let ,x ,t ,e) gamma)))
         ((for (,x ,[(annotate-expr gamma) -> start]
                   ,[(annotate-expr gamma) -> end])
               ,s ...)
          (let ((gamma `((,x . int) . ,gamma)))
            (let-values (((s gamma) ((annotate-stmt* gamma) s)))
              (values `(for (,x ,start ,end) ,s ...) gamma))))
         ((set! ,x ,[(annotate-expr gamma) -> e])
          (values `(set! ,x ,e) gamma))
         ((print ,[(annotate-expr gamma) -> e])
          (values `(print ,e) gamma))
         ((print ,[(annotate-expr gamma) -> e1] ,[(annotate-expr gamma) -> e2])
          (values `(print ,e1 ,e2) gamma))         
         ((vec_set_vec ,[(annotate-expr gamma) -> e1]
                       ,[(annotate-expr gamma) -> e2]
                       ,[(annotate-expr gamma) -> e3])
          (values `(vec_set_vec ,e1 ,e2 ,e3) gamma))
         (,else
          (error 'annotate-stmt
                 (format "annotate-free-vars--unknown statement type: ~s"
                         stmt)))))))

(define annotate-expr
  (lambda (gamma)
    (lambda (expr)
      (match expr
        (,n (guard (number? n)) n)
        (,x (guard (symbol? x)) x)
        ((,+ ,[(annotate-expr gamma) -> e1] ,[(annotate-expr gamma) -> e2])
         (guard (binop? +))
         `(,+ ,e1 ,e2))
        ((= ,[(annotate-expr gamma) -> e1] ,[(annotate-expr gamma) -> e2])
         `(= ,e1 ,e2))
        ((nanotime) '(nanotime))
	((assert ,[(annotate-expr gamma) -> e])
	 `(assert ,e))
        ((deref ,[(annotate-expr gamma) -> e]) `(deref ,e))
        ((hvec_byte_size ,[(annotate-expr gamma) -> e])
         `(hvec_byte_size ,e))
        ((hvec_length ,[e] ,[sz])
         `(hvec_length ,e ,sz))
        ((hvec_ref ,[v] ,[i] ,[sz])
         `(hvec_ref ,v ,i ,sz))
        ((vec_ref_1d ,[(annotate-expr gamma) -> ve] ,[(annotate-expr gamma) -> ie])
         `(vec_ref_1d ,ve ,ie))
        ((vec_ref_nd ,[(annotate-expr gamma) -> ve] ,[(annotate-expr gamma) -> ie])
         `(vec_ref_nd ,ve ,ie))
        ((cast ,t ,[(annotate-expr gamma) -> e]) `(cast ,t ,e))
        ((addressof ,[(annotate-expr gamma) -> e]) `(addressof ,e))
        ((sizeof ,t) `(sizeof ,t))
	((malloc ,[(annotate-expr gamma) -> e]) `(malloc ,e))
	((memcpy ,[dst] ,[src] ,[len])
	 `(memcpy ,dst ,src ,len))
        ((hmk_vec ,[(annotate-expr gamma) -> e1]
                  ,[(annotate-expr gamma) -> e2]
                  ,[(annotate-expr gamma) -> e3])
         `(hmk_vec ,e1 ,e2 ,e3))      
        (,else
         (error 'annotate-expr
                (format "annotate-free-vars--unknown expr type: ~s" expr)))))))

 (define free-vars-stmt*
   (lambda (stmt*)
     (let loop ([stmt* stmt*])
       (match stmt*
         [() '()]
         [((let ,x ,t ,[free-vars-expr -> e]) . ,rest)         
          (union e (remove x (loop rest)))]
         [(,stmt . ,rest)
          (union (free-vars-stmt stmt) (loop rest))]))))

 (define free-vars-stmt
   (lambda (stmt)
     (match stmt
       ((kernel (((,x* ,t*) (,xs* ,ts*)) ...) ,stmt* ...)
        (let ((stmt* (free-vars-stmt* stmt*)))
          (remove* x* stmt*)))
       ((return ,[free-vars-expr -> e]) e)
       ((do ,[free-vars-stmt -> s]) s)
       ((assert ,[free-vars-expr -> e]) e)       
       ((let ,x ,t ,[free-vars-expr -> e])
        (remove x e))
       ((for (,x ,[free-vars-expr -> start] ,[free-vars-expr -> end]) ,[free-vars-stmt -> s])
        (remove x s))
       ((set! ,x ,[free-vars-expr -> e]) (union `(,x) e))
       ((print ,[free-vars-expr -> e]) e)
       ((vec_set_vec ,[free-vars-expr -> e1] ,[free-vars-expr -> e2] ,[free-vars-expr -> e3])
        (union (union e1 e2) e3))
       (,else (error 'free-vars-stmt (format "annotate-free-vars--unknown statement type: ~s" stmt))))))

(define free-vars-expr
  (lambda (expr)
    (match expr
      (,n (guard (number? n)) '())
      (,x (guard (symbol? x)) `(,x))
      ((,+ ,[free-vars-expr -> e1] ,[free-vars-expr -> e2])
       (guard (binop? +))
       (union e1 e2))
      ((= ,[free-vars-expr -> e1] ,[free-vars-expr -> e2])
       (union e1 e2))
      ((deref ,[free-vars-expr -> e]) e)
      ((hvec_length ,[free-vars-expr -> e])
       `(hvec_length ,e))
      ((vec_ref_1d ,[free-vars-expr -> ve] ,[free-vars-expr -> ie])
       (union ve ie))
      ((vec_ref_nd ,[free-vars-expr -> ve] ,[free-vars-expr -> ie])
       (union ve ie))
      ((cast ,t ,[free-vars-expr -> e]) e)
      ((addressof ,[free-vars-expr -> e]) e)
      ((sizeof ,t) '())
      ((mk_vec ,[free-vars-expr -> e1] ,[free-vars-expr -> e2] ,[free-vars-expr -> e3])
       (union (union e1 e2) e3))
      (,else (error 'free-vars-expr (format "annotate-free-vars--unknown expr type: ~s" expr))))))

(define union
  (lambda (s1 s2)
    (cond
      [(null? s1) s2]
      [(member (car s1) s2) (union (cdr s1) s2)]
      [else (cons (car s1) (union (cdr s1) s2))])))

(define union*
  (lambda args
    (union*-aux args)))

(define union*-aux
  (lambda (args)
    (cond
      [(null? args) '()]
      [(null? (cdr args)) (car args)]
      [else (union (union (car args) (cadr args))
                   (union*-aux (caddr args)))])))

(define remove*
  (lambda (x* ls)
    (cond
      [(null? x*) ls]
      [else (remove* (cdr x*) (remove (car x*) ls))])))

(define lookup
  (lambda (x gamma)
    (cond
      [(assq x gamma) => cdr]
      [else (error 'lookup (format "annotate-free-vars--unbound variable: ~s in gamma: ~s\n" x gamma))])))

)
