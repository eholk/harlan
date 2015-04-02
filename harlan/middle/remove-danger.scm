(library
  (harlan middle remove-danger)
  (export remove-danger)
  (import
   (rnrs)
   (nanopass)
   (harlan middle languages)
   (only (elegant-weapons helpers) gensym)
   (harlan compile-opts))

  ;; This pass inserts vector bounds checks. So far we only support
  ;; this and allocation failures. The checking code for allocation
  ;; failures is inserted later, in uglify-vectors.
  ;;
  ;; The way we handle errors in kernels is going to drive us
  ;; completely insane someday, so here is a note to my future self to
  ;; hopefully lessen the pain. We support two kinds of errors, bounds
  ;; checks and allocation failures. To handle these, we add a
  ;; parameter to the kernel called the danger vector. Every
  ;; successful kernel thread should set their entry in the danger
  ;; vector to 0. If an error is detected, they fill the danger vector
  ;; with the correct error code and then return. The host code will
  ;; then iterate over the danger vector to make sure all the kernels
  ;; completed successfully.
  ;;
  ;; Now, you're probably wondering where all this code gets
  ;; generated. Well, the pass your looking at now inserts the array
  ;; bounds checks. The returnify-kernels pass is responsible for
  ;; adding the danger vector (errors are just a specially kind of
  ;; return, if you squint right). It gets better though. The
  ;; returnify-kernels pass also walks the whole AST and replaces all
  ;; the calls to (error ...) with code to fill the danger vector and
  ;; return. In this case, we've only ever generated array bounds
  ;; checks, so we just assume all of the error forms are bounds check
  ;; errors. So far so good.
  ;;
  ;; Next, we need to deal with the allocation errors. The types and
  ;; grammars are tricky to do this early on, and plus, we have lots
  ;; of allocating forms. Instead, we add the checks in
  ;; uglify-vectors. Basically, if alloc returns 0, there was a
  ;; failure to allocate. This generates new error forms, which won't
  ;; get erased from kernels in returnify-kernels like the ones from
  ;; remove-danger because returnify-kernels has already
  ;; won. Furthermore, by the time we get to uglify-vectors, it's hard
  ;; to figure out where the danger vector is. Instead we just punt
  ;; and let the error forms go to the end. Sadly, the expand into a
  ;; call to harlan_error, which OpenCL has no idea what to do with.
  ;;
  ;; But now, gpu_only.h has a macro called harlan_error which sets
  ;; the danger vector and returns. Since we know the only error forms
  ;; that made it that far are allocation failures, we set the danger
  ;; vector with the allocation failure code. But wait, didn't we just
  ;; say it's hard to find the danger vector after returnify-kernels?
  ;; Yes, we did. Well, the dirty little secret is that we decided to
  ;; always call it `danger` so the macro can find it.
  ;;
  ;; This is software engineering at its finest.

  (define-pass remove-danger : M7 (m) -> M7.0.0 ()
    (definitions
      (define (type-of e)
        (with-output-language
         (M7.0.0 Rho-Type)
         (nanopass-case
          (M7.0.0 Expr) e
          ((var ,t ,x) t)
          ((vector-ref ,t ,e1 ,e2) t)
          ((let ((,x ,t ,e) ...) ,[e0]) e0)
          ((kernel ,t ,r (,e* ...) (((,x0 ,t0) (,e1 ,t1) ,i*) ...) ,e) t)
          ((begin ,[e] ,[e*] ...)
           (let loop ((e* e*))
             (cond
               ((null? e*) e)
               ((null? (cdr e*)) (car e*))
               (else (loop (cdr e*))))))
          ((vector ,t ,r ,e) t)
          ((if ,e1 ,[e2] ,e3) e2)
          ((if ,e1 ,[e2]) e2)
	  ((call ,[e] ,e* ...)
	   (nanopass-case (M7.0.0 Rho-Type) e
			  ((fn (,t* ...) ,-> ,t) t)
			  (else (error 'remove-danger::type-of "illegal call target"
				       (unparse-M7.0.0 e)))))
          ((error ,x) 'void)
          (else (error 'remove-danger::type-of "unrecognized expr"
                       (unparse-M7.0.0 e)))))))

    (Expr
     : Expr (e) -> Expr ()

     ((vector-ref ,[t] ,[e0] ,[e1])
      (if (danger-zone)
          `(vector-ref ,t ,e0 ,e1)
          (let ((v-var (gensym 'vec))
                (i-var (gensym 'refindex))
                (vt    (type-of e0)))
            `(let ((,v-var ,vt ,e0)
                   (,i-var int ,e1))
               (begin
                 (if (>= (var int ,i-var) (length (var ,vt ,v-var)))
                     (error ,(gensym 'vector-length-error)))
                 (vector-ref ,t (var ,vt ,v-var) (var int ,i-var)))))))
     ((unsafe-vector-ref ,[t] ,[e0] ,[e1])
      `(vector-ref ,t ,e0 ,e1))))

  ;; end library
  )
