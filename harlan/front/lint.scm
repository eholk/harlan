(library
  (harlan front lint)
  (export lint)
  (import
   (elegant-weapons helpers)
   (elegant-weapons match)
   (elegant-weapons sets)
   (rnrs)
   (util compat)
   (util color))

  ;; The goal of the lint pass is to make sure the Harlan program is
  ;; basically well-formed. It should check for common programmer
  ;; errors and report warnings to the user. The warnings should
  ;; include suggestions for how to fix it. Ideally, a program that
  ;; passes lint and typechecking should make it all the way through
  ;; the rest of the compiler.
  ;;
  ;; This pass happens early on so that we still have most of the
  ;; user's original information.

  (define (report color header expr message)
    (with-color color (display (string-append "[" header "]  ")))
    (display "In expression...") (newline)
    (pretty-print expr)
    (display message) (newline) (newline))

  (define any-errors? 0)

  (define max-errors 10)
  
  (define (warn expr message)
    (report 'yellow "Warning" expr message))
  (define (err expr message)
    (report 'red "Error" expr message)
    (set! any-errors? (+ 1 any-errors?))
    (if (> any-errors? max-errors)
        (error 'lint "too many errors")))

  (define (find-repeats x*)
    (cond
      ((null? x*) '())
      ((pair? x*)
       (let ((dups (find-repeats (cdr x*))))
         (if (member (car x*) (cdr x*))
             (set-add dups (car x*))
             dups)))))
  
  (define (check-if expr env context)
    (if (eq? context 'expr)
        (match expr
          ((if ,t ,c)
           (err expr "Two-armed if found in expression context. Try adding a case for if the test is false.")))))

  (define (check-expr expr env)
    #f)

  (define (check-stmt stmt env) #f)
  
  (define (check-decl decl env context)
    (match decl
      ((define (,x ,a* ...) ,stmt)
       ;; Check for duplicate names
       (let ((dups (find-repeats a*)))
         (unless (null? dups)
           (err decl (string-append
                      "These identifiers are repeated in the arguments list: "
                      (join ", " (map symbol->string dups))
                      ".\nTry renaming the duplicates."))))
       (check-stmt stmt env))
      ;; TODO: We want some general logging facility to let us know
      ;; which cases we aren't handling.
      (,else #f)))
  
  (define (make-check-call name num-args)
    (lambda (expr env context)
      ;; TODO: It'd be nice if we had a way to warn about calling a
      ;; non-void return in statement context.
      (match expr
        ((,x ,a* ...)
         (if (= (length a*) num-args)
             (for-each (lambda (e) (check-expr e env)) a*)
             (err expr (format "~s expects ~d arguments but is being called with ~d arguments."
                               name num-args (length a*))))))))
  
  (define initial-env
    `((if . ,check-if)))

  ;; TODO: find a way to check if we have multiple top-level definitions
  
  ;; This takes a top-level decl and returns an entry for the environment 
  (define (extract-module-definition decl)
    (match decl
      ((define (,x ,a* ...) ,e ...)
       (list (cons x (make-check-call x (length a*)))))
      ((define-datatype . ,_)
       ;; TODO: add each of the constructors. We also should add a way
       ;; to check match patterns.
       '())
      ((define-macro . ,_)
       ;; TODO: Who knows what to do for this?
       '())
      ((extern ,x (,a* ...) -> ,t)
       (list (cons x (make-check-call x (length a*)))))
      (,else (warn else "unrecognized top-level definition") '())))
  
  (define (lint m)
    (match m
      ((module ,decl* ...)
       (let ((env (append (apply append
                                 (map extract-module-definition decl*))
                          initial-env)))
         (for-each (lambda (d) (check-decl d env 'top-level)) decl*)))
      (,else (err else "This is not a valid module")))
    (if (> any-errors? 0)
        (error 'lint
               (string-append (number->string any-errors?) " errors found"))
        m)))
