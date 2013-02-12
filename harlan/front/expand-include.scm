(library
    (harlan front expand-include)
  (export expand-include)
  (import
   (except (chezscheme) gensym)
   (only (elegant-weapons helpers) define-match)
   (harlan driver)
   (elegant-weapons match))

  (define-match expand-include
    ((module . ,decls)
     `(module . ,(expand-decls (cons '(import core) decls) '()))))

  (define (expand-decls decls libs)
    (match decls
      (((import ,name) . ,rest)
       (if (memq name libs)
           (expand-decls rest libs)
           (expand-decls (append (load-include (string-append
                                                (symbol->string name) ".kfc"))
                                 rest)
                         (cons name libs))))
      ((,a . ,[d]) `(,a . ,d))
      (() '())))

  (define (load-include name)
    (let-values (((source _)
                  (read-source (string-append "lib/harlan/" name))))
      (match source
        ((module . ,decls) decls))))
  )
