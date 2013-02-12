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
     `(module . ,(expand-decls (cons '(import core) decls)))))

  (define-match expand-decls
    (((import ,name) . ,rest)
     (expand-decls (append (load-include (string-append
                                          (symbol->string name) ".kfc"))
                           rest)))
    ((,a . ,[d]) `(,a . ,d))
    (() '()))

  (define (load-include name)
    (let-values (((source _)
                  (read-source (string-append "lib/harlan/" name))))
      (match source
        ((module . ,decls) decls))))
  )
