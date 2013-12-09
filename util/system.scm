(library
    (util system)
  (export join-path HARLAND)
  (import
   (rnrs)
   (only (util compat) make-parameter)
   (only (elegant-weapons helpers) join))

  (define (join-path . components)
    (join "/" components))

  (define HARLAND (make-parameter "."))
  
  ;; end library
  )
   
