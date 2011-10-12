(library
 (helpers)
 (export gensym iota)
 (import (rnrs))

 (define gensym
   (let ((c 0))
     (lambda (x)
       (unless (symbol? x) (error 'gensym "invalid symbol" x))
       (set! c (+ 1 c))
       (string->symbol
        (string-append (symbol->string x) "_" (number->string c))))))

 (define iota
   (lambda (n)
     (let loop ([i 0])
       (cond
         [(= i n) '()]
         [else (cons i (loop (+ i 1)))]))))
 )