(library
    (util color)
  (export set-color with-color set-color-string format-in-color)
  (import
   (rnrs))

  (define (set-color-string color)
    (case color
      ((default)       "\x1B;[39m")
      ((black)         "\x1B;[30m")
      ((red)           "\x1B;[31m")
      ((green)         "\x1B;[32m")
      ((yellow)        "\x1B;[33m")
      ((blue)          "\x1B;[34m")
      ((magenta)       "\x1B;[35m")
      ((cyan)          "\x1B;[36m")
      ((while)         "\x1B;[37m")
      ((light-black)   "\x1B;[30;1m")
      ((light-red)     "\x1B;[31;1m")
      ((light-green)   "\x1B;[32;1m")
      ((light-yellow)  "\x1B;[33;1m")
      ((light-blue)    "\x1B;[34;1m")
      ((light-magenta) "\x1B;[35;1m")
      ((light-cyan)    "\x1B;[36;1m")
      ((light-while)   "\x1B;[37;1m")))
  
  (define set-color
    (case-lambda
      ((color port)
       (put-string port (set-color-string color))
       (flush-output-port port))
      ((color)
       (set-color color (current-output-port)))))

  (define-syntax with-color
    (syntax-rules ()
      ((_ color expr ...)
       (begin (set-color color)
              expr ...
              (set-color 'default)))))

  ;; From TSPL4
  (define (object->string x)
    (call-with-string-output-port
     (lambda (p) (put-datum p x)))) 
  
  (define (format-in-color color s)
    (string-append (set-color-string color)
                   (object->string s)
                   (set-color-string 'default)))
  )
