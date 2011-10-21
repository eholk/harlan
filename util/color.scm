(library
    (util color)
  (export set-color)
  (import
   (rnrs))

  (define set-color
    (case-lambda
      ((color port)
       (put-string
        port
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
          ((light-while)   "\x1B;[37;1m"))))
      ((color)
       (set-color color (current-output-port))))))
