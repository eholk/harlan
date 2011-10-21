(library
 (print-c)
 (export format-c print-c join format-arg format-ident format-block unaryop? binop? relop?)
 (import (only (chezscheme) format)
         (rnrs)
         (util match))

 (define join
   (lambda (sep strings)
     (match strings
       (() "")
       ((,a) a)
       ((,a ,b) (string-append a sep b))
       ((,a ,b* ...)
         (string-append a sep (join sep b*))))))

 (define indent-level 0)
 (define push-indent
   (lambda () (set! indent-level (+ indent-level 1))))
 (define pop-indent
   (lambda () (set! indent-level (- indent-level 1))))

 (define indent-by
   (lambda (s n)
     (if (zero? n)
         s
         (string-append "    " (indent-by s (- n 1))))))

 (define indent
   (lambda (s)
     (indent-by s indent-level)))
 
 (define format-ident
   (lambda (ident)
     (unless (symbol? ident) (error 'format-ident "invalid symbol" ident))
     (symbol->string ident)))

 (define format-type
   (lambda (type)
     (match type
       (u64 "uint64_t")
       ((ptr ,[t])
        (string-append "__global " t "*"))
       ((,[t] ,[t*] ...)
        (if (null? t*)
            t
            (string-append t "< " (join ", " t*) " >")))
       (,x (guard (symbol? x))
           (symbol->string x))
       (,else (error 'format-type "Unknown type" else)))))

 (define format-arg
   (lambda (arg)
     (match arg
       ((,x ,t)
        (string-append (format-type t) " " (format-ident x))))))

 (define format-args
   (lambda (args)
     (join ", " (map format-arg args))))

 (define format-call-args
   (lambda (args)
     (join ", " (map format-expr args))))

 (define unaryop?
   (lambda (op)
     (case op
       ((length iota) #t)
       (else #f))))
 
 (define binop?
   (lambda (op)
     (case op
       ((< = bitwise-or + * - mod) #t)
       (else #f))))
 
 (define relop?
   (lambda (op)
     (case op
       ((< <= = > >=) #t)
       (else #f))))

 (define binop->string
   (lambda (op)
     (case op
       ((== =) "==")
       ((bitwise-or) "|")
       ((+) "+")
       ((*) "*")
       ((-) "-")
       ((mod) "%")
       (else (error 'binop->string "unknown binop" op)))))
 
 
 (define relop->string
   (lambda (op)
     (case op
       ((== =) "==")
       ((<) "<")
       ((>) ">")
       ((<=) "<=")
       ((>=) ">=")
       (else (error 'relop->string "unknown relop" op)))))

 (define escape-string-literal
   (lambda (s)
     (if (zero? (string-length s))
         ""
         (string-append
          (case (string-ref s 0)
            ((#\newline) "\\n")
            ((#\") "\\\"")
            (else (string (string-ref s 0))))
          (escape-string-literal
           (substring s 1 (string-length s)))))))
 
 (define format-expr
   (lambda (expr)
     (match expr
       ((field ,[obj] ,x)
        (guard (symbol? x))
        (string-append obj "." (symbol->string x)))
       ((field ,[obj] ,x ,[format-type -> t])
        (guard (symbol? x))
        (string-append obj "." (symbol->string x) "<" t ">"))
       ((vector-ref ,[v] ,[i])
        (string-append v "[" i "]"))
       ((sizeof ,[format-type -> t])
        (string-append "sizeof(" t ")"))
       ((deref ,[p])
        (string-append "*" p))
       ((cast ,[format-type -> t] ,[e])
        (string-append "((" t ")(" e "))"))
       ((addressof ,[e])
        (string-append "(&(" e "))"))
       ((,op ,[lhs] ,[rhs])
        (guard (binop? op))
        (string-append lhs " " (binop->string op) " " rhs))
       (,var (guard (symbol? var))
             (symbol->string var))
       (,n (guard (number? n))
           (number->string n))
       (,s (guard (string? s))
           (string-append "\"" (escape-string-literal s) "\""))
       ((,f ,args ...)
        (string-append (format-expr f) "(" (format-call-args args) ")"))
       (,else (error 'format-expr (format "bad expr: ~s" expr))))))
 
 (define format-stmt
   (lambda (stmt)
     (match stmt
       ((let ,[format-ident -> ident] ,[format-type -> type]
             ,[format-expr -> expr])
        (string-append type " " ident " = " expr ";"))
       ((let ,ident ,type ,expr* ...)
        (string-append (format-type type) " " (format-ident ident)
                       "(" (join ", " (map format-expr expr*)) ");"))
       ((block ,stmt* ...)
        (format-block `(block ,stmt* ...)))
       ((return ,expr)
        (string-append "return " (format-expr expr) ";"))
       ((print ,expr)
        (string-append "print(" (format-expr expr) ");"))
       ((print ,e1 ,e2)
        (string-append "print(" (format-expr e1) ", " (format-expr e2) ");"))       
       ((set! ,[format-expr -> x] ,[format-expr -> v])
        (string-append x " = " v ";"))
       ((vector-set! ,vec-expr ,i-expr ,val-expr)
        (string-append (format-expr vec-expr)
                       "[" (format-expr i-expr) "] = "
                       (format-expr val-expr) ";"))
       ((while (,[relop->string -> relop]
              ,[format-expr -> e1] ,[format-expr -> e2])
             ,[format-stmt -> stmt*] ...)
        (string-append "while(" e1 " " relop " " e2 ") {\n"
                       (join "\n" stmt*)
                       "\n}"))
       ((for (,[format-ident -> i]
              ,[format-expr -> start] ,[format-expr -> end])
             ,[format-stmt -> stmt*] ...)
        (string-append "for(int " i " = " start "; " i " < " end "; ++" i ") "
                       "{\n"
                       (join "\n" stmt*)
                       "\n}"))
       ((do ,[format-expr -> e*] ...)
        (join "\n" (map (lambda (e) (string-append e ";")) e*)))
       (,else (error 'format-stmt (format "bad stmt: ~s" stmt))))))
 
 (define format-block
   (lambda (block)
     (match block
       ((block ,stmt* ...)
        (string-append
         "{\n"
         (join "\n" (map format-stmt stmt*))
         "\n}"))
       (,else (error 'format-block "bad block")))))
 
 (define format-decl
   (lambda (decl)
     (match decl
       ((global ,type ,name ,args ...)
        (string-append
         (format-type type) " " (format-ident name)
         (if (null? args)
             ""
             (string-append "(" (format-call-args args) ")"))
         ";"))
       ((func ,type ,name (,args ...) ,stmt* ...)
        (string-append (format-type type) " " (format-ident name)
                       "(" (format-args args) ")\n"
                       (format-block `(block ,stmt* ...))))
       ((extern ,[format-type -> type] ,[format-ident -> name]
                (,[format-type -> args] ...))
        (string-append type " " name "(" (join ", " args) ");\n"))
       (,else (error 'format-decl "Invalid declaration" else)))))
 
 (define format-c
   (lambda (decls)
     (string-append
      "#include \"harlan.hpp\"\n\n"
      (join "\n\n" (map format-decl decls))
      "\n")))

 (define print-c
   (lambda (decls)
     (display (format-c decls)))))
         
