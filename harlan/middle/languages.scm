;; Definition of all the Nanopass Languages.

(library
    (harlan middle languages)
  (export M0 unparse-M0 parse-M0
          M1 unparse-M1
          M2 unparse-M2
          M3 unparse-M3 parse-M3
          M5 unparse-M5 parse-M5
          M6 unparse-M6
          M7 unparse-M7 parse-M7
          M7.0.0 unparse-M7.0.0
          M7.0 unparse-M7.0 parse-M7.0
          M7.1 unparse-M7.1
          ;;M7.2 unparse-M7.2
          M8 unparse-M8 parse-M8
          M9 unparse-M9
          M9.1 unparse-M9.1
          M9.2 unparse-M9.2
          M9.2.1 unparse-M9.2.1
          M9.2.2 unparse-M9.2.2
          M9.3 unparse-M9.3
          ;;M10 unparse-M10
          )
  (import
   (rnrs)
   (nanopass)
   (only (elegant-weapons helpers) binop? scalar-type? relop?)
   (harlan middle languages1)
   (harlan middle languages2)
   (harlan middle languages M9))
  )
