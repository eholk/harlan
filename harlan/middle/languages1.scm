;; In an effort to reign in Harlan's memory usage, I've split
;; languages.scm into a couple files.

(library
    (harlan middle languages1)
  (export M0 unparse-M0 parse-M0
          M1 unparse-M1
          M2 unparse-M2
          M3 unparse-M3 parse-M3
          M5 unparse-M5 parse-M5
          M6 unparse-M6
          M7 unparse-M7 parse-M7)
  (import
   (harlan middle languages M0-3)
   (harlan middle languages M5)
   (harlan middle languages M6)
   (harlan middle languages M7)))
