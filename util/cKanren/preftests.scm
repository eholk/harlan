(load "mk.scm")
(load "ck.scm")
(load "pref.scm")

(import (util cKanren mk))
(import (util cKanren ck))
(import (util cKanren pref))

(pretty-print
  (run* (q) (prefo q '(int float u64))))

