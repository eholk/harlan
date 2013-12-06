;; -*- scheme -*-
(library
    (util compat)
  (export directory-list printf system unlink path-last path-root path-parent
          time with-output-to-string pretty-print make-parameter parameterize)
  (import (chezscheme))

  (define unlink delete-file))
