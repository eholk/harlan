(library
    (util compat)
  (export directory-list printf system unlink)
  (import
   (rnrs)
   (only (vicare) printf)
   (only (vicare posix) system unlink opendir readdir/string))

  (define (directory-list path)
    (let ((dir (opendir path)))
      (let loop ((file (readdir/string dir)))
        (if file
            (cons file (loop (readdir/string dir)))
            '())))))
