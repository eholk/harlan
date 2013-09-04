(define-derived-mode harlan-mode scheme-mode "Harlan"
  "Harlan is a major mode for editing Harlan programs."

  (put 'kernel 'scheme-indent-function 1)
  (put 'kernel* 'scheme-indent-function 1)
  (put 'reduce 'scheme-indent-function 1)
  (put 'for 'scheme-indent-function 1)

  (font-lock-add-keywords nil
                          '(("kernel" . font-lock-keyword-face)
                            ("module" . font-lock-keyword-face)
                            ("for" . font-lock-keyword-face)
                            ("while" . font-lock-keyword-face)
                            ("return" . font-lock-keyword-face)
                            ("extern" . font-lock-keyword-face)
                            ("reduce" . font-lock-keyword-face)
                            ("%testspec" . font-lock-keyword-face)))
  )

(provide 'harlan-mode)
(setq auto-mode-alist (cons '("\\.kfc$" . harlan-mode) auto-mode-alist))
