;; (require-package 'monokai-theme)
(require-package 'solarized-theme)

;;; set font according to system type
(when (string-equal system-type "darwin") (set-default-font "monaco-13")
      (string-equal system-type "gnu/linux") (set-default-font "Fira Mono Medium-12"))

;;; theme: monokai
;; (load-theme 'monokai t)
(load-theme 'solarized-dark t)

(provide 'init-themes)
