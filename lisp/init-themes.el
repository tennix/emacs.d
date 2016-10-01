;; (require-package 'monokai-theme)
(require-package 'solarized-theme)

;;; set font according to system type
(cond ((string-equal system-type "darwin") (set-default-font "monaco-13"))
      ((string-equal system-type "gnu/linux") (set-default-font "Fira Mono Medium-12")))

(setq solarized-use-variable-pitch nil)
(setq solarized-scale-org-headlines nil)

;;; theme: monokai
;; (load-theme 'monokai t)
(if (display-graphic-p)
    (load-theme 'solarized-dark t)
  )


(provide 'init-themes)
