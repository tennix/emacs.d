(require-package 'helm)
(require-package 'helm-dash)

(setq helm-dash-browser-func 'eww)
(setq helm-dash-common-docsets '("Rust" "Go" "Python_3"))

(provide 'init-helm)
