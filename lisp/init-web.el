;;; web related mode
(require-package 'web-mode)
(require-package 'restclient)
(require-package 'emmet-mode)
(require-package 'js2-mode)
;; (require-package 'tide)
(require-package 'coffee-mode)
(require-package 'yaml-mode)
(require-package 'protobuf-mode)

(defun emmet-navigation-keys ()
  (local-set-key (kbd "M-p") 'emmet-prev-edit-point)
  (local-set-key (kbd "M-n") 'emmet-next-edit-point))
(add-hook 'emmet-mode-hook 'emmet-navigation-keys)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)

(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-current-column-highlight t)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(custom-set-variables '(coffee-tab-width 4))

(provide 'init-web)
