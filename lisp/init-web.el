;;; web related mode
(require-package 'web-mode)
(require-package 'restclient)
(require-package 'emmet-mode)
(require-package 'js2-mode)
(require-package 'json-mode)
(require-package 'tide)
(require-package 'coffee-mode)
(require-package 'yaml-mode)
(require-package 'protobuf-mode)
(require-package 'typescript-mode)


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

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

(provide 'init-web)
