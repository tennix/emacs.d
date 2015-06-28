(require 'emmet-mode)

(defun emmet-navigation-keys ()
  (local-set-key (kbd "M-p") 'emmet-prev-edit-point)
  (local-set-key (kbd "M-n") 'emmet-next-edit-point))
(add-hook 'emmet-mode-hook 'emmet-navigation-keys)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)

(provide 'init-emmet)
