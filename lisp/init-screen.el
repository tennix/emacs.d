(require-package 'elscreen)
(require-package 'elscreen-persist)

(setq desktop-files-not-to-save "")
(setq elscreen-persist-file (expand-file-name ".elscreen" user-emacs-directory))
(elscreen-persist-mode 1)
(add-hook 'after-init-hook 'elscreen-start)

(provide 'init-screen)
