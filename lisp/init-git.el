(require-package 'magit)
(require-package 'git-gutter)

(global-set-key (kbd "C-x g") 'magit-status)
(global-git-gutter-mode t)

(diminish 'git-gutter-mode)

(provide 'init-git)
