(require-package 'magit)
(require-package 'git-gutter)

(global-set-key (kbd "C-x g") 'magit-status)

(global-git-gutter-mode t)
;;; TODO: git-gutter doesn't support nlinum currently
(git-gutter:linum-setup)		; this is useless
;;; jump to previous/next hunk
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

(diminish 'git-gutter-mode)

(provide 'init-git)
