(require-package 'unfill)

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 column-number-mode t
 make-backup-files nil
 tooltip-delay 1.5
 visible-bell t
 inhibit-startup-message t
 initial-scratch-message ""
 )

(show-paren-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode)
(global-subword-mode)

(require-package 'highlight-symbol)
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))
(global-set-key (kbd "C-x M-r") 'highlight-symbol-query-replace)

(global-set-key (kbd "C-x ?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

(provide 'init-editing-utils)
