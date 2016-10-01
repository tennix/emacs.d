(require-package 'unfill)
(require-package 'rainbow-delimiters)
(require-package 'nlinum)
(require-package 'highlight-indent-guides)

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 column-number-mode t
 make-backup-files nil
 tooltip-delay 1.5
 ;; visible-bell t
 inhibit-startup-message t
 initial-scratch-message ""
 )

(show-paren-mode t)
;; (scroll-bar-mode -1)
;; (tool-bar-mode -1)
(global-nlinum-mode)
(global-subword-mode)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(setq highlight-indent-guides-method 'character)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(require-package 'highlight-symbol)
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))
(global-set-key (kbd "C-x M-r") 'highlight-symbol-query-replace)
(diminish 'highlight-symbol-mode)

(global-set-key (kbd "C-x ?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

(if (display-graphic-p)
    (progn
      (menu-bar-mode -1)
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      ))

(defun trailing-whitespace-mode ()
  (progn
    (setq show-trailing-whitespace t)
    (setq indicate-unused-lines t)
    ))
(add-hook 'prog-mode-hook 'trailing-whitespace-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'init-editing-utils)
