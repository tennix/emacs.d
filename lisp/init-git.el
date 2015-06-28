;; TODO: link commits from vc-log to magit-show-commit
;; TODO: smerge-mode
(require-package 'magit)
(require-package 'git-blame)
(require-package 'git-commit-mode)
(require-package 'git-rebase-mode)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'git-messenger) ;; Though see also vc-annotate's "n" & "p" bindings
(require-package 'git-timemachine)


(setq-default
 magit-last-seen-setup-instructions "1.4.0"
 magit-save-some-buffers nil
 magit-process-popup-time 10
 magit-diff-refine-hunk t
 magit-completing-read-function 'magit-ido-completing-read)

;; Hint: customize `magit-repo-dirs' so that you can use C-u M-F12 to
;; quickly open magit on any one of your projects.
(global-set-key [(meta f12)] 'magit-status)

(after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-goto-parent-section))

(require-package 'fullframe)
(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(add-hook 'git-commit-mode-hook 'goto-address-mode)
(after-load 'session
  (when (boundp 'session-mode-disable-list) ; newer Emacsen
    (add-to-list 'session-mode-disable-list 'git-commit-mode)))


;;; When we start working on git-backed files, use git-wip if available

(after-load 'magit
  (when (executable-find magit-git-executable)
    (global-magit-wip-save-mode)
    (diminish 'magit-wip-save-mode)))

(after-load 'magit
  (diminish 'magit-auto-revert-mode))


(when *is-a-mac*
  (after-load 'magit
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))



;; Convenient binding for vc-git-grep
(global-set-key (kbd "C-x v f") 'vc-git-grep)



(require-package 'git-messenger)
(global-set-key (kbd "C-x v p") #'git-messenger:popup-message)


(provide 'init-git)
