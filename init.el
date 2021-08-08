;;; init.el --- My Emacs init configuration

;;; Commentary:

;; This is Tennix's Emacs configuration.
;; The latest version can be found at https://github.com/tennix/emacs.d

;;; Code:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/master/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Emacs auto generates customization file
;; custom.el file must be exist
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; some good default settings
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default blink-cursor-interval 0.4
	      bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
	      case-fold-search t
	      column-number-mode t
	      inhibit-x-resources t	; avoid dark cursor when starting with emacs daemon
	      make-backup-files nil
	      tooltip-delay 1.5
	      read-process-output-max (* 1024 1024) ; 1mb
	      gc-cons-threshold 100000000
	      inhibit-startup-message t
	      initial-scratch-message ""
	      ediff-split-window-function 'split-window-horizontally
	      ediff-window-setup-function 'ediff-setup-windows-plain)

;;; builtin parenthesis editing
(show-paren-mode t)  ;; faster than show-smartparens-mode
(save-place-mode 1)  ;; save cursor place when close file

;;; subword mode for prog-mode CamelCase and snake_case word
(add-hook 'prog-mode-hook 'subword-mode)
;; Enable hl-line-mode for prog mode and text-mode
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'text-mode-hook 'hl-line-mode)


;;; automatically revert files
(setq-default global-auto-revert-non-file-buffers t
	      auto-revert-verbose nil)
(global-auto-revert-mode)

;;; recent files
(setq-default recentf-max-saved-items 500
	      recentf-exclude '("^/tmp/"
				"^/ssh:"
				"^/docker:"
				"\\.emacs\\.d/elpa/"))
(recentf-mode 1)

(defun eh-ivy-return-recentf-index (dir)
  (when (and (boundp 'recentf-list)
             recentf-list)
    (let ((files-list
           (cl-subseq recentf-list
                      0 (min (- (length recentf-list) 1) 20)))
          (index 0))
      (while files-list
        (if (string-match-p dir (car files-list))
            (setq files-list nil)
          (setq index (+ index 1))
          (setq files-list (cdr files-list))))
      index)))

(defun eh-ivy-sort-file-function (x y)
  (let* ((x (concat ivy--directory x))
         (y (concat ivy--directory y))
         (x-mtime (nth 5 (file-attributes x)))
         (y-mtime (nth 5 (file-attributes y))))
    (if (file-directory-p x)
        (if (file-directory-p y)
            (let ((x-recentf-index (eh-ivy-return-recentf-index x))
                  (y-recentf-index (eh-ivy-return-recentf-index y)))
              (if (and x-recentf-index y-recentf-index)
                  ;; Directories is sorted by `recentf-list' index
                  (< x-recentf-index y-recentf-index)
                (string< x y)))
          t)
      (if (file-directory-p y)
          nil
        ;; Files is sorted by mtime
        (time-less-p y-mtime x-mtime)))))

;;; dealing with trailing whitespace
(defun trailing-whitespace-mode ()
  "Show whitespaces and unused lines."
  (progn
    (setq show-trailing-whitespace t)
    (setq indicate-unused-lines t)))
(add-hook 'prog-mode-hook 'trailing-whitespace-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;;; disable menu-bar, tool-bar, scroll-bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(if window-system
    (scroll-bar-mode -1))

;;; kill current line when no region is active
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill current line."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
	   (line-beginning-position 2)))))

;;; make C-h and C-w same as in shell
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-x ?") 'help-command)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

;;; increase or decrease text scale conveniently
(global-set-key (kbd "S-+") 'text-scale-increase)
(global-set-key (kbd "S--") 'text-scale-decrease)

;;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;; use-package-report to display use-package statistics
(setq use-package-compute-statistics t)
;; vterm: best terminal in emacs
;; C-c C-t: toggle vterm-copy-mode
;; better to use tmux inside vterm
(use-package vterm
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-buffer-name-string "vterm %s") ; Define prompt in bashrc/zshrc: PROMPT_COMMAND='echo -ne "\033]0;${PWD}\007"'
  (setq vterm-term-environment-variable "xterm"))

;; Making M-y to paste kill-ring work for vterm
(defun vterm-counsel-yank-pop-action (orig-fun &rest args)
  (if (equal major-mode 'vterm-mode)
      (let ((inhibit-read-only t)
            (yank-undo-function (lambda (_start _end) (vterm-undo))))
        (cl-letf (((symbol-function 'insert-for-yank)
               (lambda (str) (vterm-send-string str t))))
            (apply orig-fun args)))
    (apply orig-fun args)))
(advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action)

;; doom-themes are a collection of beatiful themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  ;; doom-one, doom-gruvbox, doom-zenburn looks very nice
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))
;; Run M-x all-the-icons-install-fonts to install the fonts
(use-package all-the-icons)
;; This makes the virtual buffers darker than file buffers
(use-package solaire-mode)
(solaire-global-mode +1)
(use-package doom-modeline
  :init (doom-modeline-mode 1))
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; org insert code: C-c C-,
(setq org-log-done 'time)
(setq org-agenda-files (list "~/my-org-files/"))
(use-package org-roam
      :custom
      (org-roam-directory (file-truename "~/my-org-files/"))
      :init
      (setq org-roam-v2-ack t)
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n i" . org-roam-node-insert)
             ("C-c n c" . org-roam-capture)
             ;; Dailies
             ("C-c n j" . org-roam-dailies-capture-today))
      :config
      (org-roam-setup)
      ;; If using org-roam-protocol
      (require 'org-roam-protocol))

(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

;;; Read environment variable from shell config
(use-package exec-path-from-shell
  :when (memq window-system '(mac ns))
  :init
  (setq exec-path-from-shell-arguments nil)
  :config
  (dolist (var '("SSH_AUTH_SOCK"
		 "SSH_AGENT_PID"
		 "GPG_AGENT_INFO"
		 "LANG"
		 "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; Expand region increases the selected region by semantic units
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;;; which-key: promp a window to display key when press incomplete key prefix
(use-package which-key
  :config
  (which-key-mode))

;;; Highlight-indent-guides: similar to sublime-text
(use-package highlight-indent-guides
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-suppress-auto-error t))

;;; smartparens: strcutural parenthesis editing
;; https://ebzzry.io/en/emacs-pairs/
;; https://github.com/Fuco1/smartparens/wiki/Working-with-expressions
;; sp-forward-sexp                  ;; C-M-f
;; sp-backward-sexp                 ;; C-M-b
;; sp-down-sexp                     ;; C-M-d
;; sp-backward-down-sexp            ;; C-M-a
;; sp-up-sexp                       ;; C-M-e
;; sp-backward-up-sexp              ;; C-M-u
;; sp-next-sexp                     ;; C-M-n
;; sp-previous-sexp                 ;; C-M-p
;; sp-beginning-of-sexp             ;; C-S-d
;; sp-end-of-sexp                   ;; C-S-a
;; sp-kill-sexp                     ;; C-M-k
;; sp-backward-kill-sexp            ;; C-- C-M-k
;; sp-copy-sexp                     ;; C-M-w
;; sp-backward-copy-sexp            ;; C-- C-M-w
;; sp-unwrap-sexp                   ;; M-<delete>
;; sp-backward-unwrap-sexp          ;; M-<backspace>
;; sp-transpose-sexp                ;; C-M-t
;; sp-splice-sexp                   ;; M-D
;; sp-splice-sexp-killing-forward   ;; C-M-<delete>
;; sp-splice-sexp-killing-backward  ;; C-M-<backspace>
;; sp-splice-sexp-killing-around    ;; C-S-<backspace>
;; C-M-Space: mark word after cursor and then input any kind of paren including *, `, _, ', " in markdown mode
(use-package smartparens
  :init
  (add-hook 'prog-mode-hook #'smartparens-strict-mode)
  (add-hook 'markdown-mode-hook #'smartparens-strict-mode)
  :config
  (require 'smartparens-config))

;;; switch-window: switch other window with a number
(use-package switch-window
  :bind (("C-x o" . switch-window)))

;;; Projectile: Project navigation and management library for Emacs
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;;; Company mode: modular in-buffer completion framework for Emacs
(use-package company
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq company-tooltip-align-annotations t))
;; Make C-h and C-w work as usual in company mode
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-h") 'delete-backward-char)
  (define-key company-active-map (kbd "C-w") 'backward-kill-word))

(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save))

;;; Flycheck for syntax check
(use-package flycheck
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (add-hook 'markdown-mode-hook 'flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
	flycheck-idle-change-delay 0.8
	flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

;; lsp: LSP client
;; M-.: go to definition
;; M-,; go back
(use-package lsp-mode
  :config
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  :commands (lsp lsp-deferred)
  :hook
  (go-mode . lsp-deferred)
  (rust-mode . lsp-deferred))
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
;; (use-package lsp-ui
;;   :commands lsp-ui-mode)
(use-package lsp-ivy)

;; Text search with ripgrep
;; wgrep is enabled by this
;; C-c s: start magit like buffer
(use-package rg
  :config
  (rg-enable-default-bindings))

;;; smex used with counsel
(use-package smex
  :config
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (smex-initialize))


;;; Ivy, swiper, counsel, avy
(use-package ivy
  :bind (:map ivy-minibuffer-map
	      ;; ido style directory navigation
	      ("C-j" . ivy-immediate-done)
	      ("RET" . ivy-alt-done))
  :config
  (setq ivy-use-virtual-buffers t
	ivy-virtual-abbreviate 'full
	ivy-extra-directories nil
	ivy-count-format "(%d/%d) "
	projectile-completion-system 'ivy
	ivy-initial-inputs-alist '((counsel-M-x . "^")
				   (man . "^")
				   (woman . "^")))
  ;; sort files by mtime
  (add-to-list 'ivy-sort-functions-alist
	       '(read-file-name-internal . eh-ivy-sort-file-function))
  (ivy-mode 1))
;; Counsel: search
;; counsel-rg, counsel-git-grep
(use-package counsel
  :config
  (setq counsel-mode-override-describe-bindings t)
  (counsel-mode))
(use-package swiper
  :bind (:map ivy-mode-map
	      ;; M-n: chooses the symbol at cursor point
	      ;; M-j: chooses next word
	      ("C-s" . swiper)))

;; avy: quick jump to visible point
(use-package avy
  :bind
  (("C-c j" . avy-goto-word-or-subword-1)))

;; dumb-jump: jump to definition without language server, use rg for fuzzy searching
(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-selector 'ivy
	dumb-jump-prefer-searcher 'rg))

;;; Git
(use-package magit
  :bind (("C-c g" . magit-status)
	 ("C-c b" . magit-blame-addition)
	 ("C-c l" . magit-log-buffer-file)))
(use-package diff-hl)
(global-diff-hl-mode)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
;; forge uses ghub to interact with github
(use-package forge :after magit)
;; Copy git repo link for the current file and line number
(use-package git-link)

;;; treemacs: tree layout project explorer
;; Command-b: toggle treemacs
;; session persist at $HOME/.emacs.d/.cache/treemacs-persist
(use-package treemacs
  :config
  (progn
    (setq treemacs-eldoc-display t
	  treemacs-width 30
	  treemacs-mgx-git-entries 5000)
    (treemacs-resize-icons 12)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind  (("s-b"   . treemacs)))      ; super-b is vscode file browser
(use-package lsp-treemacs
  :after lsp-mode treemacs)
(use-package treemacs-projectile
  :after treemacs projectile)
(use-package treemacs-magit
  :after treemacs magit)

;; Optional - provides snippet support.
(use-package yasnippet
  :config
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'yaml-mode-hook #'yas-minor-mode)
  (add-hook 'protobuf-mode-hook #'yas-minor-mode)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all))

(use-package dockerfile-mode)
(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook 'trailing-whitespace-mode))

;; edit-indirect is required for markdown mode to edit code blocks in indirect buffers using C-c '
(use-package edit-indirect :defer t)

;; Youdao dictionary
(use-package youdao-dictionary
  :bind ("C-c y" . youdao-dictionary-search-at-point))

;; markdown mode
;; Styling text is prefixed with C-c C-s key binding
;; Toggle command is prefixed with C-c C-x key binding
;; C-c C-l: insert link
;; C-c C-i: insert image
;; C-c C-s i: insert italic
;; C-c C-s c: insert code
;; C-c C-s C: insert gfm code block
;; C-c C-s d: strikethrough text
;; C-c < and C-c >: shift selected region
;; C-c C-n and C-c C-p: outline navigation
(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . gfm-mode)
	 ("\\.markdown\\'" . markdown-mode)))

(use-package protobuf-mode)
(use-package flycheck-rust)
(use-package rust-mode
  :hook ((rust-mode . flycheck-rust-setup)))
(use-package toml-mode)
;; docker-tramp allows to edit files in docker container using tramp
;; C-x C-f /docker:user@container_name:/path/to/file
(use-package docker-tramp)

(use-package typescript-mode
  :defer t
  :config (add-hook 'typescript-mode-hook #'tide-mode))
(use-package tide
  :defer t
  :after (typescript-mode company flycheck)
  :config (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions nil :placeOpenBraceOnNewLineForFunctions nil :insertSpaceAfterOpeningAndBeforeClosingNonemptyParenthesis nil :insertSpaceAfterOpeningAndBeforeClosingNoneemptyBrackets nil :insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces nil))
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;;; init.el ends here
