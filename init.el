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

(setq straight-vc-git-default-clone-depth 1)

;;; Use another file to store customizations instead of init.el
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

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
(savehist-mode 1)    ;; save minibuffer history

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

;; pulse when switch buffer
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))
(dolist (command '(scroll-up-command scroll-down-command
				     recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

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
(global-set-key (kbd "C-x C-n") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "C-x C-p") 'tab-bar-switch-to-prev-tab)

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

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs)

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

;; auto format using external programming language formatter
(use-package apheleia
  :config
  (apheleia-global-mode +1))

(use-package go-mode)

;; Lightweight LSP client
(use-package eglot)

;; A workaround when hitting "Too many open files" error with LSP
;; https://www.blogbyben.com/2022/05/gotcha-emacs-on-mac-os-too-many-files.html
(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

;; Text search with ripgrep
;; wgrep is enabled by this
;; C-c s: start magit like buffer
(use-package rg
  :config
  (rg-enable-default-bindings))

(use-package ctrlf
  :init
  (ctrlf-mode +1))

(use-package vertico
  ;; :straight (:files (:defaults "extensions/*")
  ;;                   :includes (vertico-directory
  ;; 			       vertico-buffer
  ;; 			       vertico-mouse))
  :init
  (vertico-mode))

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
	 ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
	 ("C-x b" . consult-buffer)
	 ("M-y" . consult-yank-pop)
	 ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; Useful commands:
;; consult-lsp-file-symbols: Select current file symbols
;; consult-lsp-symbols: Select symbols from current workspace
;; consult-lsp-diagnostics: Select diagnostics from current workspace
;; (use-package consult-lsp)
(use-package consult-projectile)

;; This is lightweight alternative to company-mode
;; M-SPC corfu-insert-separator
(use-package corfu
  :init
  (setq corfu-auto t
	corfu-quit-no-match 'separator)
  (global-corfu-mode))

(straight-use-package
 '(popon :type git :repo "https://codeberg.org/akib/emacs-popon.git"))

(straight-use-package
 '(corfu-terminal
   :type git
   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))

(unless (display-graphic-p)
  (corfu-terminal-mode +1))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  ;; The :init configuration is always executed (Not lazy!)
  :init
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
  ; ("C-h B" . embark-bindings);; alternative for `describe-bindings'
   )
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
(use-package yaml-mode)

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

(use-package protobuf-mode
  :defer t)
(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook 'eglot-ensure))
(use-package toml-mode)

;; docker-tramp allows to edit files in docker container using tramp
;; C-x C-f /docker:user@container_name:/path/to/file
(use-package docker-tramp
  :defer t)

(use-package typescript-mode
  :init
  (setq typescript-indent-level 2))
(use-package json-mode
  :config
  (setq json-reformat:indent-width 2))
(setq js-indent-level 2)

(use-package citre
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  ;; Bind your frequently used commands.  Alternatively, you can define them
  ;; in `citre-mode-map' so you can only use them when `citre-mode' is enabled.
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  :config
  (setq
   ;; Set these if readtags/ctags is not in your path.
   citre-readtags-program "/usr/local/bin/readtags"
   citre-ctags-program "/usr/local/bin/ctags"
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   citre-project-root-function #'projectile-project-root
   ;; Set this if you want to always use one location to create a tags file.
   citre-default-create-tags-file-location 'global-cache
   ;; See the "Create tags file" section above to know these options
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t
   ;; By default, when you open any file, and a tags file can be found for it,
   ;; `citre-mode' is automatically enabled.  If you only want this to work for
   ;; certain modes (like `prog-mode'), set it like this.
   citre-auto-enable-citre-mode-modes '(prog-mode)))

;; ;;; init.el ends here
