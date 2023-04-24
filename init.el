;;; init.el --- My Emacs init configuration

;;; Commentary:

;; This is Tennix's Emacs configuration.
;; The latest version can be found at https://github.com/tennix/emacs.d

;;; Code:
(defvar bootstrap-version)
(setq straight-use-package-by-default t
      use-package-compute-statistics t;; use-package-report to display use-package statistics
      straight-vc-git-default-clone-depth '(1 single-branch))
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/master/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'cl-lib)
;; Below is copied from https://github.com/manateelazycat/lazycat-emacs/blob/master/site-start.el
(defun add-subdirs-to-load-path (search-dir)
  (interactive)
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             ;; 过滤出不必要的目录，提升 Emacs 启动速度
             (cl-remove-if
              #'(lambda (subdir)
                  (or
                   ;; 不是目录的文件都移除
                   (not (file-directory-p (concat dir subdir)))
                   ;; 父目录、 语言相关和版本控制目录都移除
                   (member subdir '("." ".."
                                    "dist" "node_modules" "__pycache__"
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github"))))
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        ;; 目录下有 .el .so .dll 文件的路径才添加到 `load-path' 中，提升 Emacs 启动速度
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                ;; .so .dll 文件指非 Elisp 语言编写的 Emacs 动态库
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))

          ;; 注意：`add-to-list' 函数的第三个参数必须为 t ，表示加到列表末尾
          ;; 这样 Emacs 会从父目录到子目录的顺序搜索 Elisp 插件，顺序反过来会导致 Emacs 无法正常启动
          (add-to-list 'load-path subdir-path t))

        ;; 继续递归搜索子目录
        (add-subdirs-to-load-path subdir-path)))))

(add-subdirs-to-load-path "~/.emacs.d/site-lisp")

;; use-package
(straight-use-package 'use-package)

(use-package emacs
  :init
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq blink-cursor-interval 0.4
	bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
	case-fold-search t
	column-number-mode t
	inhibit-x-resources t ;; avoid dark cursor when starting with emacs daemon
	;; confirm-kill-emacs 'y-or-n-p ; confirm before exit
	make-backup-files nil
	sh-basic-offset 2
	tooltip-delay 1.5
	read-process-output-max (* 5 1024 1024) ;; 5mb
	gc-cons-threshold (* 10 1024 1024) ;; 10mb, avoid too large otherwise the gc will hang for a while when typing
	inhibit-startup-message t
	initial-scratch-message ""
	ediff-split-window-function 'split-window-horizontally
	ediff-window-setup-function 'ediff-setup-windows-plain
	global-auto-revert-non-file-buffers t
	auto-revert-verbose t)
  (show-paren-mode t) ;; faster than show-smartparens-mode
  (save-place-mode 1) ;; save cursor place when close file
  (savehist-mode 1) ;; save minibuffer history
  (global-auto-revert-mode) ;; automatically revert files
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  :hook
  (prog-mode . subword-mode)
  (prog-mode . hl-line-mode)
  (text-mode . hl-line-mode))
;;; Use another file to store customizations instead of init.el
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
;; Configure font
(set-face-attribute 'default nil :height 120)

;;;; UI & UX configurations ;;;;

;; pulse when switch buffer
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))
(dolist (command '(scroll-up-command scroll-down-command
				     recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

;;; uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;;; increase or decrease text scale conveniently
(global-set-key (kbd "S-+") 'text-scale-increase)
(global-set-key (kbd "S--") 'text-scale-decrease)

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
  :bind (("s-b" . treemacs)))      ; super-b is vscode file browser
(use-package treemacs-projectile
  :after treemacs projectile)
(use-package treemacs-magit
  :after treemacs magit)

;; doom-themes are a collection of beatiful themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  ;; doom-one, doom-gruvbox, doom-zenburn looks very nice
  (load-theme 'doom-gruvbox t)
  ;; (doom-themes-visual-bell-config)
  (setq doom-themes-treemacs-theme "doom-gruvbox")
  (doom-themes-treemacs-config)
  ;; (doom-themes-org-config)
  )
;; Run M-x all-the-icons-install-fonts to install the fonts
(use-package all-the-icons)
;; This makes the virtual buffers darker than file buffers
(use-package solaire-mode
  :init (solaire-global-mode +1))
(use-package doom-modeline
  :init (doom-modeline-mode 1))
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;; Highlight-indent-guides: similar to sublime-text
(use-package highlight-indent-guides
  :hook ((prog-mode . highlight-indent-guides-mode)
	 (yaml-mode . highlight-indent-guides-mode))
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-suppress-auto-error t))

;;;; Markup Language configuration ;;;;

;; ;; org insert code: C-c C-,
;; (use-package org
;;   :init
;;   (setq org-log-done 'time
;; 	org-agenda-files (list "~/my-org-files")))

(use-package org-download
  :after org
  :bind (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

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
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . gfm-mode)
	 ("\\.markdown\\'" . markdown-mode)))

;;;; Editing enhancement ;;;;

(require 'keyfreq)
(setq keyfreq-file "~/.emacs.d/.keyfreq"
      keyfreq-file-lock "~/.emacs.d/.keyfreq.lock")
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; Expand region increases the selected region by semantic units
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;;; dealing with trailing whitespace
(defun trailing-whitespace-mode ()
  "Show whitespaces and unused lines."
  (progn
    (setq show-trailing-whitespace t)
    (setq indicate-unused-lines t)))
(add-hook 'prog-mode-hook 'trailing-whitespace-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-n") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "C-x C-p") 'tab-bar-switch-to-prev-tab)

;; Optional - provides snippet support.
(use-package yasnippet
  :init (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :hook ((prog-mode . yas-minor-mode)
	 (yaml-mode . yas-minor-mode)
	 (protobuf-mode . yas-minor-mode))
  :config (yas-reload-all))

;; docker-tramp allows to edit files in docker container using tramp
;; C-x C-f /docker:user@container_name:/path/to/file
(use-package docker-tramp :defer t)

;; Copy yaml files from https://github.com/rime/rime-pinyin-simp and https://github.com/rime/rime-wubi to ~/.emacs.d/rime
;; Create ~/.emacs.d/rime/default.custom.yaml with
;; patch:
;;  schema_list:
;;    - schema: wubi_pinyin
;; toggle rime input method with C-\
(use-package rime
  :custom
  (default-input-method "rime")
  ;; only set for macOS, download prebuilt librime from https://github.com/rime/librime/releases
  ;; extract to ~/.emacs.d/librime
  :when (memq window-system '(mac ns))
  :init
  (setq rime-librime-root "~/.emacs.d/librime/dist"))

;;; which-key: promp a window to display key when press incomplete key prefix
(use-package which-key
  :config
  (which-key-mode))

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
  :hook ((prog-mode . smartparens-strict-mode)
	 (markdown-mode . smartparens-strict-mode))
  :config
  (require 'smartparens-config))

;;; switch-window: switch other window with a number
(use-package switch-window
  :bind (("C-x o" . switch-window)))

;; chatgpt
(use-package mind-wave
  :straight (:type git :repo "https://github.com/manateelazycat/mind-wave"
		   :files ("*.el" "*.py"))
  :init (setq mind-wave-api-base "https://assistant.funcer.xyz/v1"))

(use-package chatgpt-shell
  :straight (:type git :repo "https://github.com/xenodium/chatgpt-shell"))

;; auto format using external programming language formatter
(use-package apheleia
  :config
  (apheleia-global-mode +1))

;;;; Term & Shell configurations ;;;;

;;; Read environment variable from shell config
(use-package exec-path-from-shell
  :when (memq window-system '(mac ns))
  :init (setq exec-path-from-shell-arguments nil)
  :config (dolist (var '("SSH_AUTH_SOCK"
		 "SSH_AGENT_PID"
		 "GPG_AGENT_INFO"
		 "LANG"
		 "LC_CTYPE"))
	    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package sticky-shell)

(use-package eat
  :straight (:type git :repo "https://codeberg.org/akib/emacs-eat.git"
		   :files ("*.el" ("term" "term/*.el") "*.texi"
			   "*.ti" ("terminfo/e" "terminfo/e/*")
			   ("terminfo/65" "terminfo/65/*")
			   ("integration" "integration/*")
			   (:exclude ".dir-locals.el" "*-tests.el"))))

;; (use-package aweshell
;;   :straight (:type git :repo "https://github.com/manateelazycat/aweshell"))

;; vterm: best terminal in emacs
;; C-c C-t: toggle vterm-copy-mode
;; better to use tmux inside vterm
(use-package vterm
  :commands (vterm)
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-buffer-name-string "vterm %s") ; Define prompt in bashrc/zshrc: PROMPT_COMMAND='echo -ne "\033]0;${PWD}\007"'
  (setq vterm-term-environment-variable "xterm"))

(use-package coterm)

;; Programing Language configurations
;; Lightweight LSP client
(use-package eglot)

(use-package consult-eglot)

(use-package copilot
  :straight (:type git :repo "https://github.com/zerolfx/copilot.el" :files ("dist" "*.el"))
  ;; :hook ((prog-mode . copilot-mode)
  ;; 	 (yaml-mode . copilot-mode))
  :bind (("C-c M-f" . copilot-complete)
         :map copilot-completion-map
         ("C-g" . 'copilot-clear-overlay)
         ("M-p" . 'copilot-previous-completion)
         ("M-n" . 'copilot-next-completion)
         ("<tab>" . 'copilot-accept-completion)
         ("M-f" . 'copilot-accept-completion-by-word)
         ("M-<return>" . 'copilot-accept-completion-by-line)))

;; eglot relies on flymake, flymake is good enough to use now
(use-package flymake
  :bind ((:map flymake-mode-map
	       ("M-n" . flymake-goto-next-error)
	       ("M-p" . flymake-goto-prev-error))))

(use-package go-mode
  :hook ((go-mode . eglot-ensure)
	     (go-mode . flymake-mode)
         (go-mode . (lambda ()
                      ;;set default tab width to 4 spaces
                      (setq-local tab-width 4)
                      ;;use space instead of TAB for indentation
                      (setq-local indent-tabs-mode nil)))))

(use-package rust-mode
  :hook ((rust-mode . eglot-ensure)
	 (rust-mode . flymake-mode)))

(use-package typescript-mode
  :init
  (setq typescript-indent-level 2))

(use-package devdocs)

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

;;;; Search & Completion Enhancement configurations ;;;;

;;; recent files
(use-package recentf
  :init
  (setq recentf-max-saved-items 500
	recentf-exclude '("^/tmp/"
			  "^/ssh:"
			  "^/docker:"
			  "\\.emacs\\.d/elpa/"
			  "\\.emacs\\.d/straight/"))
  (recentf-mode 1))

;; Text search with ripgrep
;; wgrep is enabled by this
;; C-c s: start magit like buffer
(use-package rg
  :config
  (rg-enable-default-bindings))

;; installed by git submodule
(require 'blink-search)
(setq blink-search-enable-icon nil)

(use-package ctrlf
  :init
  (ctrlf-mode +1))

(use-package vertico
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

;;; Projectile: Project navigation and management library for Emacs
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))
(use-package consult-projectile)

;; This is lightweight alternative to company-mode
;; M-SPC corfu-insert-separator
(use-package corfu
  :init
  (setq corfu-auto t
	corfu-quit-no-match 'separator)
  (global-corfu-mode))

(use-package popon ;; dependency for corfu-terminal
  :straight (:type git :repo "https://codeberg.org/akib/emacs-popon.git"))

(use-package corfu-terminal
  :straight (:repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :unless (display-graphic-p)
  :init (corfu-terminal-mode +1))

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
  :init (marginalia-mode))

(use-package embark
  :bind (("C-." . embark-act)         ;; pick some comfortable binding
	 ("C-;" . embark-dwim)        ;; good alternative: M-.
	 ("C-x ? B" . embark-bindings);; alternative for `describe-bindings'
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
  :bind (("C-c j" . avy-goto-word-or-subword-1)
	 ("M-g g" . avy-goto-line)))
(use-package ace-window
  :bind (("M-o" . ace-window)))

;; dumb-jump: jump to definition without language server, use rg for fuzzy searching
(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-selector 'ivy
	dumb-jump-prefer-searcher 'rg))

;;;; Git related ;;;;
(use-package magit
  :bind (("C-c g" . magit-status)
	 ("C-c b" . magit-blame-addition)
	 ("C-c l" . magit-log-buffer-file)))
(use-package diff-hl
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

;; Copy git repo link for the current file and line number
(use-package git-link)

;; edit-indirect is required for markdown mode to edit code blocks in indirect buffers using C-c '
(use-package edit-indirect :defer t)

(use-package protobuf-mode :defer t)

;; Config format
(use-package nix-mode)
(use-package yaml-mode)
(use-package toml-mode)
(use-package json-mode
  :init (setq js-indent-level 2)
  :config (setq json-reformat:indent-width 2))
(use-package dockerfile-mode)

(message "Emacs loaded in %s with %d garbage collections" (emacs-init-time) gcs-done)

;;; init.el ends here
