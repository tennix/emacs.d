;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(defconst *is-a-mac* (eq system-type 'darwin))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'init-elpa)
(require 'init-utils)
(require 'init-desktop)
(require 'init-window)
(require 'init-exec-path)

(require-package 'diminish)

(require 'init-themes)
(require 'init-uniquify)
(require 'init-flycheck)
(require 'init-flyspell)
(require 'init-recentf)
;; (require 'init-ido)
(require 'init-editing-utils)
(require 'init-yasnippet)
(require 'init-paredit)

(require 'init-markdown)
(require 'init-git)
(require 'init-python)
(require 'init-company)
(require 'init-go)
(require 'init-rust)
(require 'init-julia)
(require 'init-lisp)
(require 'init-ocaml)
(require 'init-haskell)
(require 'init-web)
(require 'init-org)
(require 'init-rfc)
(require 'init-projectile)
(require 'init-ivy)
