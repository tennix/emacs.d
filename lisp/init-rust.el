(require-package 'rust-mode)
(require-package 'racer)
(require-package 'flycheck-rust)

;;; git clone --depth=1 https://github.com/rust-lang/rust ~/.rust
(setq racer-rust-src-path "~/.rust/src/")
;;; cargo install racer
(setq racer-cmd "~/.cargo/bin/racer")

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(add-hook 'rust-mode-hook
	  '(lambda ()
	     (racer-mode)
	     (flycheck-mode)
	     (eldoc-mode)
	     (company-mode)
	     (local-set-key (kbd "M-.") #'racer-find-definition)
	     (local-set-key (kbd "TAB") #'company-indent-or-complete-common)))
(setq company-tooltip-align-annotations t)

(provide 'init-rust)
