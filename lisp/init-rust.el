(require-package 'rust-mode)
(require-package 'racer)
(require-package 'flycheck-rust)

(setq racer-rust-src-path "~/Projects/rust/rust/src/")
(setq racer-cmd "~/Projects/rust/racer/target/release/racer")

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
