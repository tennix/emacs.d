(require-package 'rust-mode)
(require-package 'flycheck-rust)

(setq racer-rust-src-path "~/Projects/rust/rust/src/")
(setq racer-cmd "~/Projects/rust/racer/target/release/racer")
(add-to-list 'load-path "~/Projects/rust/racer/editors/emacs")
(eval-after-load "rust-mode" '(require 'racer))

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(add-hook 'rust-mode-hook
	  '(lambda ()
	     (racer-activate)
	     (flycheck-mode)
	     (local-set-key (kbd "M-.") #'racer-find-definition)
	     (local-set-key (kbd "TAB") #'racer-complete-or-indent)))

(provide 'init-rust)
