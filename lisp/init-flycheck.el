(require-package 'flycheck)

(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
	  flycheck-idle-change-delay 0.8)

(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
(add-hook 'prog-mode-hook 'flycheck-mode)


(provide 'init-flycheck)
