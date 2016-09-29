(require-package 'projectile)

(add-hook 'after-init-hook 'projectile-global-mode)

(setq-default project-mode-line
	      '(:eval (if (file-remote-p default-directory)
			  " Pr"
			(format " Pr[%s]" (projectile-project-name)))))

(provide 'init-projectile)
