(require-package 'projectile)

(add-hook 'after-init-hook 'projectile-global-mode)

(setq-default projectile-mode-line
	      '(:eval (if (file-remote-p default-directory)
			  " Proj"
			(format " Proj[%s]" (projectile-project-name)))))

(provide 'init-projectile)
