(require-package 'ivy)
(require-package 'counsel)
(require-package 'swiper)
(require-package 'smex)

(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))

(ivy-mode 1)
(setq-default ivy-use-virtual-buffers t
	     ivy-count-format ""
	     projectile-completion-system 'ivy
	     ivy-initial-inputs-alist
	     '((counsel-M-x . "^")
	       (man . "^")
	       (woman . "^")))

(define-key ivy-minibuffer-map (kbd "c-j") #'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

(add-hook 'after-init-hook
	  (lambda ()
	    (when (bound-and-true-p ido-ubiquitous-mode)
	      (ido-ubiquitous-mode -1)
	      (ido-mode -1))
	    (ivy-mode 1)))

(setq-default counsel-mode-override-describe-bindings t)
(add-hook 'after-init-hook 'counsel-mode)

(define-key ivy-mode-map (kbd "C-s") 'swiper)

(after-load 'counsel (diminish 'counsel-mode))
(after-load 'ivy (diminish 'ivy-mode))

(provide 'init-ivy)
