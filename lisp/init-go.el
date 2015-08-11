;; key bindings:
;; C-c C-a        add imports
;; C-c C-r        remove unused imports
;; C-u C-c C-r    comment unused imports
;; C-c C-g        goto imports
;; C-c C-f        gofmt
;; C-c C-k        godoc
;; C-c C-d        godef-describe
;; C-c C-j        godef-jump
;; C-x <LEFT>     go back after godef-jump

;; set up
;; set GOPATH environment: `export GOPATH=$HOME/Projects/go`
;; go get code.google.com/p/go.tools/cmd/oracle

(require-package 'go-mode)
(require-package 'go-eldoc)
(require-package 'company-go)

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "GOPATH")

(add-to-list 'load-path "~/Projects/go/src/github.com/nsf/gocode/emacs-company")
(add-hook 'go-mode-hook (lambda ()
			  (setq tab-width 4)
			  (flycheck-mode)
			  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
			  (local-set-key (kbd "C-c C-g") 'go-goto-imports)
			  (local-set-key (kbd "C-c C-f") 'gofmt)
			  (local-set-key (kbd "C-c C-k") 'godoc)
			  (setq gofmt-command "goimports")
			  (setq godef-command "godef")
			  (set (make-local-variable 'company-backends) '(company-go))
			  (add-hook 'before-save-hook 'gofmt-before-save)
			  (go-eldoc-setup)))
(provide 'init-go)
