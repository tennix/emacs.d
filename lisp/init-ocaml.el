(require-package 'tuareg)
(require-package 'merlin)

(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'caml-mode-hook 'merlin-mode t)
(setq merlin-command 'opam)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'merlin-company-backend))
(add-hook 'merlin-mode-hook 'company-mode)

(provide 'init-ocaml)
