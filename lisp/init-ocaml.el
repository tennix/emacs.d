(require-package 'tuareg)
(require-package 'merlin)
(require-package 'utop)
(require-package 'ocp-indent)

(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)
(add-hook 'caml-mode-hook 'merlin-mode t)
(setq merlin-command "~/.opam/system/bin/ocamlmerlin")
(with-eval-after-load 'company
  (add-to-list 'company-backends 'merlin-company-backend))
(add-hook 'merlin-mode-hook 'company-mode)

(setq utop-command "opam config exec -- utop -emacs")
(provide 'init-ocaml)
