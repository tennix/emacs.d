(require-package 'yasnippet)

(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-reload-all)
(yas-global-mode)

(provide 'init-yasnippet)
