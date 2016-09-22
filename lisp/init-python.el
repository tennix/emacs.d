(require-package 'highlight-indentation)
(require-package 'elpy)

;;; install following packages through pip
;;; flake8, autopep8, yapf, importmagic, jedi

(add-hook 'python-mode-hook 'highlight-indentation-mode)
(add-hook 'python-mode-hook 'elpy-mode)

(provide 'init-python)
