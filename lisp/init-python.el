(require-package 'elpy)

;;; install following packages through pip
;;; flake8, autopep8, yapf, importmagic, jedi

(add-hook 'python-mode-hook 'elpy-mode)

(provide 'init-python)
