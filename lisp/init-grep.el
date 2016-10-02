(require-package 'wgrep)
(require-package 'ag)
(require-package 'wgrep-ag)

;;; in ag search result buffer
;;; [p] to previous result
;;; [n] to next result
;;; [k] to kill buffer
;;; [RET] to open file

;;; [C-c C-p] to make buffer editable
;;; [C-c C-e] to apply changes to buffers
;;; [C-c C-d] to mark line for deletion
;;; [C-c C-k] to discard all changes and exit

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(setq ag-highlight-search t)

;;; automatically save all buffers when finish wgrep edit.
;;; if set to nil, `wgrep-save-all-buffers` must be called to save changes to file
;; (setq wgrep-auto-save-buffer t)

(provide 'init-grep)
