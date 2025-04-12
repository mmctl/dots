;; -*- lexical-binding: t -*-
;; func-utils.el
;; Quitting
(defun save-buffers-kill-terminal-silent ()
  "Execute save-buffers-kill-terminal, automatically saving all buffers without asking"
  (interactive)
  (save-buffers-kill-terminal t))

(defun save-buffers-kill-emacs-silent ()
  "Execute save-buffers-kill-emacs, automatically saving all buffers without asking"
  (interactive)
  (save-buffers-kill-emacs t))

(defun save-buffers-restart-emacs ()
  "Execute save-buffers-kill-emacs, restarting Emacs afterward"
  (interactive)
  (save-buffers-kill-emacs nil t))

(defun save-buffers-restart-emacs-silent ()
  "Execute save-buffers-restart-emacs, automatically saving all buffers without asking"
  (interactive)
  (save-buffers-kill-emacs t t))


;; Indentation
;; Basic rigid indentation
(defun a-basic-indent (arg)
  "Indent (or de-indent) region by (prefix) arg *`tab-width`, or otherwise insert tab at position."
  (interactive "P")
  (if (use-region-p)
      (indent-rigidly (region-beginning) (region-end) (* arg tab-width))
    (insert-tab arg)))


(provide 'func-utils)

;;; func-utils.el ends here
