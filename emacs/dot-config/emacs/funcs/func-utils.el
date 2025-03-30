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


(provide 'func-utils)

;;; func-utils.el ends here
