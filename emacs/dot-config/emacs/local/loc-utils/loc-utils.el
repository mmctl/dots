;; -*- lexical-binding: t -*-
;; loc-utils.el

;; Killing
(defun backward-kill-line (&optional arg)
  (interactive "P")
  (if arg
      (kill-line (- arg))
    (if (bolp)
        (delete-backward-char 1 t)
      (kill-line 0))))

;; Deleting

;; Quitting
(defun save-buffers-kill-terminal-silent ()
  "Execute save-buffers-kill-terminal, automatically saving all buffers without asking."
  (interactive)
  (save-buffers-kill-terminal t))

(defun save-buffers-kill-emacs-silent ()
  "Execute save-buffers-kill-emacs, automatically saving all buffers without asking."
  (interactive)
  (save-buffers-kill-emacs t))

(defun save-buffers-restart-emacs ()
  "Execute save-buffers-kill-emacs, restarting Emacs afterward."
  (interactive)
  (save-buffers-kill-emacs nil t))

(defun save-buffers-restart-emacs-silent ()
  "Execute save-buffers-restart-emacs, automatically saving all buffers without asking."
  (interactive)
  (save-buffers-kill-emacs t t))

;;; Hooks
(defun indent-on-insertion-closer ()
  "Indent when last input was }, ), ], or \".
Meant for `post-self-insert-hook`."
  (when (memq last-command-event '(?\} ?\) ?\] ?\"))
    (save-excursion
      (indent-according-to-mode))))


(provide 'loc-utils)

;;; loc-utils.el ends here
