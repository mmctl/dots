;; -*- lexical-binding: t -*-
;; loc-utils.el

;; Joining
(defun join-line-forward ()
  "Join current line to the following line and
fix up whitespace at join. Simply calls `join-line'
with a prefix argument internally, which see."
  (interactive)
  (join-line t))

;; Killing
(defun backward-kill-line (&optional arg)
  "Kill from point to beginning of line.
If point is at beginning of line, then
kill the preceding newline character and,
if `show-trailing-whitespace' is nil,
delete the trailing whitespace of the preceding line as well.
If ARG is non-nil, simply call `kill-line' with the
corresponding negated numeric value."
  (interactive "P")
  (if arg
      (kill-line (- (prefix-numeric-value arg)))
    (if (bolp)
        (progn
          (delete-char (- 1) t)
          (unless show-trailing-whitespace
            (delete-horizontal-space t)))
      (kill-line 0))))


;; Deleting
(defun forward-delete-line (&optional arg)
  "Delete from point to end of line.
If point is at end of line, then delete
the succeeding newline character.
If ARG is non-nil, delete from point to end
of ARG-th line after current line."
  (interactive "P")
  (if arg
      (delete-region (point) (pos-eol (prefix-numeric-value (+ arg 1))))
    (if (eolp)
        (delete-char 1)
      (delete-region (point) (pos-eol)))))

(defun backward-delete-line (&optional arg)
  "Delete from point to beginning of line.
If point is at beginning of line, then
delete the preceding newline character and,
if `show-trailing-whitespace' is nil,
delete the trailing whitespace of the preceding line as well.
If ARG is non-nil, delete from point to beginning
of ARG-th line before current line."
  (interactive "P")
  (if arg
      (delete-region (pos-bol (prefix-numeric-value (+ arg 1))) (point))
    (if (bolp)
        (progn
          (delete-char (- 1))
          (unless show-trailing-whitespace
            (delete-horizontal-space t)))
      (delete-region (pos-bol) (point)))))

(defun delete-whole-line-or-region (arg)
  "Delete whole line (i.e., including terminating newline)
or region (if active).
If no region is active and ARG <= 0, then
delete previous -ARG whole lines *before* current one.
If no region is active and ARG > 0, then
delete next ARG whole lines *including* current one.
Note that this means that, if ARG = 0, this function does
nothing. In exchange, the behavior is a bit more intuitive."
  (interactive "p")
  (if (use-region-p)
      (call-interactively #'delete-region)
    (delete-region (pos-bol) (pos-bol (+ arg 1)))))

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

(provide 'loc-utils)

;;; loc-utils.el ends here
