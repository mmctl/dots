;; -*- lexical-binding: t -*-
;; local-utils.el

;;; Movement
(defun move-beginning-of-line-indentation (&optional arg)
  (interactive "^P")
  (let ((orig-point (point)))
    (forward-to-indentation (or arg 0))
    (when (= (point) orig-point)
      (move-beginning-of-line nil))))

(defun move-end-of-line-whitespace (&optional arg)
  (interactive "^P")
  (let ((orig-point (point)))
    (move-end-of-line arg)
    (re-search-backward "[^[:blank:]]" (line-beginning-position) t)
    (forward-char)
    (when (and show-trailing-whitespace (= (point) orig-point))
      (move-end-of-line nil))))

;;; Duplication
(defun duplicate-line-or-region (&optional arg)
  (interactive "p")
  (let* ((pntbeg (= (point) (region-beginning)))
         (rgn (if (use-region-p)
                  (buffer-substring-no-properties
                   (save-excursion
                     (goto-char (region-beginning))
                     (line-beginning-position (when (and pntbeg (bolp)) 2)))
                   (save-excursion
                     (goto-char (region-end))
                     (line-end-position (when (and (not pntbeg) (bolp) 0)))))
                (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position)))))
    (dotimes (i arg)
      (end-of-line)
      (newline)
      (insert rgn))))


;;; Transposing/Exchanging
(defun exchange-word (arg)
  "Exchanges word at point or, if there is none,
next word after point, with following (ARG > 0) or
preceding (ARG < 0) word |ARG| times."
  (interactive "p")
  (unless (looking-at-p "\\>")
    (forward-word 1))
  (transpose-words arg))

(defun exchange-word-backward (arg)
  "Calls `exchange-word', which see, with ARG negated."
  (interactive "p")
  (exchange-word (- arg)))


;;; Joining
(defun join-line-stay ()
  "Calls `join-line', which see, but keeps point
in same relative position."
  (interactive)
  (save-excursion
    (join-line)))

(defun join-line-forward ()
  "Joins current line to the following line and
fix up whitespace at join. Simply calls `join-line'
with a prefix argument internally, which see."
  (interactive)
  (join-line t))

(defun join-line-forward-stay ()
  "Calls `join-line-forward', which see, but keeps point
in same relative position."
  (interactive)
  (save-excursion
    (join-line-forward)))


;;; Copying
(defun kill-ring-save-line (&optional arg)
  "Copies line at point. Calling this once copies the line
from indentation up to (but not including) the trailing whitespace
or newline. Calling this twice (or more) in a row copies
the whole line, including indentation, trailing whitespace, and newline.
With ARG, moves |ARG| lines forward (ARG > 0) or backward (ARG < 0),
then performs its action for that line. Leaves point as is."
  (interactive "P")
  (save-excursion
    (when arg
      (forward-line (prefix-numeric-value arg)))
    (let* ((rep (eq last-command this-command))
           (bnds (if rep
                     (bounds-of-thing-at-point 'line)
                   (cons (progn (back-to-indentation) (point))
                         (progn (end-of-line)
                                (re-search-backward "[^[:blank:]]" (line-beginning-position) t)
                                (1+ (point)))))))
      (when bnds
        (kill-ring-save (car bnds) (cdr bnds))
        (message "Copied %s%s"
                 (if rep "entire line (content + whitespace)" "line (or field) content")
                 (if arg (format " at %s" arg) ""))))))


;;; Yanking
(defun yank-whole-line (&optional arg)
  "Yanks (in place) whole line at point.
With ARG, moves |ARG| lines forward (ARG > 0) or backward (ARG < 0),
then copies that line. Does not move point."
  (interactive "P")
  (kill-ring-save-whole-line arg)
  (yank))


;;; Killing
(defun kill-whole-word (&optional arg)
  "Kills word at point or, if no word at point, next word.
If there is also no next word, does nothing. With ARG, moves |ARG| words forward
(ARG > 0) or backward (ARG < 0), then kills that word. Does not move
point (beyond the displacement that may happen from killing words)."
  (interactive "p")
  (save-excursion
    (forward-word arg)
    (when-let* ((bnds (bounds-of-thing-at-point 'word)))
      (kill-region (car bnds) (cdr bnds)))))

(defun kill-whole-symbol (&optional arg)
  "Kills symbol at point or, if no symbol at point, next symbol.
If there is also no next symbol, does nothing.
With ARG, kills word |ARG| words forward (ARG > 0)
or backward (ARG < 0). Does not move point (beyond
the displacement that may happen from killing words)."
  (interactive "p")
  (save-excursion
    (forward-symbol arg)
    (when-let* ((bnds (bounds-of-thing-at-point 'symbol)))
      (kill-region (car bnds) (cdr bnds)))))

(defun backward-kill-line (&optional arg)
  "Kills from point to beginning of line.
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


;;; Deleting
(defun forward-delete-line (&optional arg)
  "Deletes from point to end of line.
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
  "Deletes from point to beginning of line.
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
  "Deletes whole line (i.e., including terminating newline)
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


;;; Quitting
(defun save-buffers-kill-terminal-silent ()
  "Executes `save-buffers-kill-terminal', which see,
automatically saving all buffers without asking."
  (interactive)
  (save-buffers-kill-terminal t))

(defun save-buffers-kill-emacs-silent ()
  "Executes `save-buffers-kill-emacs', which see,
automatically saving all buffers without asking."
  (interactive)
  (save-buffers-kill-emacs t))

(defun save-buffers-restart-emacs ()
  "Executes `save-buffers-kill-emacs', which see,
restarting Emacs afterward."
  (interactive)
  (save-buffers-kill-emacs nil t))

(defun save-buffers-restart-emacs-silent ()
  "Execute `save-buffers-restart-emacs', which see,
automatically saving all buffers without asking."
  (interactive)
  (save-buffers-kill-emacs t t))

(provide 'local-utils)

;;; local-utils.el ends here
