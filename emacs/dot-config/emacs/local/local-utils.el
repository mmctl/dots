;; -*- lexical-binding: t -*-
;; local-utils.el

;;; Movement
(defun move-beginning-of-line-or-indentation (&optional arg)
  "Moves point to indentation or, if point is already there, to beginning of line.
With ARG, moves to indentation ARG lines forward."
  (interactive "^P")
  (if arg
      (forward-to-indentation (prefix-numeric-value arg))
    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= (point) orig-point)
        (move-beginning-of-line nil)))))

(defun move-end-of-line-or-whitespace (&optional arg)
  "Moves point to beginning of whitespace at the end of the line or,
if point is already there and `show-trailing-whitespace' is non-nil,
to (actual) end of line. With ARG, moves to end of line
ARG - 1 lines forward."
  (interactive "^P")
  (let ((orig-point (point)))
    (move-end-of-line arg)
    (re-search-backward "[^[:blank:]]" (line-beginning-position) t)
    (forward-char)
    (when (and (null arg) show-trailing-whitespace (= (point) orig-point))
      (move-end-of-line nil))))

(defun move-it-region-vertically (start end &optional arg)
  "Moves region defined by START and END lines up or down, depending on ARG
(defaults to 1 line down). With ARG, moves |ARG| lines down (up if ARG is
negative)."
  (let ((arg (or arg 1))
        (lnstart (line-number-at-pos start))
        (lnend (line-number-at-pos end)))
    (when (and (< arg 0) (<= lnstart 1))
      (user-error "Start of region on first line of buffer, cannot move up"))
    (when (and (< 0 arg) (<= (1- (line-number-at-pos (point-max))) lnend))
      (user-error "End of region on last line of buffer, cannot move down"))
    (let* ((ltmp (< (mark) (point)))
           (startcol (save-excursion
                       (goto-char start)
                       (current-column)))
           (content (delete-and-extract-region start end)))
      (forward-line arg)
      (move-to-column startcol)
      (save-excursion
        (insert content)
        (set-mark (point)))
      (when ltmp
        (exchange-point-and-mark))
      (setq deactivate-mark nil))))

(defun move-it-region-horizontally (start end &optional arg)
  "Moves region defined by START and END characters left or
right, depending on ARG (defaults to 1 character right).
With ARG, moves |ARG| characters right (left if ARG is negative)."
  (let ((arg (or arg 1)))
    (when (and (< arg 0) (= start (point-min)))
      (user-error "Start of region at beginning of buffer, cannot move left"))
    (when (and (< 0 arg) (= end (point-max)))
      (user-error "End of region at end of buffer, cannot move right"))
    (let* ((ltmp (= (mark) start))
           (content (delete-and-extract-region start end)))
      (forward-char arg)
      (save-excursion
        (insert content)
        (set-mark (point)))
      (when ltmp
        (exchange-point-and-mark))
      (setq deactivate-mark nil))))

(defun move-it-wholeline-region-vertically (start end &optional arg)
  "Moves whole lines in region defined by START and END lines up or down,
depending on ARG (defaults to 1 line down). With ARG, moves |ARG| lines down (up
if ARG is negative)."
  (let ((arg (or arg 1))
        (down (< 0 arg))
        (lnstart (line-number-at-pos start))
        (lnend (line-number-at-pos end)))
    (when (and (< arg 0) (<= lnstart 1))
      (user-error "Start of region on first line of buffer, cannot move up"))
    (when (and down (<= (1- (line-number-at-pos (point-max))) lnend))
      (user-error "End of region on last line of buffer, cannot move down"))
    (let* ((ltmp (= (mark) start))
           (origstart start)
           (origend end)
           (start (save-excursion
                    (goto-char origstart)
                    (line-beginning-position)))
           (end (save-excursion
                  (goto-char origend)
                  (end-of-line)
                  (if (looking-at-p "\n") (1+ (point)) (point))))
           (rtob (- origstart start))
           (rtoe (- end origend))
           (content (delete-and-extract-region start end)))
      (forward-line arg)
      (save-excursion
        (insert content)
        (set-mark (- (point) rtoe)))
      (forward-char rtob)
      (when ltmp
        (exchange-point-and-mark))
      (setq deactivate-mark nil))))

(defun move-it-wholeline-region-horizontally (start end &optional arg)
  "Moves whole lines in region defined by START and END characters left or
right, depending on ARG (defaults to 1 character right).
With ARG, moves |ARG| characters right (left if ARG is negative).
This is mostly equivalent to `indent-rigidly', which see, but includes
all lines with content in region, not only those that start in region."
  (let* ((start (save-excursion
                  (goto-char start)
                  (line-beginning-position))))
    (indent-rigidly start end (or arg 1))
    (setq deactivate-mark nil)))

(defun move-it-region-up (start end &optional arg)
  "Moves region defined by START and END
one line up. With ARG, moves |ARG|
lines up instead (down if ARG is negative)."
  (interactive "r\np")
  (if (= (line-number-at-pos start) (line-number-at-pos end))
      (move-it-region-vertically start end (- arg))
    (move-it-wholeline-region-vertically start end (- arg))))

(defun move-it-region-down (start end &optional arg)
  "Moves region defined by START and END
one line down. With ARG, moves |ARG|
lines down (up if ARG is negative)."
  (interactive "r\np")
  (if (= (line-number-at-pos start) (line-number-at-pos end))
      (move-it-region-vertically start end arg)
    (move-it-wholeline-region-vertically start end arg)))

(defun move-it-region-left (start end &optional arg)
  "Moves region defined by START and END
one character left. With ARG, moves |ARG|
lines up instead (down if ARG is negative)."
  (interactive "r\np")
  (if (= (line-number-at-pos start) (line-number-at-pos end))
      (move-it-region-horizontally start end (- arg))
    (move-it-wholeline-region-horizontally start end (- arg))))

(defun move-it-region-right (start end &optional arg)
  "Moves region defined by START and END
one character right. With ARG, moves |ARG|
lines down (up if ARG is negative)."
  (interactive "r\np")
  (if (= (line-number-at-pos start) (line-number-at-pos end))
      (move-it-region-horizontally start end arg)
    (move-it-wholeline-region-horizontally start end arg)))

(defun move-it-line-vertically (&optional arg)
  "Moves line at point up or down, depending on ARG
(defaults to one line down). With ARG, moves |ARG|
lines down (up if ARG is negative)."
  (let ((arg (or arg 1))
        (ln (line-number-at-pos)))
    (when (and (< arg 0) (<= ln 1))
      (user-error "On first line of buffer, cannot move up"))
    (when (and (< 0 arg) (<= (1- (line-number-at-pos (point-max))) ln))
      (user-error "On last line of buffer, cannot move down"))
    (pcase-let* ((col (current-column))
                 (`(,beg . ,end) (bounds-of-thing-at-point 'line))
                 (line (delete-and-extract-region beg end)))
      (forward-line arg)
      (save-excursion (insert line))
      (move-to-column col))))

(defun move-it-line-horizontally (&optional arg)
  "Moves line at point left or right, depending on ARG
(defaults to one character right). With ARG, moves |ARG|
characters right (left if ARG is negative).
This is equivalent to performing `indent-rigidly' on the current line."
  (pcase-let ((`(,beg . ,end) (bounds-of-thing-at-point 'line)))
    (indent-rigidly beg end (or arg 1))))

(defun move-it-line-up (&optional arg)
  "Moves line at point ARG lines up (defaults to 1)."
  (interactive "p")
  (move-it-line-vertically (- arg)))

(defun move-it-line-down (&optional arg)
  "Moves line at point ARG lines down (defaults to 1)."
  (interactive "p")
  (move-it-line-vertically arg))

(defun move-it-line-left (&optional arg)
  "Moves line at point ARG characters left (defaults to 1)."
  (interactive "p")
  (move-it-line-horizontally (- arg)))

(defun move-it-line-right (&optional arg)
  "Moves line at point ARG characters right (defaults to 1)."
  (interactive "p")
  (move-it-line-horizontally arg))

(defun move-it-up (&optional arg)
  "If region is active, moves region or whole lines in region |ARG| lines up
(down if ARG is negative). If region is not active, moves line at point
|ARG| lines up (down if ARG is negative). Defaults to 1 line up."
  (interactive "p")
  (if (use-region-p)
      (move-it-region-up (region-beginning) (region-end) arg)
    (move-it-line-up arg)))

(defun move-it-down (&optional arg)
  "If region is active, moves region or whole lines in region |ARG| lines down
(up if ARG is negative). If region is not active, moves line at point |ARG|
lines down (up if ARG is negative). Defaults to 1 line down."
  (interactive "p")
  (if (use-region-p)
      (move-it-region-down (region-beginning) (region-end) arg)
    (move-it-line-down arg)))

(defun move-it-left (&optional arg)
  "If region is active, moves region or whole  lines in region |ARG| characters
left (right if ARG is negative). If region is not active, moves line
at point |ARG| characters left (right if ARG is negative).
Defaults to 1 character left."
  (interactive "p")
  (if (use-region-p)
      (move-it-region-left (region-beginning) (region-end) arg)
    (move-it-line-left arg)))

(defun move-it-right (&optional arg)
  "If region is active, moves region or whole lines in region |ARG| characters
right (left if ARG is negative). If region is not active, moves line
a point |ARG| characters right (left if ARG is negative).
Defaults to 1 character right."
  (interactive "p")
  (if (use-region-p)
      (move-it-region-right (region-beginning) (region-end) arg)
    (move-it-line-right arg)))

(defun push-mark-no-activate (&optional location)
  "Pushes LOCATION (defaults to `point') to `mark-ring' without
activating it."
  (interactive)
  (push-mark (or location (point))))

(defun exchange-point-and-mark-invert (&optional arg)
  "Identical to `exchange-point-and-mark' but inverts the prefix argument,
meaning that (with Transient Mark mode on) it defaults to deactivating the mark
if it is active and not reactivating mark."
  (interactive "P")
  (exchange-point-and-mark (null arg)))

;;; Duplication
(defun duplicate-line-or-lines-in-region (&optional arg)
  "Duplicates current line or, when region is active, lines in current region.
With ARG, duplicates |ARG| times forward (ARG > 0) or backward (ARG < 0),
putting point at same relative position in final duplication."
  (interactive "p")
  (pcase-let* ((neg (< arg 0))
               (`(,beg . ,end) (if (use-region-p)
                                   (cons (save-excursion
                                           (goto-char (region-beginning))
                                           (line-beginning-position))
                                         (save-excursion
                                           (goto-char (region-end))
                                           (line-end-position)))
                                 (cons (line-beginning-position)
                                       (line-end-position))))
               (relpnt (- (point) (if neg beg end)))
               (content (buffer-substring beg end)))
    (goto-char (if neg beg end))
    (dotimes (_ (abs arg))
      (if neg
          (save-excursion
            (insert content)
            (newline))
        (newline)
        (insert content)))
    (forward-char relpnt)))


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
(defun join-line-stay (&optional arg)
  "Calls `join-line' |ARG| times, which see, but keeps point
in same relative position. If ARG is negative, calls
`join-line' with a prefix argument."
  (interactive "^p")
  (save-excursion
    (dotimes (_ (abs arg))
      (join-line (< arg 0)))))

(defun join-line-forward (&optional arg)
  "Joins current line to the following |ARG| lines and
fix up whitespace at join. If ARG is negative, joins
with the preceding |ARG| lines instead. Simply calls `join-line'
internally, which see."
  (interactive "^p")
  (dotimes (_ (abs arg))
      (join-line (<= 0 arg))))

(defun join-line-forward-stay (&optional arg)
  "Calls `join-line-forward' with ARG, which see, but keeps point
in same relative position."
  (interactive "^p")
  (save-excursion
    (join-line-forward arg)))


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
  "Yanks (in place) line at point.
With ARG, moves |ARG| lines forward (ARG > 0) or backward (ARG < 0),
then copies that line. Does not move point."
  (interactive "P")
  (kill-ring-save-line arg)
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

(defun kill-whole-line-back-to-indentation (&optional arg)
  "Kills whole line using `kill-whole-line' and moves back
to indentation using `back-to-indentation'. Passes ARG
directly to `kill-whole-line'"
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))

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
    (delete-region (line-beginning-position) (line-beginning-position (+ arg 1)))))


;;; Files and directories
(defun find-file-as-root (filename &optional arg)
  "Find FILENAME as root using `find-file', taking remote
connections into account. With ARG, use `find-alternate-file' instead."
  (interactive
   (list (expand-file-name
          (read-file-name (format-prompt "Find file as root"
                                         buffer-file-name)
                          nil
                          buffer-file-name
                          'confirm))
         current-prefix-arg))
  (let* ((remote-method (file-remote-p default-directory 'method))
         (remote-host (file-remote-p default-directory 'host))
         (remote-localname (file-remote-p filename 'localname))
         (fileid (format "/%s:root@%s:%s"
                         (or remote-method "sudo")
                         (or remote-host "localhost")
                         (or remote-localname filename))))
    (if arg
        (find-alternate-file fileid)
      (find-file fileid))))

(defun reopen-file-as-root ()
  "Reopen file visited by current buffer
as root using `find-alternate-file'."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer not visiting a file"))
  (find-file-as-root buffer-file-name t))

(defun dired-as-root (dirname)
  "Find DIRNAME as root using `dired', taking remote
connections into account."
  (interactive
   (list (expand-file-name
          (read-directory-name (format-prompt "Find directory as root"
                                              default-directory)
                               nil
                               default-directory
                               'confirm))))
   (let* ((remote-method (file-remote-p default-directory 'method))
          (remote-host (file-remote-p default-directory 'host))
          (remote-localname (file-remote-p dirname 'localname))
          (dirid (format "/%s:root@%s:%s"
                         (or remote-method "sudo")
                         (or remote-host "localhost")
                         (or remote-localname dirname))))
     (dired dirid)))

(defun dired-default-directory-as-root ()
  "Open directory (specifically, `default-directory')
of current buffer as root using `dired'."
  (interactive)
  (dired-as-root (expand-file-name default-directory)))


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
