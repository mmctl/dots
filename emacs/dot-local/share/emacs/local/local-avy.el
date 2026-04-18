;; -*- lexical-binding: t -*-
;; local-avy.el

(require 'avy)


;;; Actions
;;;###autoload
(defun avy-action-a-push-mark-no-activate (pt)
  "Executes `push-mark' at PT (selected with Avy), not activating mark,
leaving point."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (push-mark pt))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

;;;###autoload
(defun avy-action-a-push-mark-activate (pt)
  "Executes `push-mark' at PT (selected with Avy), activating mark, leaving
point."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (push-mark-command nil))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

;;;###autoload
(defun avy-action-a-kill-line-stay (pt)
  "Executes `avy-action-kill-stay', but kills till end of line current."
  (let ((avy-command 'avy-goto-line))
    (avy-action-kill-stay pt)))

;;;###autoload
(defun avy-action-a-kill-line-move (pt)
  "Executes `avy-action-kill-move', but kills till end of line current."
  (let ((avy-command 'avy-goto-line))
    (avy-action-kill-move pt)))

;;;###autoload
(defun avy-action-a-kill-whole-line-stay (pt)
  "Executes `kill-whole-line' at PT (selected with Avy), leaving point."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (kill-whole-line))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

;;;###autoload
(defun avy-action-a-kill-whole-line-move (pt)
  "Executes `kill-whole-line' at PT (selected with Avy), moving point."
  (goto-char pt)
  (kill-whole-line)
  (point))

;;;###autoload
(defun avy-action-a-copy-line (pt)
  "Executes `avy-action-copy' with PT, but copies to line end instead."
  (let ((avy-command 'avy-goto-line))
    (avy-action-copy pt)))

;;;###autoload
(defun avy-action-a-copy-whole-line (pt)
  "Copies line at PT (selected with Avy)."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (when-let* ((bnds (bounds-of-thing-at-point 'line)))
          (copy-region-as-kill (car bnds) (cdr bnds))))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

;;;###autoload
(defun avy-action-a-yank-line (pt)
  "Yanks to line end at PT (selected with Avy) to current point."
  (avy-action-a-copy-line pt)
  (save-excursion (yank))
  t)

;;;###autoload
(defun avy-action-a-yank-whole-line (pt)
  "Yanks line at PT (selected with Avy) to current point."
  (avy-action-a-copy-whole-line pt)
  (save-excursion (yank))
  t)

;;;###autoload
(defun avy-action-a-teleport-line (pt)
  "Executes `avy-action-teleport' with PT, but teleports upto line end instead."
  (let ((avy-command 'avy-goto-line))
    (avy-action-teleport pt)))

;;;###autoload
(defun avy-action-a-teleport-whole-line (pt)
  "Teleports line at PT (selected with Avy) to current point."
  (avy-action-a-kill-whole-line-stay pt)
  (save-excursion (yank))
  t)


;;; Region-based extension
(defvar an-avy-region-dispatch-alist
  '((?q . avy-action-region-kill)
    (?m . avy-action-region-mark)
    (?t . avy-action-region-teleport)
    (?y . avy-action-region-yank)
    (?w . avy-action-region-copy))
  "Analog of `avy-dispatch-alist' for region-based actions.")

(defun avy-action-region-copy (beg end)
  "Copies region defined by BEG and END, returning
to starting point afterward."
  (copy-region-as-kill beg end)
  (select-window
     (cdr (ring-ref avy-ring 0))))

(defun avy-action-region-kill (beg end)
  "Kills region defined by BEG and END, returning
to starting point afterward."
  (kill-region beg end)
  (select-window
     (cdr (ring-ref avy-ring 0))))

(defun avy-action-region-mark (beg end)
  "Marks region defined by BEG and END, pushing current mark
to mark ring."
  (push-mark beg nil t)
  (goto-char end))

(defun avy-action-region-teleport (beg end)
  "Teleports region defined by BEG and END
to current point."
  (avy-action-region-kill beg end)
  (save-excursion (yank)))

(defun avy-action-region-yank (beg end)
  "Yanks region defined by BEG and END
to current point."
  (avy-action-region-copy beg end)
  (save-excursion (yank)))

(defun an-avy-process-noaction (candidates &optional overlay-fn cleanup-fn)
  "Version of `avy-process' that does not perform any action directly,
but simply returns the entire chosen position (including window)."
  (let* ((overlay-fn (or overlay-fn (avy--style-fn avy-style)))
         (cleanup-fn (or cleanup-fn #'avy--remove-leading-chars))
         (candidates (if (and (consp (car candidates))
                              (windowp (cdar candidates)))
                         candidates
                       (mapcar (lambda (x) (cons x (selected-window)))
                               candidates)))
         (original-cands (copy-sequence candidates))
         (res (avy--process-1 candidates overlay-fn cleanup-fn)))
    (setq avy-last-candidates (copy-sequence candidates))
    (cond
     ((null res)
      (if (and (eq avy-style 'words) candidates)
          (an-avy-process-noaction original-cands overlay-fn cleanup-fn)
        (message "zero candidates")
        t))
     ((eq res 'restart)
      (an-avy-process-noaction original-cands overlay-fn cleanup-fn))
     ((eq res 'exit))
     ((eq res 'abort)
      nil)
     (t
      res))))

(defun an-avy-region-validate-position (position)
  "Checks whether POSITION is valid for use with
Avy region functions, meaning it is a list of the form
((BEG . END) WINDOW), where BEG and END are numbers
and WINDOW is a window."
  (or (and position
           (listp position)
           (numberp (caar position))
           (numberp (cdar position))
           (windowp (cdr position)))
      (user-error "Invalid position: %s" position)))

(defun an-avy-region-position-reader-regex (regex)
  "Reads region position based on user input, using REGEX to
search for candidates."
  (let ((position (an-avy-process-noaction (avy--regex-candidates regex))))
    (when (an-avy-region-validate-position position)
      position)))

(defun an-avy-region-position-reader-timer ()
  "Reads region position based on user input, using Avy's timer functionality to
search for candidates,  akin to `avy-goto-char-timer'."
  (let ((position (an-avy-process-noaction (avy--read-candidates))))
    (when (an-avy-region-validate-position position)
      position)))

(defun an-avy-region-reader-char-1 ()
  "Defines region based on user input, using
a single character to search for candidates
for each position, akin to `avy-goto-char-1'."
  (interactive)
  (let* ((begch (read-char (format-prompt "char (region begin)" "")))
         (begpos (an-avy-region-position-reader-regex (regexp-quote (string begch))))
         (endch (read-char (format-prompt "char (region end)" "")))
         (endpos (an-avy-region-position-reader-regex (regexp-quote (string endch)))))
    (cons begpos endpos)))

(defun an-avy-region-reader-timer ()
  "Defines region based on user input, sing Avy's timer functionality to
search for candidates for each position, akin to `avy-goto-char-timer'."
  (interactive)
  (let* ((begpos (an-avy-region-position-reader-timer))
         (endpos (an-avy-region-position-reader-timer)))
    (cons begpos endpos)))

(defun an-avy-region-command (cmd &optional arg)
  "Acts on the region defined through CMD. `avy-dispatch-alist' is set to
`an-avy-region-dispatch-alist', enabling the selectijon of the dispatch actions
contained therein. (You can select a dispatch action during the reading of both
region end-points, but the latter will overwrite the former). The window scope
is determined by `avy-all-windows' or `avy-all-windows-alt' when ARG is non-nil."
  (pcase-let* ((avy-all-windows (if arg avy-all-windows-alt avy-all-windows))
               (avy-dispatch-alist an-avy-region-dispatch-alist)
               (`(,begpos . ,endpos) (funcall cmd))
               (`((,begpnt . _) . ,begwin) begpos)
               (`((,endpnt . _) . ,endwin) endpos))
    (if (not (eq begwin endwin))
        (user-error "Selected region points are not in the same window")
      (funcall avy-pre-action begpos)
      (let ((endpnt (if (< endpnt (point-max)) (1+ endpnt) endpnt))
            (action (or avy-action #'avy-action-region-copy)))
        (funcall action begpnt endpnt)))))

;;;###autoload
(defun an-avy-region-char-1 (&optional arg)
  "Executes `an-avy-region-timer' with `an-avy-region-reader-char-1',
which see both. ARG is passed directly."
  (interactive)
  (avy-with an-avy-region-timer
    (an-avy-region-command #'an-avy-region-reader-char-1 arg)))

;;;###autoload
(defun an-avy-region-timer (&optional arg)
  "Executes `an-avy-region-timer' with `an-avy-region-reader-timer',
which see both. ARG is passed directly."
  (interactive)
  (avy-with an-avy-region-timer
    (an-avy-region-command #'an-avy-region-reader-timer arg)))


(provide 'local-avy)

;;; local-avy.el ends here
