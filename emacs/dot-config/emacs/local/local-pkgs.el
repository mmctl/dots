;; -*- lexical-binding: t -*-
;; local-pkgs.el

;;; Avy
(require 'avy)

;; Actions (additional)
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
  "Executes `kill-whole-line' at PT (selected with Avy), moving point."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (kill-whole-line))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

;;;###autoload
(defun avy-action-a-kill-whole-line-move (pt)
  "Executes `kill-whole-line' at PT (selected with Avy), leaving point."
  (unwind-protect
      (progn
        (goto-char pt)
        (kill-whole-line))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

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

;;;###autoload
(defun avy-action-an-embark-select (pt)
  "Executes `embark-select' at PT (selected with Avy)."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-select))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

;;;###autoload
(defun avy-action-an-embark-act (pt)
  "Executes `embark-act' at PT (selected with Avy)."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

;;;###autoload
(defun avy-action-an-embark-dwim (pt)
  "Executes `embark-dwim' at PT (selected with Avy)."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-dwim))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

;; Region-based extension
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

;;;###autoload
(defun an-avy-region-timer (&optional arg)
  "Acts on the region defined by the positions determined through
Avy's timer-based reading functionality. `avy-dispatch-alist' is set
to `an-avy-region-dispatch-alist', enabling the selection of the
dispatch actions contained therein. (You can select a dispatch action
during the reading of both region end-points, but the latter will
overwrite the former). The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil."
  (interactive)
  (avy-with an-avy-region-timer
    (when-let* ((avy-all-windows (if arg avy-all-windows-alt avy-all-windows))
                (avy-dispatch-alist an-avy-region-dispatch-alist)
                (begpos (an-avy-process-noaction (avy--read-candidates)))
                ((if (or (null begpos)
                         (not (listp begpos))
                         (not (numberp (caar begpos)))
                         (not (windowp (cdr begpos))))
                     (user-error "Failed to select beginning of region")
                   t))
                (endpos (an-avy-process-noaction (avy--read-candidates)))
                ((if (or (null endpos)
                         (not (listp endpos))
                         (not (numberp (caar endpos)))
                         (not (windowp (cdr endpos))))
                     (user-error "Failed to select end of region")
                   t)))
      (if (not (eq (cdr begpos) (cdr endpos)))
          (user-error "Selected region points are not in the same window")
        (funcall avy-pre-action begpos)
        (let* ((begpnt (caar begpos))
               (endpnt (caar endpos))
               (endpnt (if (< endpnt (point-max)) (1+ endpnt) endpnt))
               (action (or avy-action #'avy-action-region-copy)))
          (funcall action begpnt endpnt))))))

;;;###autoload
(defun an-avy-region-char (&optional arg)
  "As `an-avy-region-timer', but uses the non-timer (single-char) reading
functionality for determining the region beginning/end."
  (interactive)
  (avy-with an-avy-region-timer
    (when-let* ((avy-all-windows (if arg avy-all-windows-alt avy-all-windows))
                (avy-dispatch-alist an-avy-region-dispatch-alist)
                (begch (read-char (format-prompt "char (region begin)" "")))
                (begpos (an-avy-process-noaction (avy--regex-candidates (regexp-quote (string begch)))))
                ((if (or (null begpos)
                         (not (listp begpos))
                         (not (numberp (caar begpos)))
                         (not (windowp (cdr begpos))))
                     (user-error "Failed to select begin of region")
                   t))
                (endch (read-char (format-prompt "char (region end)" "")))
                (endpos (an-avy-process-noaction (avy--regex-candidates (regexp-quote (string endch)))))
                ((if (or (null endpos)
                         (not (listp endpos))
                         (not (numberp (caar endpos)))
                         (not (windowp (cdr endpos))))
                     (user-error "Failed to select end of region")
                   t)))
      (if (not (eq (cdr begpos) (cdr endpos)))
          (user-error "Selected region points are not in the same window")
        (funcall avy-pre-action begpos)
        (let* ((begpnt (caar begpos))
               (endpnt (caar endpos))
               (endpnt (if (< endpnt (point-max)) (1+ endpnt) endpnt))
               (action (or avy-action #'avy-action-region-copy)))
          (funcall action begpnt endpnt))))))


;;; Embark
;;;###autoload
(defun an-embark-select-vertico-previous ()
  "Performs `embark-select' and `vertico-previous' in sequence, immediately
moving to the next candidate after selecting."
  (interactive)
  (embark-select)
  (vertico-previous))

;;;###autoload
(defun an-embark-select-vertico-next ()
  "Performs `embark-select' and `vertico-next' in sequence, immediately
moving to the next candidate after selecting."
  (interactive)
  (embark-select)
  (vertico-next))

(provide 'local-pkgs)

;;; local-pkgs.el ends here
