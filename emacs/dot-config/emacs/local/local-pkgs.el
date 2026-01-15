;; -*- lexical-binding: t -*-
;; local-pkgs.el

(require 'avy)
(require 'ace-window)
(require 'embark)
(require 'vertico)
(require 'org)
(require 'org-agenda)

;;; Avy
;; Actions (additional)
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

;; Embark with Ace Window prefix
(defun an-embark-ace-window-action (fun)
  "Select and switch to window with `ace-window', always dispatching,
before calling FUN interactively."
  (interactive)
  (with-demoted-errors "%s"
    (let* ((aw-dispatch-always t))
      (aw-switch-to-window (aw-select nil))
      (call-interactively fun))))

;;;###autoload
(defun an-embark-ace-window-find-file ()
  "Select and switch to window with `ace-window', always dispatching,
then calling `find-file' interactively."
  (interactive)
  (an-embark-ace-window-action #'find-file))

;;;###autoload
(defun an-embark-ace-window-pop-to-buffer ()
  "Select and switch to window with `ace-window', always dispatching,
then calling `pop-to-buffer-same-window' interactively."
  (interactive)
  (an-embark-ace-window-action #'pop-to-buffer-same-window))

;;;###autoload
(defun an-embark-ace-window-bookmark-jump ()
  "Select and switch to window with `ace-window', always dispatching,
then calling `bookmark-jump' interactively."
  (interactive)
  (an-embark-ace-window-action #'bookmark-jump))

;; (cl-defun an-embark--call-prefix-action (&rest rest &key run type &allow-other-keys)
;;   "Looks up command in `a-window-prefix-map' corresponding to the
;; key sequence this command was called with, and executes that (prefix)
;; command before running the Embark's current default command.

;; Meant as hook around dummy command, to be put in
;; `embark-around-action-hooks', which see; this command should then be put
;; in an Embark keymap to allow for executing default commands with a
;; prefix."
;;   (message "cmdkeysvector: %s; interpretation: %s" (this-command-keys-vector) (key-description (this-command-keys-vector)))
;;   (when-let* ((cmd (keymap-lookup
;;                     a-window-prefix-map
;;                     (key-description ""))(this-command-keys-vector)))))
;;     (funcall cmd))
;;   (funcall run :action (embark--default-action type) :type type rest))
; ;;;###autoload
(defun an-embark-choose-window-default-action ()
  "Choose window according to prefix before executing default action.

Dummy command (no-op) for use with `an-embark--call-prefix-action',
which see."
  (interactive))

;;;###autoload
(defmacro an-around-advice-with-minibuffer-keymap (keymap)
  "Expands to a lambda (taking a function and arguments) usable as around
advice that applies the provided function to its arguments inside a
minibuffer for which the local keymap is composed with KEYMAP."
  `(lambda (fun &rest args)
     (minibuffer-with-setup-hook
         (lambda ()
           (use-local-map
            (make-composed-keymap ,keymap (current-local-map))))
       (apply fun args))))

;;;###autoload
(defun an-embark-act-with-completing-read (&optional arg)
  "Calls `embark-act' with its completing read prompter
and minimal indicators."
  (interactive "P")
  (let* ((embark-prompter 'embark-completing-read-prompter)
         (embark-indicators '(embark-minimal-indicator)))
    (embark-act arg)))

;;; Org
;;;###autoload
(defun an-org-todo-manipulate-time (&optional arg)
  "As `org-todo'/`org-agenda-todo', but with the
date/time set to that entered by the user through `org-read-date'."
  (interactive "P")
  (cl-letf* ((org-read-date-prefer-future nil)
             (datetime (org-read-date t t nil "Manipulate --"))
             ((symbol-function 'current-time) #'(lambda () datetime))
             ((symbol-function 'org-current-effective-time) #'(lambda () datetime))
             ((symbol-function 'org-today) #'(lambda () (time-to-days datetime))))
    (if (eq major-mode 'org-agenda-mode)
        (org-agenda-todo arg)
      (org-todo arg))))

(provide 'local-pkgs)

;;; local-pkgs.el ends here
