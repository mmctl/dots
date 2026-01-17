;; -*- lexical-binding: t -*-
;; local-utils.el

;;; Predicates
(defun exists-window-with-derived-mode (mode &optional frame)
  "Checks whether there exists a window in FRAME showing a buffer
in a mode derived from MODE (including MODE itself). If FRAME is nil,
defaults to checking selected frame."
  (seq-some #'(lambda (win)
                (with-current-buffer (window-buffer win)
                   (derived-mode-p mode)))
            (window-list frame)))


;;; Displaying
;; Fitting
(defconst MAX_WIDTH_DIV_WINDOW_LR_SIDE 2.5
  "Divisor used to compute maximum width of side window
on the left or right.")
(defconst MIN_WIDTH_DIV_WINDOW_LR_SIDE 7.5
  "Divisor used to compute minimum width of side window
on the left or right.")
(defconst MAX_HEIGHT_DIV_WINDOW_BT_SIDE 3
  "Divisor used to compute maximum height of side window
on the bottom or top.")
(defconst MIN_HEIGHT_DIV_WINDOW_BT_SIDE 5
  "Divisor used to compute minimum height of side window
on the bottom or top.")

(defun fit-lr-side-window-to-buffer (&optional window)
  "Fits right-side window to buffer with a maximum (resp. minimum) width
determined by dividing the frame width by `MAX_WIDTH_DIV_WINDOW_LR_SIDE'
(resp. `MIN_WIDTH_DIV_WINDOW_LR_SIDE'), which see."
  (fit-window-to-buffer window nil nil
                        (floor (frame-width) MAX_WIDTH_DIV_WINDOW_LR_SIDE)
                        (floor (frame-width) MIN_WIDTH_DIV_WINDOW_LR_SIDE)))

(defun fit-bt-side-window-to-buffer (&optional window)
  "Fits bottom-side window to buffer with a maximum (resp. minimum) height
determined by dividing the frame height by `MAX_HEIGHT_DIV_WINDOW_BT_SIDE'
(resp. `MIN_HEIGHT_DIV_WINDOW_BT_SIDE'), which see."
  (fit-window-to-buffer window
                        (floor (frame-height) MAX_HEIGHT_DIV_WINDOW_BT_SIDE)
                        (floor (frame-height) MIN_HEIGHT_DIV_WINDOW_BT_SIDE)))

;; Selection (Window/Buffer)
(defun switch-to-buffer-display-override-same-window ()
  "Calls `switch-to-buffer', but tries to first reuse same window when
obeying display actions (see `switch-to-buffer-obey-display-actions')."
  (interactive)
  (let ((display-buffer-overriding-action '((display-buffer-same-window))))
    (call-interactively #'switch-to-buffer)))


;;; Help
(defun a-which-key-repeated-prefix-help-command ()
  "Prefix help command that makes the current prefix map
active (for repeated actions) until it is explicitly exited via
`<keyboard-quit>'. Uses `which-key'.

Sets `which-key-persistent-popup' (via `setq') temporarily to `t',
but undoes this when the prefix map is exited, in order to
have `which-key''s pop-up stick while the prefix map is active."
  (interactive)
  (when-let* ((keys (this-command-keys-vector))
              (prefix (seq-take keys (1- (length keys))))
              (orig-keymap (key-binding prefix t))
              (keymap (copy-keymap orig-keymap)))
    (let* ((orig-persistent which-key-persistent-popup)
           (exit-func (set-transient-map
                       keymap
                       t
                       (lambda ()
                         (setq which-key-persistent-popup orig-persistent)
                         (which-key-abort)))))
      (keymap-set keymap "<remap> <keyboard-quit>"
                  (lambda () (interactive) (funcall exit-func)))
      (setq which-key-persistent-popup t)
      (which-key--create-buffer-and-show nil keymap))))

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
    (move-end-of-line (prefix-numeric-value arg))
    (when (re-search-backward "[^[:blank:]]" (line-beginning-position) t)
      (forward-char)
      (when (and (null arg) show-trailing-whitespace (= (point) orig-point))
        (move-end-of-line nil)))))

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
                 (if rep "entire line (content + whitespace)" "line")
                 (if arg (format " at %s" arg) ""))))))


;;; Yanking
(defun yank-line (&optional arg)
  "Yanks (in place) line at point.
With ARG, moves |ARG| lines forward (ARG > 0) or backward (ARG < 0),
then copies that line. Does not move point."
  (interactive "P")
  (kill-ring-save-line arg)
  (yank))


;;; Killing
(defun kill-whole-thing (thing &optional arg)
  "Kills whole THING at point or, if no THING at point, up to and including next
whole THING. If there is also no next THING, does nothing. With ARG, kills from
beginning/end of THING at point to end/beginning of |ARG| THINGs forward (ARG >
0)/backward (ARG < 0). Does not move point (beyond the displacement that may
happen from killing THINGs). Does nothing when ARG = 0."
  (save-excursion
    (let* ((arg (or arg 1))
           (bnds (bounds-of-thing-at-point thing)))
      (when (and bnds (not (zerop arg)))
        (goto-char (if (< 0 arg) (car bnds) (cdr bnds))))
      (let ((fixpnt (point)))
        (forward-thing thing arg)
        (kill-region fixpnt (point))))))

(defun kill-whole-word (&optional arg)
  "Executes `kill-whole-thing', which see,
using `word' for thing, and directly passing ARG."
  (interactive "p")
  (kill-whole-thing 'word arg))

(defun kill-whole-symbol (&optional arg)
  "Executes `kill-whole-thing', which see,
using `symbol' for thing, and directly passing ARG."
  (interactive "p")
  (kill-whole-thing 'symbol arg))

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


;;; Searching and replacing
(defun query-replace-from-default-thing-at-point ()
  "Returns text under active region or, if no region is active, thing at
point, where thing is one of the things defined in
`isearch-forward-thing-at-point', which see, tried in order. If this
yields nothing, returns `nil'."
  (let ((bnds (if (use-region-p)
                   (prog1
                       (cons (region-beginning) (region-end))
                     (deactivate-mark))
                 (seq-some (lambda (thing)
                             (bounds-of-thing-at-point thing))
                           isearch-forward-thing-at-point))))
    (when bnds
      (goto-char (car bnds)) (buffer-substring-no-properties (car bnds) (cdr bnds)))))

(defun query-replace-thing-at-point ()
"Calls `query-replace' with `query-replace-read-from-default'
bound to `query-replace-from-default-thing-at-point', which see."
  (interactive)
  (let* ((default (query-replace-from-default-thing-at-point))
         (query-replace-read-from-default (lambda () default)))
    (call-interactively #'query-replace)))

(defun query-replace-regexp-thing-at-point ()
  "Calls `query-replace-regexp' with `query-replace-read-from-default'
bound to `query-replace-from-default-thing-at-point', which see."
  (interactive)
  (let* ((default (query-replace-from-default-thing-at-point))
         (query-replace-read-from-default (lambda () default)))
    (call-interactively #'query-replace-regexp)))

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
