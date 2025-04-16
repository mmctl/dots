;; -*- lexical-binding: t -*-
;; func-pkgs.el
;; Dependencies
(require 'avy)
(require 'embark)
(require 'proof-general)
(require 'custom-consts)

;; Avy + Embark
(defun avy-action-embark-act (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(defun avy-action-embark-dwim (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-dwim))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

;; EasyCrypt
;;; Indentation
(defun easycrypt-indent-level ()
  "Returns desired indentation level of EasyCrypt code."
  (let ((indent-level 0))
    ;; Compute desired level of indentation
    (save-excursion
      (back-to-indentation) ; Base decision on beginning of line
      (let* ((synps (syntax-ppss))
             (exprop (nth 1 synps))
             (instr (nth 3 synps))
             (incom (nth 4 synps))
             (csop (nth 8 synps)))
        (cond
         ;; If we are in a comment or string...
         ((or incom instr)
          (let ((comcl (looking-at-p "[\\^\\*]?\\*)"))
                (strcl (looking-at-p "\"")))
            (goto-char csop)
            ;; Then, if our line closes the comment or string...
            (if (or (and incom comcl) (and instr strcl))
                ;; Then, align closer with the opener
                (setq indent-level (current-column))
              ;; Else, align with the opener + tab
              (setq indent-level (+ (current-column) tab-width)))))
         ;; Else, if we are in an opened or enclosed expression...
         ;; (E.g., between { and }, or ( and ), or after { or ( with no closing counterpart)
         (exprop
          (let ((brcl (looking-at "}" t))
                (btcl (looking-at "]" t))
                (prcl (looking-at ")" t)))
            (goto-char exprop)
            ;; If opener is {...
            (if (looking-at-p "{")
                ;; Then, if first char on our line is }...
                (if brcl
                    ;; Then, align to indentation of opener
                    (setq indent-level (current-indentation))
                  ;; Else, align to indentation of opener + tab
                  (setq indent-level (+ (current-indentation) tab-width)))
              ;; Else, if first char on our line is ] or )
              ;; and current expression is opened by [ or (...
              (if (or (and btcl (looking-at-p "[[]"))
                      (and prcl (looking-at-p "(")))
                  ;; Then, align to indentation to opener
                  (setq indent-level (current-column))
                ;; Else, align to indentation of opener + tab
                (setq indent-level (+ (current-column) tab-width))))))
         ;; Else,...
         (t
          (progn
            ;; Find previous non-blank line
            (forward-line -1)
            (while (and (not (bobp)) (looking-at-p "^[[:space:]]*$"))
              (forward-line -1))
            (setq prev-line (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position)))
            (back-to-indentation)
            ;; If previous non-blank line is an unfinished non-proof/non-proc spec
            ;; (E.g., starts with `lemma` but does not end with a `.`)
            (if (and (seq-some (lambda (kw)
                                 (string-match-p (concat "^[[:space:]]*" (regexp-quote kw) "\\b") prev-line))
                               cst-easycrypt-start-keywords)
                     (not (string-match-p "\\.[[:space:]]*$" prev-line)))
                ;; Then, align with previous non-blank line + tab
                (setq indent-level (+ (current-indentation) tab-width))
              ;; Else, align with previous non-blank line (default)
              (setq indent-level (current-indentation)))))))
      indent-level)))

(defun easycrypt-indent-line ()
  "Indent line of EasyCrypt code."
  (interactive)
  ;; Indent accordingly
  (indent-line-to (easycrypt-indent-level))
  ;; Adjust point if it's inside indentation
  (when (< (current-column) (current-indentation))
    (move-to-column indent-level)))

(defun easycrypt-indent-on-insertion-closer ()
  "Indent when last input was }, ), ], or \", but only
allow de-indents (to prevent automatically indenting
code that has been manually de-indented; this is a hack
and a limitation of the localized ad-hoc computation
of the indent level).
Meant for `post-self-insert-hook`."
  (when (memq last-command-event '(?\} ?\) ?\] ?\"))
    (let* ((orig-col (current-column))
           (indent-level (easycrypt-indent-level))
           (indent-diff (- (current-indentation) indent-level)))
      ;; If 0 < indent-diff, i.e., we are de-indenting
      (when (< 0 indent-diff)
        ;; Go to the computed indent level
        (indent-line-to indent-level)
        ;; Keep point in same relative position
        ;; (`indent-line-to` moves it to end of indentation)
        (move-to-column (- orig-col indent-diff))))))

;;; Extra functionality
(defun an-easycrypt-is-supported-shell-command (command)
  "Checks if the provided `command` is a valid/supported EasyCrypt shell command
(in the sense that a command below is implemented for it).
Prints a message informing the user if that is not the case."
  (or (member command '("print" "search" "locate"))
      (progn ((message "Unknown/Unsupported command: `%s`." command) nil))))

(defun an-easycrypt-shell-command (command &rest args)
  "Combines `command` and `args` into a command for the EasyCrypt shell, and
directly calls the shell with it."
  (if args
      (proof-shell-invisible-command (concat command " " (string-join args " ")))
    (proof-shell-invisible-command command)))

(defun an-easycrypt-command-at-point (command)
  "Takes the active region or tries to find a (reasonable) thing at point,
and uses the result as an argument to the `command` command of EasyCrypt.
If nothing (reasonable) is found, or the provided `command` is not valid,
prints a message informing the user."
  (when (an-easycrypt-is-supported-shell-command command)
    (let ((arg (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (or (thing-at-point 'sexp t)
                     (thing-at-point 'word t)
                     (thing-at-point 'symbol t)))))
      (if arg
          (an-easycrypt-shell-command command arg)
        (message (concat "No valid region or reasonable thing at point found for command `%s`."
                         " Try highlighting the thing if automatic detection doesn't work.")
                 command)))))

(defun an-easycrypt-print-at-point ()
  "Takes the active region or tries to find a (reasonable) thing at point,
and uses the result as an argument to the `print` command of EasyCrypt."
  (interactive)
  (an-easycrypt-command-at-point "print"))

(defun an-easycrypt-locate-at-point ()
  "Like `an-easycrypt-print-at-point`, but issues the locate command."
  (interactive)
  (an-easycrypt-command-at-point "locate"))

(defun an-easycrypt-search-at-point ()
  "Like `an-easycrypt-print-at-point`, but issues the search command."
  (interactive)
  (an-easycrypt-command-at-point "search"))

(defun an-easycrypt-command-at-mouse (event command)
  "Like `an-easycrypt-command-at-point`, but tries to find thing at mouse
instead of point. Also doesn't consider regions."
  (when (an-easycrypt-is-supported-shell-command command)
    (let ((arg (or (thing-at-mouse event 'sexp t)
                   (thing-at-mouse event 'word t)
                   (thing-at-mouse event 'symbol t))))
      (if arg
          (an-easycrypt-shell-command command arg)
        (message "No reasonable thing at mouse found for command `%s`." command)))))

(defun an-easycrypt-print-at-mouse (event)
  "Tries to find a (reasonable) thing at mouse, and uses the result
as an argument to the `print` command of EasyCrypt."
  (interactive "e")
  (an-easycrypt-command-at-mouse event "print"))

(defun an-easycrypt-locate-at-mouse (event)
  "Like `an-easycrypt-print-at-mouse`, but issues the locate command."
  (interactive "e")
  (an-easycrypt-command-at-mouse event "locate"))

(defun an-easycrypt-search-at-mouse (event)
  "Like `an-easycrypt-print-at-mouse`, but issues the search command."
  (interactive "e")
  (an-easycrypt-command-at-mouse event "search"))

(provide 'func-pkgs)

;;; func-pkgs.el ends here
