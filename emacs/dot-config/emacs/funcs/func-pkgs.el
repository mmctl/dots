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
;;; Indentation (orig)
;; (defun easycrypt-indent-level ()
;;   "Returns desired indentation level of EasyCrypt code."
;;   (let ((indent-level 0))
;;     ;; Compute desired level of indentation
;;     (save-excursion
;;       (back-to-indentation) ; Base decision on beginning of line
;;       (let* ((synps (syntax-ppss))
;;              (exprop (nth 1 synps))
;;              (instr (nth 3 synps))
;;              (incom (nth 4 synps))
;;              (csop (nth 8 synps)))
;;         (cond
;;          ;; If we are in a comment or string...
;;          ((or incom instr)
;;           (let ((comcl (looking-at-p "[\\^\\*]?\\*)"))
;;                 (strcl (looking-at-p "\"")))
;;             (goto-char csop)
;;             ;; Then, if our line closes the comment or string...
;;             (if (or (and incom comcl) (and instr strcl))
;;                 ;; Then, align closer with the opener
;;                 (setq indent-level (current-column))
;;               ;; Else, align with the opener + tab
;;               (setq indent-level (+ (current-column) tab-width)))))
;;          ;; Else, if we are in an opened or enclosed expression...
;;          ;; (E.g., between { and }, or ( and ), or after { or ( with no closing counterpart)
;;          (exprop
;;           (let ((brcl (looking-at "}" t))
;;                 (btcl (looking-at "]" t))
;;                 (prcl (looking-at ")" t)))
;;             (goto-char exprop)
;;             ;; If opener is {...
;;             (if (looking-at-p "{")
;;                 ;; Then, if first char on our line is }...
;;                 (if brcl
;;                     ;; Then, align to indentation of opener
;;                     (setq indent-level (current-indentation))
;;                   ;; Else, align to indentation of opener + tab
;;                   (setq indent-level (+ (current-indentation) tab-width)))
;;               ;; Else, if first char on our line is ] or )
;;               ;; and current expression is opened by [ or (...
;;               (if (or (and btcl (looking-at-p "[[]"))
;;                       (and prcl (looking-at-p "(")))
;;                   ;; Then, align to indentation to opener
;;                   (setq indent-level (current-column))
;;                 ;; Else, align to indentation of opener + tab
;;                 (setq indent-level (+ (current-column) tab-width))))))
;;          ;; Else,...
;;          (t
;;           (progn
;;             ;; Find previous non-blank line
;;             (forward-line -1)
;;             (while (and (not (bobp)) (looking-at-p "^[[:space:]]*$"))
;;               (forward-line -1))
;;             (setq prev-line (buffer-substring-no-properties
;;                              (line-beginning-position)
;;                              (line-end-position)))
;;             (back-to-indentation)
;;             ;; If previous non-blank line is an unfinished non-proof/non-proc spec
;;             ;; (E.g., starts with `lemma` but does not end with a `.`)
;;             (if (and (seq-some (lambda (kw)
;;                                  (string-match-p (concat "^[[:space:]]*" (regexp-quote kw) "\\b") prev-line))
;;                                cst-easycrypt-start-keywords)
;;                      (not (string-match-p "\\.[[:space:]]*$" prev-line)))
;;                 ;; Then, align with previous non-blank line + tab
;;                 (setq indent-level (+ (current-indentation) tab-width))
;;               ;; Else, align with previous non-blank line (default)
;;               (setq indent-level (current-indentation)))))))
;;       indent-level)))

;;; Indentation
(defun easycrypt-indent-level-fallback ()
  (save-excursion
    ;; Find previous non-blank line
    (forward-line -1)
    (while (and (not (bobp)) (looking-at-p "^[[:blank:]]*$"))
      (forward-line -1))
    (setq prev-line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position)))
    (back-to-indentation)
    ;; If previous non-blank line is an unfinished non-proof/non-proc spec
    ;; (E.g., starts with `lemma` but does not end with a `.`)
    (if (and (seq-some (lambda (kw)
                         (string-match-p (concat "^[[:blank:]]*" (regexp-quote kw) "\\b") prev-line))
                       cst-easycrypt-start-keywords)
             (not (string-match-p "\\.[[:blank:]]*$" prev-line)))
        ;; Then, align with previous non-blank line + tab
        (+ (current-indentation) tab-width)
      ;; Else, align with previous non-blank line (default)
      (current-indentation))))

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
         ;; Else, if we are looking at a proof starter (i.e., "proof")
         ((seq-some (lambda (kw) (looking-at-p (concat (regexp-quote kw) "\\b")))
                    cst-easycrypt-proof-start-keywords)
          (let ((bob nil))
            (progn
              (save-excursion
                ;; Find previous "proof spec starter" (i.e., keyword starting a
                ;; lemma such as lemma, hoare (without following [), and
                ;; equiv (without following [)
                (forward-line -1)
                (while (and (not (bobp))
                            (not (seq-some
                                  (lambda (kw)
                                    (and (looking-at-p (concat "^[[:blank:]]*" (regexp-quote kw) "\\b"))
                                         (not (looking-at-p (concat "^[[:blank:]]*" (regexp-quote kw) "\\b[[:space:]]*\\[")))))
                                  cst-easycrypt-proof-spec-keywords)))
                  (forward-line -1))
                ;; If we didn't find such a line (i.e., we are at the beginning of a buffer)...
                (if (bobp)
                    ;; Then, record this fact
                    (setq bob t)
                  ;; Else, align to indentation of proof spec starter
                  (setq indent-level (current-indentation))))
              ;; If we didn't find a proof spec starter
              (when bob
                ;; Indent as per the fallback
                (setq indent-level (easycrypt-indent-level-fallback))))))
         ;; Else, if we are looking at a proof ender (i.e., "qed")
         ((seq-some (lambda (kw) (looking-at-p (concat (regexp-quote kw) "\\b")))
                    cst-easycrypt-proof-end-keywords)
          (let ((bob nil))
            (progn
              (save-excursion
                ;; Find line that started the proof (i.e., one that starts with "proof")
                (forward-line -1)
                (while (and (not (bobp))
                            (not (seq-some (lambda (kw) (looking-at-p (concat "^[[:blank:]]*" (regexp-quote kw) "\\b")))
                                  cst-easycrypt-proof-start-keywords)))
                  (forward-line -1))
                ;; If we didn't find such a line (i.e., we are at the beginning of a buffer)...
                (if (bobp)
                    ;; Then, record this fact
                    (setq bob t)
                  ;; Else, align to indentation of proof starter
                  (setq indent-level (current-indentation))))
              ;; If we didn't find a proof starter
              (when bob
                ;; Indent as per the fallback
                (setq indent-level (easycrypt-indent-level-fallback))))))
         ;; Else,...
         (t
          ;; Indent as per the fallback
          (setq indent-level (easycrypt-indent-level-fallback))))))
      indent-level))

(defun easycrypt-indent-line ()
  "Indent line of EasyCrypt code."
  (interactive)
  ;; Indent accordingly
  (let ((indent-level (easycrypt-indent-level)))
    ;; `indent-line-to`would move point to new indentation, and
    ;; we prevent this by `save-excursion` so point position remains consistent
    ;; (making templates more consistent as well)
    (save-excursion
      (indent-line-to indent-level))
    ;; Still adjust point if it's inside indentation
    (when (< (current-column) (current-indentation))
      (move-to-column indent-level))))

(defun easycrypt-indent-on-insertion-closer ()
  "Indent when last input was one of }, ), ], \" and it is the
first character on current line, or if the last input was . and
the current line starts/ends a proof. However, only
allow de-indents (to prevent automatically indenting
code that has been manually de-indented; this is a hack
and a limitation of the localized ad-hoc computation
of the indent level).
Meant for `post-self-insert-hook`."
  (when (or (and (memq last-command-event '(?\} ?\) ?\] ?\"))
                 (let ((line-before (buffer-substring-no-properties (line-beginning-position) (- (point) 1))))
                   (string-match-p "^[[:blank:]]*$" line-before)))
            (and (eq last-command-event ?\.)
                 (save-excursion
                   (back-to-indentation)
                   (seq-some (lambda (kw) (looking-at-p (concat (regexp-quote kw) "\\b")))
                             cst-easycrypt-proof-delimit-keywords))))
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
      (user-error "Unknown/Unsupported command: `%s`." command)))

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
        (user-error "No valid region or reasonable thing at point found for command `%s`. Try highlighting the thing if automatic detection doesn't work."
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
        (user-error "No reasonable thing at mouse found for command `%s`." command)))))

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
