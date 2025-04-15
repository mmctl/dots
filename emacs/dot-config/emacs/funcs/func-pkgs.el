;; -*- lexical-binding: t -*-
;; func-pkgs.el
;; Dependencies
(require 'avy)
(require 'embark)
(require 'custom-consts)

;; Functions
;;; Avy + Embark
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

;;; EasyCrypt
(defun easycrypt-indent-level ()
  "Returns desired indentation level of EasyCrypt code."
  (let ((indent-level 0))
    ;; Compute desired level of indentation
    (save-excursion
      (back-to-indentation) ; Base decision on beginning of line
      (let ((incom (nth 3 (syntax-ppss)))
            (instr (nth 3 (syntax-ppss)))
            (csop (nth 8 (syntax-ppss)))
            (exprop (nth 1 (syntax-ppss))))
        (cond
         ;; If we are in a comment...
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
      ;; If 0 < indent-diff, i.e., we de-indent
      (when (< 0 indent-diff)
        ;; Go to the computed indent level
        (indent-line-to indent-level)
        ;; Keep point in same relative position
        ;; (`indent-line-to` moves it to end of indentation)
        (move-to-column (- orig-col indent-diff))))))

(provide 'func-pkgs)

;;; func-pkgs.el ends here
