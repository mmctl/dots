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
(defun easycrypt-indent-line ()
  "Indent line of EasyCrypt code."
  (interactive)
  (let ((indent-level 0)
        (bol (line-beginning-position)))
    ;; Compute desired level of indentation
    (save-excursion
      (back-to-indentation) ; Base decision on beginning of line
      ;; If we are in an opened or enclosed expression...
      ;; (e.g., between { and } or ( and ), or after { or ( with no closing counterpart)
      (if (nth 1 (syntax-ppss))
          (let ((brcl (looking-at "}" t))
                (btcl (looking-at "]" t))
                (prcl (looking-at ")" t)))
            (progn
              (goto-char (nth 1 (syntax-ppss)))
              ;; If opener is {...
              (if (looking-at "{" t)
                  ;; Then, if first char on our line is }...
                  (if brcl
                      ;; Then, align to indentation of opener
                      (setq indent-level (current-indentation))
                    ;; Else, align to indentation of opener + tab
                    (setq indent-level (+ (current-indentation) tab-width)))
                ;; Else, if first char on our line is ] or )
                ;; and current expression is opened by [ or (...
                (if (or (and btcl (looking-at "[[]" t))
                        (and prcl (looking-at "(" t)))
                    ;; Then, align to indentation to opener
                    (setq indent-level (current-column))
                  ;; Else, align to indentation of opener + tab
                  (setq indent-level (+ (current-column) tab-width))))))
        ;; Else, find previous non-blank line...
        (progn
          (forward-line -1)
          (while (and (not (bobp)) (looking-at "^[[:space:]]*$" t))
            (forward-line -1))
          (setq prev-line (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position)))
          (back-to-indentation)
          ;; If previous non-blank line is an unfinished functional spec
          ;; E.g., starts with `lemma` but does not end with a `.`
          (if (and (seq-some (lambda (kw)
                               (string-match-p (concat "^[[:space:]]*" (regexp-quote kw) "\\b") prev-line))
                             cst-easycrypt-functional-spec-start-keywords)
                   (not (string-match-p "\\.[[:space:]]*$" prev-line)))
              ;; Then, align with previous non-blank line + tab
              (setq indent-level (+ (current-indentation) tab-width))
            ;; Else, align with previous non-blank line (default)
            (setq indent-level (current-indentation))))))
    ;; Indent accordingly
    (indent-line-to indent-level)
    ;; Adjust point if it's inside indentation
    (when (< (current-column) (current-indentation))
      (move-to-column indent-level))))

(provide 'func-pkgs)

;;; func-pkgs.el ends here
