;; -*- lexical-binding: t -*-
;; func-pkgs.el
;; Dependencies
(require 'avy)
(require 'embark)

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
        (syntax (syntax-ppss))
        (bol (line-beginning-position)))
    ;; Compute desired level of indentation
    (save-excursion
      (back-to-indentation) ; Base decision on beginning of line
      ;; If we are in an opened or enclosed expression...
      ;; (e.g., between { and } or ( and ), or after { or ( with no closing counterpart)
      (if (nth 1 (syntax-ppss))
          (let ((brexp (looking-at "}" t))
                (btexp (looking-at "]" t))
                (prexp (looking-at ")" t)))
            (progn
              (goto-char (nth 1 (syntax-ppss)))
              (cond
               ;; If first char on our line is }...
               (brexp
                ;; Then, if a { opened current expression (i.e., we are closing with })
                (if (looking-at "{" t)
                    ;; Then align to indentation opener
                    (setq indent-level (current-indentation))
                  ;; Else align to indentation of opener + tab
                  (setq indent-level (+ (current-indentation) tab-width))))
               ;; Else, if first char is ]/) and current expression is opened by [/(...
               ((or (and btexp (looking-at "[[]" t)) (and prexp (looking-at "(" t)))
                ;; Then, match indentation to opener
                (setq indent-level (current-column)))
               ;; Else, align to indentation of opener + tab
               (t
                (setq indent-level (+ (current-column) 1 tab-width))))))
        ;; Else, default to aligning with previous non-blank line
        (progn
          (forward-line -1)
          (while (and (not (bobp)) (looking-at "^[[:space:]]*$" t))
            (forward-line -1))
          (back-to-indentation)
          (setq indent-level (current-indentation)))))
    ;; Indent accordingly
    (indent-line-to indent-level)
    ;; Adjust point if it's inside indentation
    (when (< (current-column) (current-indentation))
      (move-to-column indent-level))))

(provide 'func-pkgs)

;;; func-pkgs.el ends here
