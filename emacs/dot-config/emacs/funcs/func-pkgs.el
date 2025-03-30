;; -*- lexical-binding: t -*-
;; func-pkgs
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


(provide 'func-pkgs)

;;; func-pkgs ends here
