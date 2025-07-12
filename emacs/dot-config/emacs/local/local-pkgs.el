;; -*- lexical-binding: t -*-
;; local-pkgs.el

;;;###autoload
(defun an-avy-action-embark-select (pt)
  "Goes to position selected with Avy and
executes `embark-select'."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-select))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

;;;###autoload
(defun an-avy-action-embark-act (pt)
  "Goes to position selected with Avy and
executes `embark-act'."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

;;;###autoload
(defun an-avy-action-embark-dwim (pt)
  "Goes to position selected with Avy and
executes `embark-dwim'."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-dwim))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

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
