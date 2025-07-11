;; -*- lexical-binding: t -*-
;; loc-pkgs.el

(defun avy-action-embark-act (pt)
  "Goes to position selected with Avy and
executes `embark-act'."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(defun avy-action-embark-dwim (pt)
  "Goes to position selected with Avy and
executes `embark-dwim'."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-dwim))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(defun an-embark-select-vertico-previous ()
  "Performs `embark-select' and `vertico-previous' in sequence, immediately
moving to the next candidate after selecting."
  (interactive)
  (embark-select)
  (vertico-previous))

(defun an-embark-select-vertico-next ()
  "Performs `embark-select' and `vertico-next' in sequence, immediately
moving to the next candidate after selecting."
  (interactive)
  (embark-select)
  (vertico-next))

(defvar-keymap an-embark-vertico-repeat-map
  :doc "Keymap (repeatable) for Embark + Vertico."
  :repeat (:hints ((an-embark-select-vertico-previous . "p: Select current and move to previous candidate")
                   (an-embark-select-vertico-next . "n: Select current and move to next candidate")
                   (embark-select . "s: Select current candidate")
                   (vertico-previous . "P: Move to previous candidate")
                   (vertico-next . "N: Move to next candidate")
                   (vertico-previous-group . "C-p: Move to previous group")
                   (vertico-next-group . "C-n: Move to next group")))
  "p" #'an-embark-select-vertico-previous
  "n" #'an-embark-select-vertico-next
  "s" #'embark-select
  "P" #'vertico-previous
  "N" #'vertico-next
  "C-p" #'vertico-previous-group
  "C-n" #'vertico-next-group)

(provide 'loc-pkgs)

;;; loc-pkgs.el ends here
