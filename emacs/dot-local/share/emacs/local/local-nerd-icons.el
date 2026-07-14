;; -*- lexical-binding: t -*-
;; local-nerd-icons.el

(require 'nerd-icons)


(defun an-after-advice-local-setup-gui-nerd-icons-font (frame &rest _)
  "Configure the Nerd Icons font mapping for FRAME.

Meant as after advice for `local-setup-gui-frame' (and also only makes
sense for GUI frames)."
  (when (display-graphic-p frame)
    (nerd-icons-set-font nil frame)))


(provide 'local-nerd-icons)

;;; local-nerd-icons ends here
