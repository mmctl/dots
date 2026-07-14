;; -*- lexical-binding: t -*-
;; local-nerd-icons.el

(require 'nerd-icons)


(defun an-after-advice-local-setup-gui-nerd-icons-font (frame &rest _)
  "Configure the Nerd Icons font mapping for FRAME.

Meant as after advice for `local-setup-gui-frame', so FRAME
is assumed to be a GUI frame."
   (message "frame: %s" frame)
  (nerd-icons-set-font nil frame))


(provide 'local-nerd-icons)

;;; local-nerd-icons ends here
