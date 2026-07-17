;; -*- lexical-binding: t -*-
;; local-nerd-icons.el

(require 'nerd-icons)


(defun a-nerd-icons-set-font-frame (frame)
  "Configure the Nerd Icons font mapping for frame FRAME.

Meant as hook for `after-make-frame-functions'."
  (with-selected-frame frame
    (when (display-graphic-p frame)
      (nerd-icons-set-font nil frame))))


(provide 'local-nerd-icons)

;;; local-nerd-icons.el ends here
