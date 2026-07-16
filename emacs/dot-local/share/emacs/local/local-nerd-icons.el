;; -*- lexical-binding: t -*-
;; local-nerd-icons.el

(require 'nerd-icons)


(defun a-nerd-icons-set-font-client-frame-hook ()
  "Configure the Nerd Icons font mapping for client frame FRAME.

Meant as hook for `server-after-make-frame-hook'."
  (let ((frame (selected-frame)))
    (when (and (display-graphic-p frame)
               (frame-parameter frame 'client))
      (nerd-icons-set-font nil frame))))


(provide 'local-nerd-icons)

;;; local-nerd-icons.el ends here
