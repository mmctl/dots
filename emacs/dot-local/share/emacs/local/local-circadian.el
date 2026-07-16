;; -*- lexical-binding: t -*-
;; local-circadian.el

(require 'circadian)


(defun a-circadian-setup-first-client-frame-hook ()
  "Set up circadian on the first graphical client frame FRAME.

Meant as hook for `server-after-make-frame-hook'."
  (let ((frame (selected-frame)))
    (when (and (display-graphic-p frame)
               (frame-parameter frame 'client))
      (circadian-setup)
      (remove-hook 'server-after-make-frame-hook
                   #'a-circadian-setup-first-client-frame-hook))))


(provide 'local-circadian)

;;; local-circadian.el ends here
