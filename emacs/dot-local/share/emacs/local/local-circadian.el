;; -*- lexical-binding: t -*-
;; local-circadian.el

(require 'circadian)

(defun a-circadian-setup-first-graphical-frame (frame)
  "Set up circadian on the first graphical client frame FRAME.

Meant as hook for `after-make-frame-functions'."
  (with-selected-frame frame
    (when (display-graphic-p frame)
      (circadian-setup)
      (remove-hook 'after-make-frame-functions
                   #'a-circadian-setup-first-graphical-frame))))

(defun a-circadian-setup-first-client-frame ()
  "Set up circadian on the first graphical client frame FRAME.

Meant as hook for `server-after-make-frame-hook'."
  (let ((frame (selected-frame)))
    (when (display-graphic-p frame)
      (circadian-setup)
      (remove-hook 'server-after-make-frame-hook
                   #'a-circadian-setup-first-client-frame))))


(provide 'local-circadian)

;;; local-circadian.el ends here
