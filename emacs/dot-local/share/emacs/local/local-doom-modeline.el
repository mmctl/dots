;; -*- lexical-binding: t -*-
;; local-doom-modeline.el

(require 'doom-modeline)


(defun a-doom-modeline-init-first-graphical-frame (frame)
  "Initialize Doom Modeline using first (non-dummy) graphical FRAME.

Meant as hook for `after-make-frame-functions'."
  (when (and (display-graphic-p frame)
             (not (frame-parameter frame 'server-dummy-buffer)))
    (with-selected-frame frame
      (doom-modeline-mode 1))
    (remove-hook 'after-make-frame-functions #'a-doom-modeline-init-first-graphical-frame)))


(provide 'local-doom-modeline)

;;; local-doom-modeline.el ends here
