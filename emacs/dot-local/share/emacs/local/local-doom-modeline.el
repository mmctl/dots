;; -*- lexical-binding: t -*-
;; local-doom-modeline.el

(require 'doom-modeline)


(defun a-doom-modeline-init-first-graphical-frame (frame)
  "Initialize Doom Modeline using first (non-dummy) graphical FRAME.

Meant as hook for `after-make-frame-functions'."
  (when (and (display-graphic-p frame)
             (not (frame-parameter frame 'server-dummy-buffer))
             (not (frame-parameter frame 'a-primer)))
    (with-selected-frame frame
      (doom-modeline-mode 1))
    (remove-hook 'after-make-frame-functions #'a-doom-modeline-init-first-graphical-frame)))

(defun a-doom-modeline-init-first-graphical-client-frame ()
  "Initialize Doom Modeline using first (non-dummy) client graphical frame.

Meant as hook for `server-after-make-frame-hook'."
  (let ((frame (selected-frame)))
    (when (and (display-graphic-p frame)
               (not (frame-parameter frame 'server-dummy-buffer))
               (frame-parameter frame 'client))
      (doom-modeline-mode 1)
      (remove-hook 'server-after-make-frame-hook #'a-doom-modeline-init-first-graphical-client-frame))))


(provide 'local-doom-modeline)

;;; local-doom-modeline.el ends here
