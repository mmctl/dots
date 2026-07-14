;; -*- lexical-binding: t -*-
;; local-ediff.el

(require 'ediff)


;; Save/restore window configuration when starting/quitting ediff
(defvar an-ediff-preceding-window-configuration
  "Window configuration before starting ediff.")

;;;###autoload
(defun an-ediff-store-window-configuration ()
  "Stores window configuration in `an-ediff-preceding-window-configuration'"
  (setq an-ediff-preceding-window-configuration (current-window-configuration)))

;;;###autoload
(defun an-ediff-restore-window-configuration ()
  "Restores window configuration stored in
`an-ediff-preceding-window-configuration', if any, and resets it."
  (when an-ediff-preceding-window-configuration
    (set-window-configuration an-ediff-preceding-window-configuration)
    (setq an-ediff-preceding-window-configuration nil)))


(provide 'local-ediff)

;;; local-ediff.el ends here
