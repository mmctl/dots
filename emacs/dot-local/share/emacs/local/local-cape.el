;; -*- lexical-binding: t; -*-
;; local-cape.el

(require 'cape)
(require 'cape-keyword)

;;; Setup functionality
;;;###autoload
(defun a-setup-cape-text-mode ()
  (add-hook 'completion-at-point-functions #'cape-dabbrev nil t)
  (add-hook 'completion-at-point-functions #'cape-dict nil t))

;;;###autoload
(defun a-setup-cape-code-mode ()
  (add-hook 'completion-at-point-functions #'cape-keyword nil t))

;;;###autoload
(defun a-setup-cape-minibuffer ()
  (add-hook 'completion-at-point-functions #'cape-history nil t)
  (add-hook 'completion-at-point-functions #'cape-file nil t))


(provide 'local-cape)
;;; local-cape.el ends here
