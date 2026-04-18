;; -*- lexical-binding: t -*-
;; local-auctex.el

(require 'tex)


;; Swap to \( and \) instead of $ and $ (e.g., when using LaTeX instead of TeX)
;;;###autoload
(defun a-setup-latex-mode-electric-math ()
  (setq-local TeX-electric-math '("\\(" . "\\)")))


(provide 'local-auctex)

;;; local-auctex.el ends here
